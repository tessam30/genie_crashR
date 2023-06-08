# PROJECT: Genie Crash course
# PURPOSE: Munge and Analysis of Genie Dat
# AUTHOR: Tim Essam | SI
# REF ID:   7fc56074
# LICENSE: MIT
# DATE: 2023-06-08
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(glue)
    library(gt)
    library(gtExtras)


  # SI specific paths/functions
    merdata <- file.path(glamr::si_path("path_msd"))

    file_path <- return_latest(folderpath = merdata,
      pattern = "Sample")

  # REF ID for plots
    ref_id <- "7fc56074"

  # Functions


# LOAD DATA ============================================================================

  # Load Data
  df <- vroom::vroom(file_path)

  # Investigate structure
  glimpse(df)


# EXPLORE ============================================================================

  # Appears to be a site X IM data set which means that the data go all the way
  # down to the site level at which PEPFAR works.


  # Indicators
  df %>% distinct(indicator)

  # HTS_TST = Number of tests recorded in a quarter
  # TX_NEW = Number of new initations on treatment
  # TX_CURR = Number or clients on TX_CURR
  # TX_PVLS = Number of viral load tests (used to derive VLC and VLS)

  # Formulas
  # Viral Load Suppression = TX_PVLS_N / TX_PVLS_D
  # Viral Load Coverage = TX_PVLS_D / TX_CURR_Lag2


# Standardized Disaggregates ============================================================================

  # To accurately summarize the data, we need to filter it by a series of standardized disaggregates
  indic_list1 <- c("HTS_TST", "TX_CURR", "TX_NEW", "TX_PVLS")

  df %>%
    filter(indicator %in% indic_list1) %>%
    count(indicator, standardizeddisaggregate) %>%
    spread(indicator, n)

  # You'll see that there are a lot of options for standardized disaggregates
  # TX_PVLS is one of a few that have both numerator and denominator options

  # Snapshot versus Cumulative
  # There are also a few indicators that track clients at a point in time
  # and other indicators that are cumulative or track totals across time
  # To return the list of snapshot indicators you can use the custom function below
  gophr::snapshot_ind

  # When analyzing snapshot indicators you will want to focus on the latest value
  # entered in DATIM. BE CARE YOU DO NOT AGGREGATE THESE OR YOU WILL GET INCORRECT RESULTS
  # If in doubt, use the cumulative column in the MSD / Genie file. This will have the correct
  # calculation for both snapshot and cumulative indicators

  indic_list <- indic_list1[!indic_list %in% c("TX_PVLS")]

  # For example, if you want high level numbers you will likely be using the "Total Numerator" standardized disaggregate
  df %>%
    filter(standardizeddisaggregate == "Total Numerator",
           indicator %in% indic_list) %>%
    group_by(funding_agency, fiscal_year, indicator) %>%
    summarize(total = sum(cumulative, na.rm = T), .groups = "drop") %>%
    spread(indicator, total)

  # Say you would like to add in Implementing partners, you will use mech_code or mech_name
  df %>%
    filter(standardizeddisaggregate == "Total Numerator",
           indicator %in% indic_list) %>%
    group_by(funding_agency, mech_name, fiscal_year, indicator) %>%
    summarize(total = sum(cumulative, na.rm = T), .groups = "drop") %>%
    spread(indicator, total)

  # Cut data by geography
  df %>%
    filter(standardizeddisaggregate == "Total Numerator",
           indicator %in% indic_list) %>%
    group_by(funding_agency, snu1, fiscal_year, indicator) %>%
    summarize(total = sum(cumulative, na.rm = T), .groups = "drop") %>%
    spread(indicator, total)


  # For a full list of the DATIM dataelements, see here:
  # https://datim.zendesk.com/hc/en-us/articles/115002334246-DATIM-Data-Import-and-Exchange-Resources#current


# AGE/Sex Disaggregates ============================================================================

  # If you need to filter the data by age / sex disaggergates, you will need
  # to use a different type of standardized disaggegrate

   df %>%
    filter(indicator %in% indic_list) %>%
    count(indicator, standardizeddisaggregate, sex, trendscoarse) %>%
    spread(indicator, n) %>%
    prinf()

  # Base on the output, the you'll need the following stddisag for each indicator
  # HTS_TST --> Modality/Age/Sex/Result
  # TX_NEW  --> Age/Sex/HIVStatus
  # TX_CURR --> Age/Sex/HIVStatus
  # TX_PVLS --> Age/Sex/HIVStatus

  # The munging pipeline then becomes
  df %>%
    filter(standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Age/Sex/HIVStatus"),
           indicator %in% indic_list) %>%
    group_by(funding_agency, sex, trendscoarse, fiscal_year, indicator) %>%
    summarize(total = sum(cumulative, na.rm = T), .groups = "drop") %>%
    spread(indicator, total)



# Targets and achievement -------------------------------------------------
  # At times you'll likely be asked to show results, targets and achievement.
  # To do this, you will need to summarize the results / targets to the
  # appropriate aggregation level using the correct filters upstream.

  # Our starting query will then be modified in the summarise line
  # Notice, that we can use the lower level stddisags to get the same answer
  # but we cannot use the "Total Numerator" disags to get lower level answers
  df %>%
    filter(standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Age/Sex/HIVStatus"),
           indicator %in% indic_list) %>%
    group_by(funding_agency, fiscal_year, indicator) %>%
    summarize(across(c(cumulative, targets), \(x) sum(x, na.rm = T))) %>%
    calc_achievement()


  df_viz <- df %>%
    filter(standardizeddisaggregate %in% c("Modality/Age/Sex/Result", "Age/Sex/HIVStatus"),
           indicator %in% indic_list) %>%
    group_by(sex, fiscal_year, indicator) %>%
    summarize(across(c(cumulative, targets), \(x) sum(x, na.rm = T))) %>%
    calc_achievement() %>%
    arrange(indicator)


# Viral Load --> see the helper function

  # Create a data frame with viral load info on agency + sex
  vl_df <- create_vl_df(df, funding_agency, sex)


  # Create a data frame with viral load info on sex + age
  vl_df <- create_vl_df(df, trendscoarse, sex) %>%
    filter(str_detect(trendscoarse, "Unknown", negate = T))


# PLOTTING ----------------------------------------------------------------

  # make a plot showing the results and targets for each indicator by sex
  df_viz %>%
    ggplot(aes(x = sex, fill = sex)) +
    geom_col(aes(y = targets), fill = grey20k, width = 0.5, position = position_nudge(x = -0.1)) +
    geom_col(aes(y = cumulative),  width = 0.5) +
    geom_text(aes(y = cumulative, label = percent(achievement)),
              size = 10/.pt,
              vjust = -0.5) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    facet_wrap(~indicator) +
    scale_fill_manual(values = c("Female" = moody_blue, "Male" = genoa)) +
    labs(x = NULL, y = NULL,
         title = "TANZANIA INDICATOR SUMMARY BY SEX") +
    si_style_ygrid() +
    theme(legend.position = "none")

