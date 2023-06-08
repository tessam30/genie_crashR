# PROJECT: Genie Crash course
# PURPOSE: Munge and Analysis of Genie Dat
# AUTHOR: Tim Essam | SI
# REF ID:   e3a75326
# LICENSE: MIT
# DATE: 2023-06-08
# NOTES: Tim Essam | SI

# SETUP ===========================================================================

  # We uses a series of custom packages for data munging
  # https://usaid-oha-si.github.io/tools/

  # Install SI specific R packages we use for data munging assistance.
  purrr::map(list("glitr", "glamr", "gisr", "gophr", "selfdestructin5",
                  "grabr", "gagglr"),
             ~devtools::install_github(paste0("USAID-OHA-SI/", ., "")))


# LOAD LIBRARIES ----------------------------------------------------------

  library(gagglr)

# CONFIGURE SETUP =================================================================

  # We will be using these two functions to configure your workspace
  # and to point towards a standardized file path
  ?set_paths()
  ?si_path()

  # Running this chunk below will create a set of paths where PEPFAR data
  # will be stored. Customize this as needed via your .Rprofile.
  # When done, save it and restart Rstudio.
  set_paths()


