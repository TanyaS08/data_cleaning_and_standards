#' ------------------------------------------------------------------#
#'  LDP Scientific Data Management in Ecology & Evolution Assignment 1
#'  script for importing and _some_ cleaning the bwgv dataset.
#'  Developed by: TANYA STRYDOM
#'  Reachable at tanya.strydom@umontreal.ca
#' ------------------------------------------------------------------#

### 0) Preamble ----
### >> a) Dependencies ----
library(here)
library(tidyverse)
library(assertr)
library(ggplot2)
library(taxize)

### >> b) Data import ----

myfiles <- list.files(
  path = "data_raw/", 
  pattern = "*.csv", 
  full.names = TRUE
)
myfiles

list2env(
  lapply(
    setNames(myfiles, 
             make.names(
               gsub(".*1_", "", 
                    tools::file_path_sans_ext(myfiles)))), 
    read_csv), 
  envir = .GlobalEnv)

#remove file list from environment
rm(myfiles)

### 1) Some intial (apriori) checks ----

#' ------------------------------------------------------------------#
#'  The aim here is to check for any potential misspelling of taxon 
#'  names in the `abundance` dataset and 'prepare' data to be used 
#'  for {taxize}
#' ------------------------------------------------------------------#

abundance = 
  abundance %>%
  # remove everything BUT alpha characters and make title case
  mutate(taxon = str_to_title(str_remove_all(bwg_name,
                                             "[^[:alpha:]]")))

#' ------------------------------------------------------------------#
#'  The aim here is to check if the bromeliad IDs all 'talk'/match 
#'  with those in the `bromeliad` df. 
#' ------------------------------------------------------------------#

if (nrow(anti_join(abundance, bromeliads)) > 0) {
  stop()
  print("Not all bromeliad id's have a match")
} else {
  print("All bromeliad id's have a match")
}

