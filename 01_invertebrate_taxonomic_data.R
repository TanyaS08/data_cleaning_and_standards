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

abundance = read.csv("data_raw//bwgv1_abundance.csv")

### 1) Some initial checks ----

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

### 2) Adding taxonomic data ----

library(taxize)

# get taxonomic data 

taxon_backbone = 
  abundance %>%
  distinct(taxon) %>%
  # we know for unknown species we cannot get any taxonomic data
  filter(taxon != "Unknown") %>%
  pull(taxon) %>%
  # get taxo data
  classification(db = 'itis',
                 # remove 'user interaction' to automate the pocess
                 ask = FALSE) %>% 
  # wrangle into prettier format
  rbind() %>% 
  tibble() %>% 
  select(-id) %>% 
  filter(rank %in% c("kingdom", "phylum", "class", "subclass",
                     "order", "family", "genus", "species")) %>%
  pivot_wider(id_cols = "query", 
              names_from = "rank", 
              values_from = "name") %>%
  #' ------------------------------------------------------------------#
  #'  Here we can adress species that were not 'captured' in the first 
  #'  pass at getting upstream taxonomic information. If more species 
  #'  are added and are unmatched you can adress that here as well
  #' ------------------------------------------------------------------#
  bind_rows(
    abundance %>%
      distinct(taxon) %>%
      # these were the two unmatched morpho species
      filter(taxon %in% c("Oligochaeta", "Hirudinea")) %>%
      pull(taxon) %>%
      # querying worms db
      classification(db = 'worms') %>% 
      rbind() %>% 
      tibble() %>% 
      select(-id) %>% 
      # make lowercase
      mutate(rank = str_to_lower(rank)) %>% 
      filter(rank %in% c("kingdom", "phylum", "class", "subclass",
                         "order", "family", "genus", "species")) %>%
      pivot_wider(id_cols = "query",
                  names_from = "rank",
                  values_from = "name")
  )


#' ------------------------------------------------------------------#
#'  This checks if all species have a resloved taxonomy by comparing 
#'  the number of rows (species) in the new `taxon_backbone` df to 
#'  the original (`abundance`) dataset - if it 'passes' we then write 
#'  the file to a .csv file
#' ------------------------------------------------------------------#

if (nrow(taxon_backbone) == nrow(abundance %>%
                                 distinct(taxon) %>%
                                 # we know for unknown species we cannot get any taxo data
                                 filter(taxon != "Unknown"))) {
  print("All species have a resolved taxonomy :) and are exported as `data_clean/invertebrate_taxonomy.csv`")
  
  # all checks passed: write file
  
  dir.create("data_clean")
  
  write_csv(taxon_backbone %>%
              select(-query),
            "data_clean/invertebrate_taxonomy.csv")
  
  #' ------------------------------------------------------------------#
  #'  If the evalutation fails i.e row numbers are not the same it 
  #'  throws an error and lists the 'unresolved' species
  #' ------------------------------------------------------------------#
  
} else {
  
  missing = anti_join(abundance %>%
                        distinct(taxon),
                      taxon_backbone,
                      by = c("taxon" = "query"))
  
  stop(glue::glue("The following species have an unresloved taxonomy: {missing}"))
}



