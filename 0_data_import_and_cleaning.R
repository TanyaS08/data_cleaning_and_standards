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
bromeliads = read.csv("data_raw//bwgv1_bromeliads.csv")
traits = read.csv("data_raw//bwgv1_traits.csv")

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

#' ------------------------------------------------------------------#
#'  The aim here is to check if the bromeliad IDs all 'talk'/match 
#'  with those in the `bromeliad` df. 
#' ------------------------------------------------------------------#

if (nrow(anti_join(abundance, bromeliads)) > 0) {
  # this will stop/exit the current code chunk if there is no match
  stop() 
  print("Not all bromeliad ID's have a match")
} else {
  # current code chunk will be run if all bromeliad IDs match
  print("All bromeliad ID's have a match")
}

#' ------------------------------------------------------------------#
#'  The aim here is to check if the species IDs all 'talk'/match 
#'  with those in the `triats` df. 
#' ------------------------------------------------------------------#

if (nrow(anti_join(abundance, traits)) > 0) {
  # this will stop/exit the current code chunk if there is no match
  stop() 
  print("Not all species ID's have a match")
} else {
  # current code chunk will be run if all bromeliad IDs match
  print("All species ID's have a match")
}


### 2) Adding taxonomic data ----

library(taxize)
# helper function for 'not in'
'%!in%' <- function(x,y)!('%in%'(x,y))

# get taxo data 

taxon_backbone = 
  abundance %>%
  distinct(taxon) %>%
  # we know for unknow species we cannot get any taxo data
  filter(taxon %!in% c("Unknown")) %>%
  pull(taxon) %>%
  classification(db = 'itis',
                 # remove 'interaction' to 'automate' pocess
                 ask = FALSE) %>% 
  rbind() %>% 
  tibble() %>% 
  select(-id) %>% 
  filter(rank %in% c("kingdom", "phylum", "class", "subclass",
                     "order", "family", "genus", "species")) %>%
  pivot_wider(., id_cols = "query", names_from = "rank", values_from = "name")

if (nrow(taxon_backbone) == nrow(abundance %>%
                                 distinct(taxon)%>%
                                 # we know for unknow species we cannot get any taxo data
                                 filter(taxon %!in% c("Unknown")))) {
  print("All species have a resolved taxonomy :)")
} else {
  missing = anti_join(abundance%>%
              distinct(taxon),
            taxon_backbone,
            by = c("taxon" = "query"))
   glue::glue("The following species have an unresloved taxonomy: {missing}")
   
   #' ------------------------------------------------------------------#
   #'  Here it throws the 'error' that some species are unresloved and
   #'  we need to go in and adress that 'manually' since both are worm
   #'  species I decided to query the wroms database
   #' ------------------------------------------------------------------#
   
   taxon_backbone = 
     taxon_backbone %>%
     bind_rows(abundance %>%
                 distinct(taxon) %>%
                 # we know for unknown species we cannot get any taxo data
                 filter(taxon %in% c("Oligochaeta", "Hirudinea")) %>%
                 pull(taxon) %>%
                 get_wormsid() %>%
                 classification(db = 'worms') %>% 
                 rbind() %>% 
                 tibble() %>% 
                 select(-id) %>% 
                 # make lowercase
                 mutate(rank = str_to_lower(rank)) %>% 
                 filter(rank %in% c("kingdom", "phylum", "class", "subclass",
                                    "order", "family", "genus", "species")) %>%
                 pivot_wider(., id_cols = "query", names_from = "rank", values_from = "name")) %>%
     select(-query)
   
   # Check again
   
   if (nrow(taxon_backbone) == nrow(abundance %>%
                                    distinct(taxon)%>%
                                    # we know for unknown species we cannot get any taxo data
                                    filter(taxon %!in% c("Unknown")))) {
     print("All species have a resolved taxonomy")
   } else {
     missing = anti_join(abundance%>%
                           distinct(taxon),
                         taxon_backbone,
                         by = c("taxon" = "query"))
     glue::glue("The following species have an unresloved taxonomy: {missing}")
   }
}

### 3) Exporting taxonomic data ----

dir.create("data_clean")

write_csv(taxon_backbone,
          "data_clean/invertebrate_taxonomy.csv")