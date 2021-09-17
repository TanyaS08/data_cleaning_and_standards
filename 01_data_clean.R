#' ------------------------------------------------------------------#
#'  LDP Scientific Data Management in Ecology & Evolution Assignment 1
#'  17 September 2021
#'  Developed by: TANYA STRYDOM
#'  Reachable at tanya.strydom@umontreal.ca
#' ------------------------------------------------------------------#

### 0) Preamble ----
### >> a) Dependencies ----
library(here)
library(tidyverse)
library(assertr)
library(taxize)

### >> b) Data import ----

abundance = read.csv("data_raw/bwgv1_abundance.csv")
bromeliads = read_csv("data_raw/bwgv1_bromeliads.csv")

# create folder if not present in wd to save ouitputs
dir.create("data_clean")

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

### 2) Outliers in bromeliad data ----

### >> a) Check genus names are spelled correctly ----

# Make a reference dataframe with the correct spellings of Latin names 
# for species recorded in the study system, which can itself be quality 
# controlled for taxonomic updates, etc.

species_list <- 
  bromeliads %>% 
  distinct(species) %>%
  rename(binomial = species) %>% 
  separate(col = binomial,
           into = c("genus","species"),
           sep = "_",
           remove = FALSE)

bromeliads <- 
  bromeliads %>% 
  rename(binomial = species) %>%
  separate(col = binomial, into = c("genus","species"),
           sep = "_",
           remove = FALSE)

# Check for cases where the entry in the bromeliads dataset 
# is different from the species list i.e. a genus was 
# maybe mis-specified (this could possibly also be picked
# up by a distinct() call though)
anti_join(bromeliads, species_list, by = "binomial") 
# returns no inconsistencies

### >> b) Check for number of leaves and leaf water ----

# Here we're checking whether two measured variables, 
# max_water and num_leaf, do not have any biologically 
# unrealistic (i.e., outlier) values and that none of
# the values are negative; neither measured variable should 
# be greater than 0, though NA values are possible.

#select target columns
bromeliads_leafwater = 
  bromeliads %>% 
  select(bromeliad_id,binomial,genus,species,max_water,num_leaf)

##### Steps: 
#### 1. check both variables to make sure they're numeric

verify(bromeliads_leafwater,
       is.numeric(max_water)
       && is.numeric(num_leaf))

#### 2. make sure the values of both variables are ≥0

verify(bromeliads_leafwater,
       max_water >= 0 
       && num_leaf >= 0)

### >> c) Finding potential outliers ----

# first using the `error_fun` call in insist() we can add the 
#outputs as attributes which we use to find the 'problem' bromes

bromeliads_leafwater_w_attribute = 
  bromeliads_leafwater %>% 
  insist(within_n_mads(2), 
         # I combined both vars of interest to 'trim' the code
         c(max_water, num_leaf), 
         error_fun = error_append)

# this next bit is ugly but will have to do for now
# here (based on attributes) pull the index (row number) for 
# bromes that 'failed' the insist() 'test'

# max_water outliers
outlier_bromeliads_max_water = 
  attributes(bromeliads_leafwater_w_attribute)$assertr_errors[[1]]$error_df$index

# num_leaf outliers
outlier_bromeliads_num_leaf = 
  attributes(bromeliads_leafwater_w_attribute)$assertr_errors[[2]]$error_df$index

# we can now use this to filter against the `bromeliads_leafwater` df.
# intersect here only returns row numbers for bromes that
# have both leaves and max water that are outliers so point
# to bromes that are most likely 'freakishly' large/small i.e. 
# 'true' outliers (since we expect these two variables to co-vary)
# other entries I would probably check for potential 
# data entry mistakes

bromeliads_leafwater %>%
  filter(row_number() %in% 
           intersect(outlier_bromeliads_max_water,
                     outlier_bromeliads_num_leaf))

# for simplicity though we can create a .csv that lists all outlier
# bromes that a more informed user can look into. 

bromeliads_leafwater %>%
  filter(row_number() %in% outlier_bromeliads_max_water) %>%
  mutate(outlier_variable = "max_water") %>%
  bind_rows(bromeliads_leafwater %>%
              filter(row_number() %in% outlier_bromeliads_num_leaf) %>%
              mutate(outlier_variable = "num_leaf")) %>%
  write_csv("data_clean/outlier_bromeliads.csv")

### 3) Adding taxonomic data for invertebrates ----

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



