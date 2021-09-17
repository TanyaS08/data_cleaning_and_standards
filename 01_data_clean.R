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

bromeliads = read_csv("data_raw/bwgv1_bromeliads.csv")

# create folder if not present in wd to save ouitputs
dir.create("data_clean")

### 1) Outliers in bromeliad data ----

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
           remove = FALSE)  %>%
  # this adds whitepsace for the binomial name (for finding taxonomic info)
  mutate(binomial = if_else(species == "sp",
                            genus,
                            str_replace_all(binomial,
                                            "_",
                                            " ")))

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
  select(bromeliad_id, binomial, genus, species,
         max_water, num_leaf)

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

### 2) Adding taxonomic data for bromeliads ----

library(taxize)

# get taxonomic data 

taxon_backbone = 
  bromeliads %>%
  distinct(binomial) %>%
  pull(binomial) %>%
  # get taxo data
  classification(db = 'ncbi',
                 # remove 'user interaction' to automate the pocess
                 ask = FALSE) %>% 
  # wrangle into prettier format
  rbind() %>% 
  tibble() %>% 
  select(-id) %>%
  mutate(rank = str_to_lower(rank)) %>% 
  filter(rank %in% c("kingdom", "phylum", "class", "subclass",
                     "order", "family", "genus", "species")) %>%
  pivot_wider(id_cols = "query", 
              names_from = "rank", 
              values_from = "name")
# no match for Vriesea kupperiana BUT we know it will share same 
# higher taxonomy with other Vriesea sp. so we can duplicate them

taxon_backbone = 
  taxon_backbone %>%
  bind_rows(taxon_backbone %>%
              filter(query == "Vriesea" & is.na(species)) %>%
              mutate(species = "kupperiana"))

#' ------------------------------------------------------------------#
#'  Here we can adress species that were not 'captured' in the first 
#'  pass at getting upstream taxonomic information by querying a 
#'  different database. If more species are added and are unmatched you 
#'  can address that here as well and add them using a `bind_rows` call
#' ------------------------------------------------------------------#


#' ------------------------------------------------------------------#
#'  This checks if all species have a resloved taxonomy by comparing 
#'  the number of rows (species) in the new `taxon_backbone` df to 
#'  the original (`bromeliads`) dataset - if it 'passes' we 
#'  then write the file to a .csv file
#' ------------------------------------------------------------------#

if (nrow(taxon_backbone) == nrow(bromeliads %>%
                                 distinct(binomial))) {
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
  
  missing = anti_join(bromeliads %>%
                        distinct(binomial),
                      taxon_backbone,
                      by = c("binomial" = "query"))
  
  stop(glue::glue("The following species have an unresloved taxonomy: {missing}"))
}



