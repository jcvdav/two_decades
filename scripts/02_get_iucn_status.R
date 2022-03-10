######################################################
# iucn_status
######################################################
# 
# Get IUCN status for all monitored species
#
######################################################

# Load packages
library(here)
library(rredlist)
library(tidyverse)

# Define categories we care about
categories <- c("DD", "LC", "NT", "VU", "EN", "CR", "EW", "EX", "LRlc", "LRnt", "LRcd")

# Redefine a helper function to call the API and extract the result
get_spp <- function(cat) {
  rredlist::rl_sp_category(category = cat)$result %>% 
    mutate(iucn_cat = cat)
}

# Get list of all species in the categories of interest
red_spp <- map_dfr(categories, get_spp) %>% 
  mutate(scientific_name = str_extract(scientific_name, pattern = "[\\w]+\\s[\\w]+"))

# Get list of species in AquaMaps
spp_list <- list.files(here("data", "processed_data"), pattern = "transects.csv", full.names = T) %>% 
  map_dfr(read_csv, show_col_types = F) %>% 
  filter(!str_detect(species, "spp|Otros|Almeja")) %>% 
  pull(species) %>% 
  unique()

# Get the intersection of species
cr_spp <- red_spp %>% 
  filter(scientific_name %in% spp_list) %>% 
  select(scientific_name, iucn_cat)

# Export data
write_csv(cr_spp, here("data", "processed_data", "iucn_status.csv"))
