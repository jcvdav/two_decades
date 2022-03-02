######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(kableExtra)
library(tidyverse)

iucn_status <- read_csv(here("data", "processed_data", "iucn_status.csv"), show_col_types = F)

iucn_status %>%
  filter(! iucn_cat %in% c("DD", "LC")) %>% 
  select(iucn_cat, scientific_name) %>% 
  kable(col.names = c("IUCN Category", "Species"),
        format = "latex",
        caption = "IUCN status for 30 species that are not data defficient or of least concern",
        label = "iucn",
        booktabs = T) %>% 
  kable_styling() %>% 
  column_spec(column = 2, italic = T) %>% 
  collapse_rows(columns = 1) %>% 
  cat(file = here("tab", "iucn_table.tex"))
