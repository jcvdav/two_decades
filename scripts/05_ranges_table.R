######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(kableExtra)
library(tidyverse)

ranges <- read_csv(here("data", "processed_data", "range_extensions.csv"),
                   show_col_types = F)

ranges %>%
  arrange(Topic) %>% 
  kable(format = "latex",
        caption = "Long-term ecological monitoring has ancillary benefits. The ecological monitoring data was collected to evaluate the effectiveness of marine reserves, but has been used in a plethora of other studies that continue to advance our knowledge and understanding of the natural world.",
        label = "lit",
        booktabs = T,
        escape = F) %>% 
  kable_styling() %>% 
  column_spec(column = 1, width = "9cm") %>% 
  cat(file = here("tab", "literature.tex"))


