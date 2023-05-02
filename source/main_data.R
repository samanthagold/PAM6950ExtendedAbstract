# -------------------------------------------------------------------------
## Purpose: second pass constructing main dataset for analysis 
## - gather black flows from files [X]
## - merge on opportunity metrics [X]
## - also try and find percentile rank/alternative measure of opportunity [X]
## - merge on segregation statistics 
# -------------------------------------------------------------------------
# Setup -------------------------------------------------------------------
options(scipen = 999)
library(tidyverse)
library(RStata)
library(tidycensus)



flows_black <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/MigrationPatternsData/od_race.csv"
cz_opportunity_black <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/OpportunityAtlas/clean/cz_outcomes_clean_race_supersubset.csv"
intergenerational_mobility <- "/Users/sammygold/Documents/Github/PAM6950ExtendedAbstract/data/IntergenerationalOpp/table_4.dta"
xwalk <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/cty_cz_st_crosswalk.csv"
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Merging things together -------------------------------------------------
data <- read_csv(flows_black) %>% 
  filter(pool == "Black") %>% 
  rename_with(~paste0(.x, "_black"), .cols = everything()) %>% 
  rename(o_cz = o_cz_black, 
         d_cz = d_cz_black) %>% 
  relocate(o_cz, d_cz, n_black, n_tot_o_black, n_tot_d_black, 
           pr_d_o_black, pr_o_d_black, everything()) %>% 
  # merge on opportunity 
  inner_join(read_csv(cz_opportunity_black) %>% 
               select(cz, contains("black")) %>% 
               rename_with(~paste0("o_", .x), .cols = everything())
             , by = "o_cz") %>%
  inner_join(read_csv(cz_opportunity_black) %>% 
               select(cz, contains("black")) %>% 
               rename_with(~paste0("d_", .x), .cols = everything())
             , by = "d_cz") %>% 
  # merge on intergenerational mobility 
  inner_join(read_dta(intergenerational_mobility) %>% 
               select(cz, kfr_black_pooled_p25, kfr_white_pooled_p25) %>% 
               rename_with(~paste0("o_", .x), .cols = everything())
             , by = "o_cz") %>% 
  inner_join(read_dta(intergenerational_mobility) %>% 
               select(cz, kfr_black_pooled_p25, kfr_white_pooled_p25) %>% 
               rename_with(~paste0("d_", .x), .cols = everything())
             , by = "d_cz") 



