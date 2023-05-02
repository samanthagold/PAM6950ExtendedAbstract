# -------------------------------------------------------------------------
## Purpose: second pass constructing main dataset for analysis 
## - gather black flows from files [X]
## - merge on opportunity metrics [X]
## - also try and find percentile rank/alternative measure of opportunity [X]
## - merge on segregation statistics [X]
## - organize metrics [X]
## - label all variables [X]
## - merge in population numbers (total number of black people in commuting zone) [X]
### Ambitious to-do's 
#### add in additional intergenerational metrics 
# -------------------------------------------------------------------------
# Setup -------------------------------------------------------------------
options(scipen = 999)
library(tidyverse)
library(RStata)
library(tidycensus)


# Infile Paths
flows_black <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/MigrationPatternsData/od_race.csv"
cz_opportunity_black <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/OpportunityAtlas/clean/cz_outcomes_clean_race_supersubset.csv"
intergenerational_mobility <- "/Users/sammygold/Documents/Github/PAM6950ExtendedAbstract/data/IntergenerationalOpp/table_4.dta"
segregation_fp <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/segregation.csv"
labels_fp <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/main_data_labels.csv"
cz_pop_fp <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/cz_pop.csv"

# Outfile Paths
data_fp <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/main_data.csv"
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
  # merge on opportunity - black
  inner_join(read_csv(cz_opportunity_black) %>% 
               select(cz, contains("black")) %>% 
               rename_with(~paste0("o_", .x), .cols = everything())
             , by = "o_cz") %>%
  inner_join(read_csv(cz_opportunity_black) %>% 
               select(cz, contains("black")) %>% 
               rename_with(~paste0("d_", .x), .cols = everything())
             , by = "d_cz") %>% 
  # merge on opportunity - white 
  inner_join(read_csv(cz_opportunity_black) %>% 
               select(cz, contains("white")) %>% 
               rename_with(~paste0("o_", .x), .cols = everything())
             , by = "o_cz") %>%
  inner_join(read_csv(cz_opportunity_black) %>% 
               select(cz, contains("white")) %>% 
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
             , by = "d_cz") %>% 
  # merge on segregation
  inner_join(read_csv(segregation_fp) %>% select(cz, starts_with("o_")), by = c("o_cz" = "cz")) %>% 
  inner_join(read_csv(segregation_fp) %>% select(cz, starts_with("d_")), by = c("d_cz" = "cz")) %>% 
  # opportunity gap metrics 
  mutate(o_kfr_top20_bwgap = o_kfr_top20_black_pooled_mean - o_kfr_top20_white_pooled_mean, 
         d_kfr_top20_bwgap = d_kfr_top20_black_pooled_mean - d_kfr_top20_white_pooled_mean, 
         o_kir_top20_bwgap = o_kir_top20_black_pooled_mean - o_kir_top20_white_pooled_mean, 
         d_kir_top20_bwgap = d_kir_top20_black_pooled_mean - d_kir_top20_white_pooled_mean, 
         o_kfr_p25_bwgap = o_kfr_black_pooled_p25 - o_kfr_white_pooled_p25, 
         d_kfr_p25_bwgap = d_kfr_black_pooled_p25 - d_kfr_white_pooled_p25) %>% 
  # commuting zone population #'s 
  inner_join(read_csv(cz_pop_fp) %>% select(cz, o_nblack), by = c("o_cz" = "cz")) %>% 
  inner_join(read_csv(cz_pop_fp) %>% select(cz, d_nblack), by = c("d_cz" = "cz")) %>% 
  # organizing 
  select(o_cz, d_cz, 
           o_cz_name_black, d_cz_name_black, 
           o_state_name_black, d_state_name_black, 
           n_black, # flows between o_cz, d_cz
           n_tot_o_black, n_tot_d_black, # total number of black in each cz
           pr_d_o_black, pr_o_d_black, 
           o_kfr_top20_black_pooled_mean, d_kfr_top20_black_pooled_mean, # opportunity - black
           o_kfr_top20_white_pooled_mean, d_kfr_top20_white_pooled_mean,
           o_kfr_top20_bwgap, d_kfr_top20_bwgap, 
           o_kir_top20_black_pooled_mean, d_kir_top20_black_pooled_mean, # opp alternative- black
           o_kir_top20_bwgap, d_kir_top20_bwgap, 
           o_kfr_black_pooled_p25, d_kfr_black_pooled_p25, # intergenerational upward mobility
           o_kfr_white_pooled_p25, d_kfr_white_pooled_p25, 
           o_kfr_p25_bwgap, d_kfr_p25_bwgap, 
           o_avg_diswbcty, d_avg_diswbcty, # black-white segregation
           o_diswbcz, d_diswbcz, 
           o_nblack, d_nblack) # population numbers from the decennial 2010 and 2000 surveys  

write_csv(data, data_fp)

# Labeling data frame 
labels <- read_csv(labels_fp)
for(i in 1:ncol(data)){
  var <- names(data[,i])
  label <- labels %>% filter(variable == var) %>% pull(label)
  if(!identical(label, character(0))){
    var <- data[, i, drop = TRUE]
    attr(var, "label") <- label
    data[, i] <- var
  }
  
}
  



