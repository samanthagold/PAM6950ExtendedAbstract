
# -------------------------------------------------------------------------
## Purpose: get migration data ready for sankey plots + generate sankey plots
## - produce flows_black_czfeatures [X]
## - add on cz names (to and from) [X]
## - add in full variables that are not in flows_pooled_czfeatures [X]
## - merge on opportunity metrics from OpportunityAtlas/clean/cz_outcomes_clean_race.csv [X]
## - scatter plots of origin segregation and origin opportunity [x]
    ## these show an unintuitive relationship between segregation and opportunity. 
    ## focus on black-white dissimilarity 
    ## focus on black opportunity 
## - categorize high segregation vs. low segregation (use multi-group Theil Index for now) []
## - categorize high opportunity vs. low opportunity []
# -------------------------------------------------------------------------


# Setup -------------------------------------------------------------------
library(tidyverse)

flows_pooled_czfeatures <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/opp_migration_with_features.csv"
flows_black <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/MigrationPatternsData/od_race.csv"
flows_pooled <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/MigrationPatternsData/od_pooled.csv"
flows_black_czfeatures <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/flows_black_czfeatures.csv"
cz_opportunity_black <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/OpportunityAtlas/clean/cz_outcomes_clean_race_supersubset.csv"
cz_opportunity_pooled <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/OpportunityAtlas/clean/cz_outcomes_clean_pooled_supersubset.csv"
outdir <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/output/"
# -------------------------------------------------------------------------


# produce flows_black_czfeatures  -----------------------------------
data <- read_csv(flows_black) %>% 
  filter(pool == "Black") %>% 
  rename_with(~paste0(.x, "_black"), .cols = everything()) %>% 
  rename(o_cz_name = o_cz_name_black, 
         d_cz_name = d_cz_name_black, 
         o_state_name = o_state_name_black, 
         d_state_name = d_state_name_black) %>% 
  inner_join(read_csv(flows_pooled_czfeatures), by = c("o_cz_black" = "o_cz", 
                                                       "d_cz_black" = "d_cz")) %>% 
  rename(o_cz = o_cz_black, 
         d_cz = d_cz_black) %>% 
  relocate(o_cz, d_cz, n, n_black, n_tot_o, n_tot_o_black, n_tot_d, n_tot_d_black, 
           pr_d_o_black, pr_o_d_black, everything()) %>% 
  # merge on opportunity -------------------------------------------------- 
  inner_join(read_csv(cz_opportunity_black) %>% 
               select(cz, contains("black")) %>% 
                        rename_with(~paste0("o_", .x), .cols = everything())
             , by = "o_cz") %>%
  inner_join(read_csv(cz_opportunity_black) %>% 
               select(cz, contains("black")) %>% 
                        rename_with(~paste0("d_", .x), .cols = everything())
             , by = "d_cz") %>% 

  # merge on opportunity - pooled -------------------------------------------
  inner_join(read_csv(cz_opportunity_pooled) %>% rename_with(~paste0("o_", .x), .cols = everything()), 
             by = "o_cz") %>% 
  inner_join(read_csv(cz_opportunity_pooled) %>% rename_with(~paste0("d_", .x), .cols = everything()), 
             by = "d_cz") 
  
data %>% write_csv(flows_black_czfeatures)
# -------------------------------------------------------------------------


# origin opportunity vs. origin segregation -------------------------------
ggplot(data, aes(x = o_kfr_top20_pooled_pooled_mean, y = o_racial_segregation)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(data, aes(x = o_kfr_top01_pooled_pooled_mean, y = o_racial_segregation)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(data, aes(x = o_racial_segregation, y = o_kfr_top20_pooled_pooled_mean)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(data, aes(x = o_racial_segregation, y = o_kfr_top01_pooled_pooled_mean)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(data, aes(x = o_racial_segregation, y = o_kir_top20_pooled_pooled_mean)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(data, aes(x = o_racial_segregation, y = o_kir_top01_pooled_pooled_mean)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(data, aes(x = o_income_segregation, y = o_kir_top01_pooled_pooled_mean)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(data, aes(x = o_income_segregation, y = o_kir_top20_pooled_pooled_mean)) + 
  geom_point() + 
  geom_smooth(method = "lm")

## Note to self: there are some commuting zones with no black people (about 172 commuting zones)
## or the cells are too small to be able to look at it separately. Will need to use the pooled metrics 
## moving forward. Investigation code looking at large # of missing values 
# data %>% 
#   mutate(missing_opportunity_top1 = case_when(is.na(o_kfr_top01_black_pooled_mean) ~ 1, TRUE ~ 0)) %>% 
#   select(o_cz, d_cz, missing_opportunity_top1, o_cz_name, d_cz_name) %>% 
#   filter(missing_opportunity_top1 == 1) %>% 
#   distinct(o_cz_name) 

ggplot(data %>% 
         filter(n != 0) %>% 
         group_by(o_cz) %>% 
         summarize(d_avg_p_top01 = mean(d_kir_top01_pooled_pooled_mean)) %>% 
         ungroup() %>% 
         inner_join(data %>% select(o_cz, o_racial_segregation), by = "o_cz") %>% 
         distinct(), 
       aes(x = o_racial_segregation, y = d_avg_p_top01)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(data %>% 
         filter(n != 0) %>% 
         group_by(o_cz) %>% 
         summarize(d_avg_p_top20 = mean(d_kir_top20_pooled_pooled_mean)) %>% 
         ungroup() %>% 
         inner_join(data %>% select(o_cz, o_racial_segregation), by = "o_cz") %>% 
         distinct(), 
       aes(x = o_racial_segregation, y = d_avg_p_top20)) + 
  geom_point() + 
  geom_smooth(method = "lm")
## Interesting! There are some clear dividers when looking at the pooled data. 
## Not interested in opportunity of everyone (only in opportunity for minorities) 
## Also segregation is everyone. Not quite sure what is the right way to think about this 
## measurement. 

## Doesn't look like there is a strong relationship between segregation and opportunity. 
## or at least with these metrics. 
## might want to try an alternative segregation metric (currently using Theil's Index) 
## can't really change the opportunity metrics - but may want to focus on just black opportunity
## instead of opportunity for everyone. 
## as racial segregation increases, opportunity within the commuting zone increases (more affluent neighorhoods)
## I think provides the argument that I just want to focus at black outflows 
## What you want: a strong downward sloping 
## Something might be going wrong with the metrics that you are using... 





