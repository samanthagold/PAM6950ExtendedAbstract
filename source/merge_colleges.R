
# -------------------------------------------------------------------------
## Purpose: merge college scorecard data together 
# -------------------------------------------------------------------------
## produce a file that has the following columns opeid6 | mr_kq5_pq1 | longitude | latitude
## distinct at the opeid6 level ()

# Setup -------------------------------------------------------------------
library(tidyverse)
chetty_scorecard <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/CollegeMobilityScorecard/clean/scorecard_cleaned.csv"
addresses <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/CollegeScoreCard/clean/CollegeScoreCard_cleaned.csv"
outfile <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/schoolmobility_locations.csv"
# -------------------------------------------------------------------------
# Merge -------------------------------------------------------------------
schools <- read_csv(chetty_scorecard) %>%
  inner_join(read_csv(addresses) %>% mutate(opeid6 = as.numeric(opeid6)), by = c("opeid" = "opeid6")) %>% 
  mutate(latitude = case_when(latitude == "NULL" ~ NA_character_, 
                              TRUE ~ latitude), 
         longitude = case_when(longitude == "NULL" ~ NA_character_, 
                               TRUE ~ longitude)) %>% 
  filter(
         !is.na(latitude), # dropping any schools with no lat/long 
         name != "Colleges with insufficient data") %>% # dropping schools with insufficient data 
  select(super_opeid, instnm, mr_kq5_pq1, latitude, longitude) %>% 
  arrange(super_opeid) %>% 
  distinct(instnm, .keep_all = TRUE) 

write_csv(schools, outfile)
  

  

  
  
  


