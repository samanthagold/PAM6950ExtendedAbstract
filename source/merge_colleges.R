
# -------------------------------------------------------------------------
## Purpose: merge college scorecard data together 

# -------------------------------------------------------------------------
## Data file will be used in maps - points representing different types 
## of colleges. 

# Setup -------------------------------------------------------------------
library(tidyverse)
chetty_scorecard <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/CollegeMobilityScorecard/clean/scorecard_cleaned.csv"
addresses <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/CollegeScoreCard/clean/CollegeScoreCard_cleaned.csv"
outfile <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/UScolleges.csv"

# -------------------------------------------------------------------------


# Merge -------------------------------------------------------------------
read_csv(chetty_scorecard) %>%
  inner_join(read_csv(addresses) %>% mutate(opeid6 = as.numeric(opeid6)), by = c("opeid" = "opeid6")) %>% 
  mutate(latitude = case_when(latitude == "NULL" ~ NA_character_, 
                              TRUE ~ latitude), 
         longitude = case_when(longitude == "NULL" ~ NA_character_, 
                               TRUE ~ longitude)) %>% 
  group_by(super_opeid) %>% 
  fill(latitude:longitude, .direction = "updown") %>% 
  ungroup() %>% 
  distinct(super_opeid, .keep_all = TRUE) 
  
  
  


