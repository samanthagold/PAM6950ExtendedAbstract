# purpose -----------------------------------------------------------------
## produce a file that has the following columns super_opeid | mr_kq5_pq1 | longitude | latitude
## distinct at the opeid6 level ()
# -------------------------------------------------------------------------


# Setup -------------------------------------------------------------------
library(tidyverse)
## In
mobilityrate <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/CollegeMobilityScorecard/clean/scorecard_cleaned.csv"
locations <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/CollegeScoreCard/clean/CollegeScoreCard_cleaned.csv"
## Out 
schoolmobility_locations <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/schoolmobility_locations.csv"
# -------------------------------------------------------------------------


# merging -----------------------------------------------------------------
schools <- read_csv(locations) %>% 
  mutate(opeid6_char = as.numeric(opeid6)) %>% 
  inner_join(read_csv(mobilityrate), by = c("opeid6_char" = "opeid")) %>% # drops ~5% of mobility rate colleges 
  filter(name != "Colleges with insufficient data") # keeping schools who have valid mr rates 

write_csv(schools, schoolmobility_locations)








