
# -------------------------------------------------------------------------
## Purpose: clean US Department of Ed college scorecard data 
## Cleaning needs: subset columns as much as possible so it is easier to work with 

# -------------------------------------------------------------------------


# Setup -------------------------------------------------------------------
library(tidyverse)
fp <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/CollegeScoreCard/"
outfp <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/CollegeScoreCard/clean/"
outfile <- "CollegeScoreCard_cleaned.csv"


# -------------------------------------------------------------------------


# Cleaning Data -----------------------------------------------------------
read_csv(paste0(fp, "CollegeScoreCard.csv")) %>% 
  select(OPEID6, 
         INSTNM, 
         LATITUDE, 
         LONGITUDE, 
         HBCU) %>%
  rename_with(~tolower(.x), .cols = everything()) %>% 
  write_csv(paste0(outfp, outfile))



