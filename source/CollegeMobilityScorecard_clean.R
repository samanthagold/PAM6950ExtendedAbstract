
# -------------------------------------------------------------------------
# purpose: clean the Opportunity Insights college scorecard data
# output: cleaned file with subsetted columns only 
# -------------------------------------------------------------------------


# Setup -------------------------------------------------------------------
library(tidyverse)
library(haven)
fp <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/CollegeMobilityScorecard/"
file <- "mrc_table1-2.dta"
xwalk <- read_dta(paste0(fp, "mrc_table11.dta")) %>% 
  select(super_opeid, 
         opeid, 
         institution_name, 
         superopeid_name) %>% 
  rename(opeid_name = institution_name) 

outdir <- "clean/scorecard_cleaned.csv"

# -------------------------------------------------------------------------


# Clean -------------------------------------------------------------------
read_dta(paste0(fp, file)) %>% 
    select(super_opeid, 
           name, 
           czname, 
           kq5_cond_parq1, 
           ktop1pc_cond_parq1, 
           mr_kq5_pq1, 
           mr_ktop1_pq1
           ) %>% 
    inner_join(xwalk, by = "super_opeid") %>% 
    write_csv(paste0(fp, outdir))





