# clean opportunity insights cz chars -------------------------------------
# purpose: select variables of interest, drop other variables 
# output: cleaned cz_chars file

# Setup --------------------------------------------------------------------
library(haven)
library(tidyverse)

wd <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/"
dirname <- "OpportunityAtlas"
filename_in <- "cz_covariates.dta"
outdir <- "/Users/sammygold/Documents/PAM6950ExtendedAbstract/data/OpportunityAtlas/clean/"
filename_out <- "/clean/cz_covariates_cleaned.csv"
# -------------------------------------------------------------------------

# Selecting Relevant Variables --------------------------------------------
read_dta(paste0(wd, dirname, "/", filename_in)) %>% 
    select(cz,  
           czname, 
           ann_avg_job_growth_2004_2013, 
           emp2000, 
           hhinc_mean2000, 
           job_density_2013, 
           poor_share2010, 
           popdensity2010, 
           rent_twobed2015, 
           popdensity2010, 
           frac_coll_plus2010, 
           foreign_share2010, 
           med_hhinc2016, 
           gsmn_math_g3_2013) %>% 
    write_csv(paste0(wd, dirname, filename_out))


# -------------------------------------------------------------------------










