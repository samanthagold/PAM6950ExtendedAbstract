
# -------------------------------------------------------------------------
# Purpose: clean cz_outcomes.dta 
# Outfile: subsetted dataset 
library(tidyverse)
library(haven)
fp <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/"
dir <- "OpportunityAtlas/"
filename <- "cz_outcomes.dta"
filename_out_pooled <- "clean/cz_outcomes_clean_pooled.csv"
filename_out_pooled_supersubset <- "clean/cz_outcomes_clean_pooled_supersubset.csv"
filename_out_race <- "clean/cz_outcomes_clean_race.csv"
filename_out_race_supersubset <- "clean/cz_outcomes_clean_race_supersubset.csv"
# -------------------------------------------------------------------------


# Subsetting Data - All Races ---------------------------------------------
read_dta(paste0(fp, dir, filename)) %>% 
    select(cz, 
           has_dad_pooled_pooled_mean, 
           has_mom_pooled_pooled_mean, 
           two_par_pooled_pooled_mean, 
           kfr_top01_pooled_pooled_mean, 
           kfr_top20_pooled_pooled_mean, 
           kfr_26_pooled_pooled_mean, 
           kir_top01_pooled_pooled_mean, 
           kir_top20_pooled_pooled_mean, 
           lpov_nbh_pooled_pooled_mean, 
           staycz_pooled_pooled_mean, 
           stayhome_pooled_pooled_mean, 
           teenbrth_pooled_female_mean, 
           work_24_pooled_pooled_mean) %>% 
    write_csv(paste0(fp, dir, filename_out_pooled))

read_dta(paste0(fp, dir, filename)) %>% 
  select(cz, 
         kir_top01_pooled_pooled_mean, 
         kir_top20_pooled_pooled_mean, 
         kfr_top01_pooled_pooled_mean, 
         kfr_top20_pooled_pooled_mean, 
         kfr_26_pooled_pooled_mean) %>% 
  write_csv(paste0(fp, dir, filename_out_pooled_supersubset)) 
# -------------------------------------------------------------------------


# Subsetting Data - Race -------------------------------------------------
race <- c("black", "white", "asian", "hisp", "other", "natam")
varp1 <- c("has_dad", "has_mom", "two_par", "kfr_top01", 
           "kfr_top20", "kir_top01", "kir_top20", "lpov_nbh", "staycz", 
           "stayhome", "teenbrth", "work_24")
varnames <- c()
for(i in race){
    for(v in varp1){
        if(v != "teenbrth"){
            varname <- paste0(v, "_", i, "_", "pooled_mean")
            varnames <- c(varnames, varname)
        } else {
            varname <- paste0(v, "_", i, "_", "female_mean")
            varnames <- c(varnames, varname)
        }
      
    }
    
}

read_dta(paste0(fp, dir, filename)) %>% 
    select(cz, 
           all_of(varnames)) %>% 
    write_csv(paste0(fp, dir, filename_out_race))

# SUPER Subsetted Data - Race -------------------------------------------------
race <- c("black", "white", "asian", "hisp", "other", "natam")
bareminimumvars <- c("kfr_top01", 
           "kfr_top20", 
           "kir_top01", 
           "kir_top20")
varnames <- c()
for(i in race){
  for(v in bareminimumvars){
    if(v != "teenbrth"){
      varname <- paste0(v, "_", i, "_", "pooled_mean")
      varnames <- c(varnames, varname)
    } else {
      varname <- paste0(v, "_", i, "_", "female_mean")
      varnames <- c(varnames, varname)
    }
    
  }
  
}
read_dta(paste0(fp, dir, filename)) %>% 
  select(cz, 
         all_of(varnames)) %>% 
  write_csv(paste0(fp, dir, filename_out_race_supersubset))


## need to figure out what kfr and kir are again 

