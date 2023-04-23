# setup -------------------------------------------------------------------
# goal: merge on opportunity insights economic mobility measures to 
# pairwise migration matrix. 
# date: apr 20 2023
# -------------------------------------------------------------------------


# setup -------------------------------------------------------------------
library(tidyverse)
library(haven)

wd <- "/users/sammygold/Documents/PAM6950ExtendedAbstract/"


# data --------------------------------------------------------------------
read_dta(paste0(wd, "data/preferred_measures-1.dta")) %>% 
    inner_join(read_csv(paste0(wd, "data/od_pooled.csv")), by = c("cz" = "o_cz")) %>% 
    inner_join(read_csv(paste0(wd, "data/od_pooled.csv")), by = c("cz" = "d_cz")) 






