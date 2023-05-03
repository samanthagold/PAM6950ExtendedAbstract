# purpose -----------------------------------------------------------------
## goal: execute main analysis for extended abstract 
### data cleaning task: generate quartiles for outcomes of interest [ ]
### plot 1 - heatmaps (origin segregation x destination economic opportunity) []
### plot 2 - create sankey graphs with flows colored by region of US/state? []
### data cleaning: create perc_outflows variable, create perc_inflows variable []
### plot 3 - mapping outflows + overlaying colleges (by color) []
### plot 4 - mapping inflows + overlaying college (by upward mobility status) []
# -------------------------------------------------------------------------
# Setup -------------------------------------------------------------------
library(tidyverse)
library(sf)
library(ggalluvial)
# infile
data <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/main_data.csv"
# output directory
output <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/output/"
# -------------------------------------------------------------------------
# generate quartiles for outcomes of interest -----------------------------







