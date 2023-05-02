
# Purpose -----------------------------------------------------------------
## Want to generate the segregation metrics to merge onto main analysis file 
### for each census year, get corresponding commuting zone [x]
### generate dissimilarity at the commuting zone level for easy merging [x]
### 


# Setup -------------------------------------------------------------------
library(tidyverse)
library(RStata)
library(tidycensus)
# Setup steps for running Stata in Rscript 
stata_fp <- "/Applications/Stata/StataSE.app/Contents/MacOS/stata-se" 
stata_version <- 15
options("RStata.StataPath" = stata_fp)
options("RStata.StataVersion" = stata_version)
# Relevant files 
xwalk <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/cty_cz_st_crosswalk.csv"
xwalk_daniel <- "/Users/sammygold/Downloads/cz_xwalk.csv"
# Helpers
states <- function() {
  c(
    "Alabama",
    "Alaska", 
    "Arizona",
    "Arkansas",
    "California",
    "Colorado",
    "Connecticut",
    "Delaware",
    "District Of Columbia",
    "Florida",
    "Georgia",
    "Hawaii", 
    "Idaho",
    "Illinois",
    "Indiana",
    "Iowa",
    "Kansas",
    "Kentucky",
    "Louisiana",
    "Maine",
    "Maryland",
    "Massachusetts",
    "Michigan",
    "Minnesota",
    "Mississippi",
    "Missouri",
    "Montana",
    "Nebraska",
    "Nevada",
    "New Hampshire",
    "New Jersey",
    "New Mexico",
    "New York",
    "North Carolina",
    "North Dakota",
    "Ohio",
    "Oklahoma",
    "Oregon",
    "Pennsylvania",
    "Rhode Island",
    "South Carolina",
    "South Dakota",
    "Tennessee",
    "Texas",
    "Utah",
    "Vermont",
    "Virginia",
    "Washington",
    "West Virginia",
    "Wisconsin",
    "Wyoming"
  )
}
# Output filepaths
segregation_fp <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/segregation.csv"

# Segregation Metrics -----------------------------------------------------
## Getting census data for population #'s
pop2010vars <- load_variables(year = 2010, dataset = "sf1") %>% 
  filter(concept == "RACE") %>% 
  filter(name %in% c("P003001", "P003002", "P003003", 
                     "P003004", "P003005", "P003006", 
                     "P003007", "P003008")) %>% 
  pull(name)

# Pulling in 2010 decennial data - will be used for destination segregation 
census2010 <- get_decennial(geography = "tract",
                            state = states(),
                            year = 2010, 
                            variables = pop2010vars
) %>% 
  pivot_wider(names_from = variable, 
              values_from = value) %>% 
  rename(totalpop = P003001, 
         nwhite = P003002, 
         nblack = P003003, 
         nnative = P003004, 
         nasian = P003005, 
         npacific = P003006,
         nother = P003007, 
         nmixed = P003008) %>% 
  mutate(county = substr(GEOID, 1, 5), 
         county2 = as.integer(substr(GEOID, 1, 5)), 
         county2 = case_when(county2 == 12086 ~ 12025, 
                            county2 == 2230 ~ 2231, 
                            TRUE ~ county2)) %>% 
  left_join(read_csv(xwalk_daniel), by = c("county" = "county_fips_code")) %>% 
  left_join(read_csv(xwalk), by = c("county2" = "cty")) %>% 
  mutate(commuting_zone = coalesce(commuting_zone_id, cz))

  

dissimilarity_destination <- 
  stata(
    "
    seg nwhite nblack, d by(county) gen(d diswbcty) nodis
    seg nwhite nblack, d by(commuting_zone) gen(d diswbcz) nodis
    
    ",
    data.in = census2010, 
    data.out = TRUE, 
    stata.echo = FALSE
  ) %>% 
  group_by(commuting_zone) %>% 
  mutate(avg_diswbcty = mean(diswbcty)) %>% 
  ungroup() %>% 
  distinct(commuting_zone, avg_diswbcty, diswbcz) %>% 
  filter(!is.na(commuting_zone)) %>% 
  rename_with(~paste0("d_", .x), .cols = everything()) %>% 
  rename(cz = d_commuting_zone)


# Pulling in 2000 decennial data - will be used for origin segregation
census2000 <- get_decennial(geography = "tract", 
                            state = states(), 
                            year = 2000, 
                            variables = c(pop2010vars, "P003009")) %>% 
  pivot_wider(names_from = variable, 
              values_from = value) %>% 
  rename(totalpop = P003001, 
         nwhite = P003003, 
         nblack = P003004, 
         nnative = P003005, 
         nasian = P003006, 
         npacific = P003007, 
         nother = P003008, 
         nmixed = P003009) %>% 
  select(-P003002) %>% 
  mutate(county = substr(GEOID, 1, 5), 
         county2 = as.integer(substr(GEOID, 1, 5)), 
         county2 = case_when(county2 == 12086 ~ 12025, 
                             county2 == 2230 ~ 2231, 
                             TRUE ~ county2)) %>% 
  left_join(read_csv(xwalk_daniel), by = c("county" = "county_fips_code")) %>% 
  left_join(read_csv(xwalk), by = c("county2" = "cty")) %>% 
  mutate(commuting_zone = coalesce(commuting_zone_id, cz))

dissimilarity_origin <- 
  stata(
    "
    seg nwhite nblack, d by(county) gen(d diswbcty) nodis
    seg nwhite nblack, d by(commuting_zone) gen(d diswbcz) nodis
    
    ",
    data.in = census2000, 
    data.out = TRUE, 
    stata.echo = FALSE
  ) %>% 
  group_by(commuting_zone) %>% 
  mutate(avg_diswbcty = mean(diswbcty)) %>% 
  ungroup() %>% 
  distinct(commuting_zone, avg_diswbcty, diswbcz) %>% 
  filter(!is.na(commuting_zone)) %>% 
  rename_with(~paste0("o_", .x), .cols = everything()) %>% 
  rename(cz = o_commuting_zone)

# Merging both 
segregation <- dissimilarity_origin %>% 
  inner_join(dissimilarity_destination, by = "cz")
write_csv(segregation, segregation_fp)
