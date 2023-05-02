
# purpose -----------------------------------------------------------------
## output data file of the form: cz | o_n_tot_black | d_n_tot_black 
## needed to calculate percent outflows and percent inflows for mapping 
## relying on decennial census 
# -------------------------------------------------------------------------
# Setup -------------------------------------------------------------------
library(tidyverse)
library(tidycensus)
# In paths
xwalk <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/cty_cz_st_crosswalk.csv"
xwalk_daniel <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/cz_xwalk.csv"
# Outfile paths 
cz_pop_fp <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/cz_pop.csv"
# -------------------------------------------------------------------------
# Destination population  -------------------------------------------------
pop2010vars <- load_variables(year = 2010, dataset = "sf1") %>% 
  filter(concept == "RACE") %>% 
  filter(name %in% c("P003001", "P003002", "P003003", 
                     "P003004", "P003005", "P003006", 
                     "P003007", "P003008")) %>% 
  pull(name)

census2010 <- get_decennial(geography = "county",
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
  mutate(GEOID2 = as.integer(GEOID)) %>% 
  left_join(read_csv(xwalk_daniel), by = c("GEOID" = "county_fips_code")) %>% 
  left_join(read_csv(xwalk), by = c("GEOID2" = "cty")) %>% 
  mutate(commuting_zone = coalesce(commuting_zone_id, cz)) %>% 
  group_by(commuting_zone) %>% 
  summarize_at(vars(totalpop:nmixed), sum, na.rm = TRUE) %>% 
  distinct(commuting_zone, .keep_all = TRUE) %>% 
  filter(!is.na(commuting_zone))
# -------------------------------------------------------------------------
# Origin ------------------------------------------------------------------
census2000 <- get_decennial(geography = "county", 
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
  mutate(GEOID2 = as.integer(GEOID), 
         GEOID2 = case_when(GEOID2 == 12086 ~ 12025, 
                            GEOID2 == 2230 ~ 2231, 
                            TRUE ~ GEOID2)) %>% 
  left_join(read_csv(xwalk_daniel), by = c("GEOID" = "county_fips_code")) %>% 
  left_join(read_csv(xwalk), by = c("GEOID2" = "cty")) %>% 
  mutate(commuting_zone = coalesce(commuting_zone_id, cz)) %>% 
  group_by(commuting_zone) %>% 
  summarize_at(vars(totalpop:nmixed), sum, na.rm = TRUE) %>% 
  distinct(commuting_zone, .keep_all = TRUE) %>% 
  filter(!is.na(commuting_zone))
  

# -------------------------------------------------------------------------
# Final File --------------------------------------------------------------
cz_pop <- census2010 %>% 
  rename_with(~paste0("d_", .x), .cols = everything()) %>% 
  rename(cz = d_commuting_zone) %>% 
  inner_join(census2000 %>% 
               rename_with(~paste0("o_", .x), .cols = everything()) %>% 
               rename(cz = o_commuting_zone)
             , by = "cz") %>% 
  filter(!is.na(cz)) 

write_csv(cz_pop, file = cz_pop_fp)

  



