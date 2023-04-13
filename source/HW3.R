
# Setup -------------------------------------------------------------------
library(tidyverse)
library(tidycensus)
options(scipen = 999)

wd <- "/Users/sammygold/Documents/PAM6950ExtendedAbstract/"

migration <- read_csv(paste0(wd, "data/od_pooled.csv")) %>%
    left_join(
        read_csv(paste0(wd, "data/cty_cz_st_crosswalk.csv")) %>%
            select(cty, county_name, cz), by = c("o_cz" = "cz"))


census2020 <- read_csv(paste0(wd, "data/DECENNIALPL2020.P3-Data.csv")) %>%
     slice(-1) %>%
     rename(totalpop = P3_001N,
            nwhite = P3_003N,
            nblack = P3_004N,
            nasian = P3_006N,
            nnative = P3_005N,
            npacfic = P3_007N,
            nmixed = P3_009N,
            nother = P3_008N
            ) %>%
     select(GEO_ID,
            NAME,
            totalpop,
            starts_with("n")) %>%
     mutate(ctycode = str_split(GEO_ID, "US", simplify = TRUE)[,2] %>% as.integer())

# ## Checking merge
overlap <- migration %>% inner_join(census2020, by = c("cty" = "ctycode")) %>% pull(cty)
anti_join(migration, census2020, by = c("cty" = "ctycode")) %>% distinct(cty) 


#census_api_key(key = "0ef9c9a04e91d40ab1346a3c8df6c2babab3ac57")
pop2010vars <- load_variables(year = 2010, dataset = "sf1") %>% 
    filter(concept == "RACE") %>% 
    filter(name %in% c("P003001", "P003002", "P003003", 
                       "P003004", "P003005", "P003006", 
                       "P003007", "P003008")) %>% 
    pull(name)
#census_api_key(key = "0ef9c9a04e91d40ab1346a3c8df6c2babab3ac57", install = TRUE, overwrite = TRUE)
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
    mutate(GEOID = as.integer(GEOID))


pop2000vars <- load_variables(year = 2000, dataset = "sf1") %>% 
    filter(concept == "RACE [71]") 

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
    mutate(countycd = as.integer(GEOID))


## Checking overlap with all datasources 
anti_join(migration, census2010, by = c("cty" = "GEOID")) %>% 
    anti_join(census2000 %>% mutate(ctycd = as.integer(GEOID)), by = c("cty" = "ctycd"))
## Two counties are an issue --> Dade and Skagway
## In total there are only 5 weird counties 

final_data <- migration %>% 
    inner_join(census2010, by = c("cty" = "GEOID")) %>% 
    inner_join(census2000, by = c("cty" = "countycd"))

# Mapping migration data --------------------------------------------------



    

spat_dat <- sf::st_read(paste0(wd, "data/cz1990.shp")) %>% 
    inner_join(migration %>% distinct(d_cz, .keep_all = TRUE), by = c("cz" = "d_cz")) %>% 
    tigris::shift_geometry()


migration_plot <- spat_dat %>% 
    ggplot(aes(fill = pr_d_o)) + 
    geom_sf(size = 0.001, color = "white") 
ggsave(plot = migration_plot, 
       filename = paste0(wd, "output/migration_plot.png"), 
       height = 10, 
       width = 12)
