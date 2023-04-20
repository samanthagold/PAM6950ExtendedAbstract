
# Setup -------------------------------------------------------------------
library(tidyverse)
library(tidycensus)
library(RStata)
library(biscale)

options(scipen = 999)

# Setup steps for running Stata in Rscript 
stata_fp <- "/Applications/Stata/StataSE.app/Contents/MacOS/stata-se" 
stata_version <- 15
options("RStata.StataPath" = stata_fp)
options("RStata.StataVersion" = stata_version)

states <- function() {
    c(
        "Alabama",
        "Arizona",
        "Arkansas",
        "California",
        "Colorado",
        "Connecticut",
        "Delaware",
        "District Of Columbia",
        "Florida",
        "Georgia",
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

# Data Gathering ----------------------------------------------------------

wd <- "/Users/sammygold/Documents/PAM6950ExtendedAbstract/data/"
outdir <- "/Users/sammygold/Documents/PAM6950ExtendedAbstract/output/"

# Loading in Opportunity Insights Migration #'s 
migration <- read_csv(paste0(wd, "od_pooled.csv")) %>%
    left_join(
        read_csv(paste0(wd, "cty_cz_st_crosswalk.csv")) %>%
            select(cty, county_name, cz), by = c("o_cz" = "cz"))

# Getting column names of relevant population variables using 
# tidycensus for get_decennial call 
pop2010vars <- load_variables(year = 2010, dataset = "sf1") %>% 
    filter(concept == "RACE") %>% 
    filter(name %in% c("P003001", "P003002", "P003003", 
                       "P003004", "P003005", "P003006", 
                       "P003007", "P003008")) %>% 
    pull(name)

# Pulling in 2010 decennial data 
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

# Pulling in 2000 decennial data 
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
    mutate(GEOID = as.integer(GEOID)) %>% 
    select(-P003002)


## Checking overlap with all datasources 
## Checking overlap with 2010 census 
missing_codes2010 <- anti_join(migration, census2010, by = c("cty" = "GEOID")) %>% 
    distinct(cty) %>% pull()

# County codes that are missing from 2010 census but are in 2000 census 
census_all <- census2010 %>% 
    bind_rows(census2000 %>% filter(GEOID %in% missing_codes2010))

# Looking into remaining county codes that are not in either the 
# 2010 census or the 2000 census that are in the opportunity 
# insights data 
anti_join(migration, census_all, by = c("cty" = "GEOID")) %>% 
    distinct(cty) %>% 
    pull()
## Two counties are an issue --> Dade and Skagway
## Looks like a renaming issue so just going to manually 
## recode these two counties with county that we have data on 


cz_pop <- migration %>% 
    # Manually recoding to adjust for county code changes for 
    # two problematic counties 
    mutate(cty = case_when(cty == 12025 ~ 12086, 
                           cty == 2231 ~ 2230, 
                           TRUE ~ cty)) %>% 
    left_join(census_all, by = c("cty" = "GEOID")) %>% 
    select(cty, o_cz, totalpop:nmixed) %>% 
    distinct(cty, o_cz, .keep_all = TRUE) 

dissimilarity <- 
    stata(
        "
    seg nwhite nblack, d by(o_cz) gen(d diswb) nodis
    
    ",
        data.in = cz_pop, 
        data.out = TRUE, 
        stata.echo = FALSE
    ) %>% 
    distinct(o_cz, .keep_all = TRUE) %>% 
    select(o_cz, diswb)

# Assuming that the migration file has each county for every commuting 
# zone included so summing up to the commuting zone level. 



# Final Data Prep ---------------------------------------------------------
final_data <- migration %>% 
    distinct(o_cz, .keep_all = TRUE) %>% 
    left_join(dissimilarity, by = c("o_cz")) %>% 
    select(-cty, -county_name) %>% 
    left_join(cz_pop %>% group_by(o_cz) %>% summarize(totalpop = sum(totalpop)), 
              by = "o_cz") %>% 
    mutate(perc_outflow = n_tot_o/totalpop) 



# Create quantiles for dissimilarity
seg_quantiles <- quantile(
    final_data$diswb, 
    seq(0, 1, length.out = 5)
)

outflow_quantiles <- quantile(
    final_data$perc_outflow, 
    seq(0, 1, length.out = 5)
)

# Create color scheme 
bivariate_color_scale <- dplyr::bind_rows(
    tibble::enframe(
        biscale::bi_pal("DkViolet2", dim = 4, preview = FALSE),
        name = "group",
        value = "fill"
    )
)


# Create quantiles in data 
spat_dat <- final_data |>
    dplyr::mutate(
        segregation = cut(
            diswb,
            breaks = seg_quantiles,
            include.lowest = TRUE
        ),
        #        absolute_mobility = forcats::fct_rev(absolute_mobility),
        outflow = cut(
            perc_outflow,
            breaks = outflow_quantiles,
            include.lowest = TRUE
        ),
        group = paste0(as.numeric(segregation), "-", as.numeric(outflow))
    ) |>
    dplyr::left_join(bivariate_color_scale, by = "group") 


# Mapping Prep ------------------------------------------------------------
font_fam <- "Trebuchet MS"
bivariate_color_scale2 <- bivariate_color_scale |>
    tidyr::separate(group, into = c("segregation", "outflow"), sep = "-") |>
    dplyr::mutate(
        segregation = as.integer(segregation),
        outflow = as.integer(outflow)
    )

legend <- ggplot2::ggplot() +
    ggplot2::geom_tile(
        data = bivariate_color_scale2,
        mapping = ggplot2::aes(
            x = segregation,
            y = outflow,
            fill = fill
        )
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::labs(
        x = "  Higher segregation \U00BB",
        y = "  Higher outflow \U00BB"
    ) +
    ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_text(size = 10),
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.grid = ggplot2::element_blank(),
        text = ggplot2::element_text(family = font_fam)
    ) +
    ggplot2::coord_fixed()


state_lines <- tigris::states(cb = TRUE, resolution = "20m", year = 1990) %>% 
    filter(NAME %in% states())
# Mapping migration data --------------------------------------------------


spat_dat <- sf::st_read("Documents/PAM 6950/HW3/cz1990_shapefile/cz1990.shp") %>% 
    inner_join(spat_dat, by = c("cz" = "o_cz")) %>% 
    tigris::shift_geometry() %>% 
    filter(! o_state_name %in% c("Alaska", "Hawaii"))

segregation_migration_map <- ggplot2::ggplot(
    data = spat_dat,
    mapping = ggplot2::aes(fill = fill)
) +
    ggplot2::geom_sf() +
    ggplot2::geom_sf(
        data = sf::st_transform(state_lines, sf::st_crs(spat_dat)),
        color = "white",
        alpha = 0,
        inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_identity() + 
    ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(
            color = "gray92",
            size = 0.2
        ),
        panel.grid.minor = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 18),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 15),
        legend.title = ggplot2::element_blank(),
        text = ggplot2::element_text(family = font_fam)
    ) + 
    ggplot2::labs(
        title = "Migration of Young Adults and Segregation in the U.S.",
        subtitle = "Percent of outflows and White-Black dissimilarity index by Commuting Zone", 
        caption = 
            "Percent of outflows represented as the number of people moving out of a given commuting zone divided by the number of people who live in the commuting zone. 
        Percent of outflows calculated using migration data from Opportunity Insights and population estimates from the Decennial Census. Note that the migration 
        patterns are restricted to adults younger than 26. The black-white dissimilarity index was constructed using population estimates primarily from the 2010 
        Decennial Census and supplemented with the 2000 Decennial Census where necessary. Quantile breaks were used to determine the color ramp for both layers of data. 
        Map projection used: USA Contiguous Albers Equal Area Conic. Commuting Zone shapefile from the Health Inequality Project."
        
    )

segregation_and_migration <- cowplot::ggdraw() +
    cowplot::draw_plot(segregation_migration_map, 0, 0, 1, 1) +
    cowplot::draw_plot(legend, .05, .075, .2, .2)


ggsave(plot = segregation_and_migration, 
       filename = paste0(outdir, "final_plot.png"), 
       height = 10, 
       width = 14)

# Migration Plot Only -----------------------------------------------------
migration_plot <- spat_dat %>% 
    ggplot(aes(fill = pr_d_o)) + 
    geom_sf(size = 0.001, color = "white") 
ggsave(plot = migration_plot, 
       filename = paste0(outdir, "migration_plot.png"), 
       height = 10, 
       width = 12)

