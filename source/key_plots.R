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
shapefile <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/cz1990_shapefile/cz1990.shp"
schools <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/schoolmobility_locations.csv"
# output directory
outdir <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/output/"
# -------------------------------------------------------------------------
# generate quartiles for outcomes of interest -----------------------------
data <- read_csv(data)




# plot 3 ------------------------------------------------------------------
# Final tweaks to data 
final_data <- data %>% 
  filter(o_cz == d_cz) %>% 
  mutate(outflows = n_tot_o_black - n_black, 
         perc_outflow = outflows/n_tot_o_black, 
         perc_outflow = case_when(is.na(perc_outflow) ~ 0, 
                                  is.infinite(perc_outflow) ~ 0, # result of random noise being added to publicly available dataset
                                  perc_outflow < 0 ~ 0,          # result of random noise 
                                  TRUE ~ perc_outflow))  %>% 
  rename(perc_noflow = pr_d_o_black)


# Create quantiles for dissimilarity
seg_quantiles <- quantile(
  final_data$o_diswbcz, 
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
      o_diswbcz,
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

## Map prep 
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
state_lines <- tigris::states(cb = TRUE, resolution = "20m", year = 1990) %>% 
  filter(NAME %in% states())

spat_dat <- sf::st_read(shapefile) %>% 
  inner_join(spat_dat, by = c("cz" = "o_cz")) %>% 
  tigris::shift_geometry() %>% 
  filter(! o_state_name_black %in% c("Alaska", "Hawaii"))

schools <- read_csv(schools) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), 
               crs = 4326, agr = "constant") %>% 
  mutate(high_mobility = case_when(mr_kq5_pq1 >= quantile(mr_kq5_pq1, prob = 0.75) ~1, 
                                   TRUE ~ 0))

# Finally mapping everything together 
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
  ggplot2::geom_sf(
    data = sf::st_transform(schools %>% filter(high_mobility == 1), sf::st_crs(spat_dat)), 
    alpha = 0.5, 
    size = 0.025, 
    inherit.aes = FALSE) + 
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
       filename = paste0(outdir, "plot3_segoutflows.png"), 
       height = 10, 
       width = 14)


# -------------------------------------------------------------------------
# plot 4 ------------------------------------------------------------------
final_data <- data %>% 
  filter(o_cz == d_cz) %>% 
  mutate(outflows = n_tot_o_black - n_black, 
         perc_outflow = outflows/n_tot_o_black, 
         perc_outflow = case_when(is.na(perc_outflow) ~ 0, 
                                  is.infinite(perc_outflow) ~ 0, # result of random noise being added to publicly available dataset
                                  perc_outflow < 0 ~ 0,          # result of random noise 
                                  TRUE ~ perc_outflow))  %>% 
  rename(perc_noflow = pr_d_o_black)


# Create quantiles for dissimilarity
seg_quantiles <- quantile(
  final_data$o_diswbcz, 
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
      o_diswbcz,
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

## Map prep 
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
state_lines <- tigris::states(cb = TRUE, resolution = "20m", year = 1990) %>% 
  filter(NAME %in% states())

spat_dat <- sf::st_read(shapefile) %>% 
  inner_join(spat_dat, by = c("cz" = "o_cz")) %>% 
  tigris::shift_geometry() %>% 
  filter(! o_state_name_black %in% c("Alaska", "Hawaii"))

schools <- read_csv(schools) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), 
               crs = 4326, agr = "constant") %>% 
  mutate(high_mobility = case_when(mr_kq5_pq1 >= quantile(mr_kq5_pq1, prob = 0.75) ~1, 
                                   TRUE ~ 0))

# Finally mapping everything together 
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
  ggplot2::geom_sf(
    data = sf::st_transform(schools %>% filter(high_mobility == 1), sf::st_crs(spat_dat)), 
    alpha = 0.5, 
    size = 0.025, 
    inherit.aes = FALSE) + 
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


