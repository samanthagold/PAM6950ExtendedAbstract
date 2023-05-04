# purpose -----------------------------------------------------------------
## goal: execute main analysis for extended abstract 
### data cleaning task: generate quartiles for outcomes of interest [X]
### plot 1 - heatmaps (origin segregation x destination economic opportunity) [X]
### plot 2 - create sankey graphs with flows colored by region of US/state? [X]
### data cleaning: create perc_outflows variable, create perc_inflows variable [X]
### plot 3 - mapping outflows + overlaying colleges (by color) [X]
### plot 4 - mapping inflows + overlaying college (by upward mobility status) [X]
### for plots 3 and 4 - make sure you are adding in alaska and hawaii [X]
# -------------------------------------------------------------------------
# Setup -------------------------------------------------------------------
library(tidyverse)
library(sf)
library(ggalluvial)
# infile
data <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/main_data.csv"
shapefile <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/cz1990_shapefile/cz1990.shp"
schools_fp <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/schoolmobility_locations.csv"
# output directory
outdir <- "/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/output/"
# helpers
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
# generate quartiles for outcomes of interest -----------------------------
## creating quantiles for segregation in origin 
data <- read_csv(data)
seg_quantiles_o <- quantile(
  data$o_diswbcz, 
  seq(0, 1, length.out = 4)
)

## creating quantiles for segregation in destination 
seg_quantiles_d <- quantile(
  data$d_diswbcz, 
  seq(0, 1, length.out = 4)
)


## Using this outcome because it is missing less than d_kfr_black_pooled_mean 
opp_quantiles_d <- quantile(
  data$d_kfr_black_pooled_p25, 
  seq(0, 1, length.out = 4), na.rm = TRUE
)

## opportunity of origin 
opp_quantiles_o <- quantile(
  data$o_kfr_black_pooled_p25, 
  seq(0, 1, length.out = 4), na.rm = TRUE
)

## actually creating categories in the data 
plot_data <- data %>% 
  mutate(
    origin_segregation_levels = cut(
      o_diswbcz, 
      breaks = seg_quantiles_o, 
      include.lowest = TRUE
    ),
    destination_segregation_levels = cut(
      o_diswbcz, 
      breaks = seg_quantiles_d, 
      include.lowest = TRUE
    ),
    dest_opp_levels = cut(
      d_kfr_black_pooled_p25, 
      breaks = opp_quantiles_d, 
      include.lowest = TRUE
    ), 
    origin_opp_levels = cut(
      o_kfr_black_pooled_p25, 
      breaks = opp_quantiles_o, 
      include.lowest = TRUE
    ), 
    origin_segregation_levels = case_when(origin_segregation_levels == "[0,0.367]" ~ "Low Segregation", 
                                          origin_segregation_levels == "(0.367,0.498]" ~ "Medium Segregation", 
                                          origin_segregation_levels == "(0.498,0.882]" ~ "High Segregation", 
                                          TRUE ~ NA_character_), 
    dest_opp_levels = case_when(dest_opp_levels == "[16.4,32.1]" ~ "Low Opportunity", 
                                dest_opp_levels == "(32.1,34.9]" ~ "Medium Opportunity", 
                                dest_opp_levels == "(34.9,56.6]" ~ "High Opportunity", 
                                TRUE ~ NA_character_), 
    dest_seg_levels = case_when(destination_segregation_levels == "[0,0.339]" ~ "Low Segregation", 
                                destination_segregation_levels == "(0.339,0.47]" ~ "Medium Segregation", 
                                destination_segregation_levels == "(0.47,0.856]" ~ "High Segregation", 
                                TRUE ~ NA_character_), 
    origin_opp_levels = case_when(origin_opp_levels == "[16.4,32.1]" ~ "Low Opportunity", 
                                  origin_opp_levels == "(32.1,34.9]" ~ "Medium Opportunity", 
                                  origin_opp_levels == "(34.9,56.6]" ~ "High Opportunity", 
                                  TRUE ~ NA_character_)
  ) %>% 
  # Restricting to different flows only 
  filter(o_cz != d_cz)




# plot 1 ------------------------------------------------------------------
plot_data %>% 
  group_by(origin_segregation_levels, dest_opp_levels) %>% 
  summarize(total_flows = sum(n_black)) %>% 
  filter(!is.na(dest_opp_levels)) %>% 
  ggplot(aes(x = origin_segregation_levels, 
             y = dest_opp_levels, 
             fill = total_flows)) +
  geom_tile()
  
# plot 2 1st pass ------------------------------------------------------------------
plot_data %>% 
  group_by(origin_segregation_levels, dest_opp_levels) %>% 
  summarize(total_flows= sum(n_black)) %>% 
  filter(!is.na(dest_opp_levels)) %>% 
  group_by(origin_segregation_levels) %>% 
  mutate(sum_origin_flows = sum(total_flows)) %>% 
  ungroup() %>% 
  mutate(perc_flow = total_flows/sum_origin_flows) %>%
         #, 
         #origin_segregation_levels = case_when(origin_segregation_levels == "High" ~ "High Segregation", 
        #                                       origin_segregation_levels == "Low" ~ "Low Segregation", 
        #                                       origin_segregation_levels == "Medium" ~ "Medium Segregation"), 
        # dest_opp_levels = case_when(dest_opp_levels == "High" ~ "High Opportunity",
        #                             dest_opp_levels == "Medium" ~ "Medium Opportunity", 
        #                             dest_opp_levels == "Low" ~ "Low Opportunity")) %>%
  group_by(origin_segregation_levels) %>% 
  arrange(desc(perc_flow), origin_segregation_levels) %>% 
  mutate(row_num = row_number(), 
         top_flow = case_when(row_num == 1 ~ 1, TRUE ~ 0), 
         fill = case_when(top_flow == 1 ~ "#0B58C7", 
                          TRUE ~ "#8A96A7")) %>% 
  ungroup() %>% 
#  mutate(color = case_when(origin_segregation_levels == "Low" & dest_opp_levels == "High" ~ "Green", 
#                           origin_segregation_levels == "Low" & dest_opp_levels == "Medium" ~ "Green", 
#                           origin_segregation_levels == "Low" & dest_opp_levels == "Low" ~ "Red", 
#                           origin_segregation_levels == "Medium" & dest_opp_levels == "High" ~ "Green", 
#                           origin_segregation_levels == "Medium" & dest_opp_levels == "Medium" ~ "Grey", 
#                           origin_segregation_levels == "Medium" & dest_opp_levels == "Low" ~ "Red", 
#                           origin_segregation_levels == "High" & dest_opp_levels == "High" ~ "Grey", 
#                           origin_segregation_levels == "High" & dest_opp_levels == "Medium" ~ "Red", 
#                           origin_segregation_levels == "High" & dest_opp_levels == "Low" ~ "Red")) %>% 
  as.data.frame() %>% 
  ggplot(aes(y = perc_flow, 
             axis1 = origin_segregation_levels, 
             axis2 = dest_opp_levels)) + 
  #geom_alluvium(aes(fill = factor(color)), width = 1/12) + 
  geom_alluvium(aes(fill = fill), width = 1/12) + 
  geom_stratum(width = 1/12, fill = "black", color = "grey") + 
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) + 
  scale_x_discrete(limits = c("Origin Segregation", "Destination Opportunity"), expand = c(0.05, 0.05)) + 
  theme_minimal() + 
  scale_fill_identity()



# key_flows ---------------------------------------------------------------
plot_data %>% 
  filter(origin_opp_levels == "Low Opportunity" & dest_opp_levels == "High Opportunity") %>% 
  arrange(desc(n_black)) %>% 
  slice_head(n = 3) %>% 
  select(o_cz_name_black, d_cz_name_black, n_black) 

plot_data %>% 
  filter(origin_segregation_levels == "High Segregation" & dest_opp_levels == "High Opportunity") %>% 
  arrange(desc(n_black)) %>% 
  slice_head(n = 3) %>% 
  select(o_cz_name_black, d_cz_name_black, n_black)

plot_data %>% 
  filter(origin_segregation_levels == "High Segregation" & dest_seg_levels == "Low Segregation") %>% 
  arrange(desc(n_black)) %>% 
  slice_head(n = 3) %>% 
  select(o_cz_name_black, d_cz_name_black, n_black)



# descriptive analysis ----------------------------------------------------
## how many flows were there in total? this is the denominator to provide context
data %>% summarize(total_flows = sum(n_black))
## how many flows were within commuting zone flows?? 
data %>% 
  add_tally(n_black, name = "total_flows") %>% 
  # What are the avg characteristics of within_flows vs. moving origin cz flows? 
  mutate(same_cz = case_when(o_cz == d_cz ~ 1, 
                                    TRUE ~ 0)) %>% 
  group_by(same_cz) %>% # different flows 
  mutate(total_by_flow_type = sum(n_black), 
         perc_by_flow_type = round(sum(n_black)/total_flows * 100, 3), 
         avg_o_seg = mean(o_diswbcz), 
         avg_d_seg = mean(d_diswbcz), 
         avg_o_kfr_black_pooled_p25 = mean(o_kfr_black_pooled_p25, na.rm = TRUE), 
         avg_d_kfr_black_pooled_p25 = mean(d_kfr_black_pooled_p25, na.rm = TRUE)) %>%
  ungroup() %>% 
  select(same_cz, total_by_flow_type, total_flows, perc_by_flow_type, avg_o_seg, avg_d_seg, avg_o_kfr_black_pooled_p25, avg_d_kfr_black_pooled_p25) %>% 
  distinct()
# same_cz | total_flows | perc_by_flow_type | avg o seg | avg d seg | avg o opportunity | avg d opportunity 
## something funky going on with metrics?? 

# Where are the largest number of flows from by type..
data %>% 
  add_tally(n_black, name = "total_flows") %>% 
  mutate(same_cz = case_when(o_cz == d_cz ~ 1, 
                             TRUE ~ 0)) %>% 
  select(same_cz, o_cz_name_black, d_cz_name_black, n_black, total_flows) %>% 
  group_by(same_cz) %>% 
  arrange(desc(n_black)) %>% 
  mutate(total_by_flow_type = sum(n_black), 
         perc_by_flow_type = round(sum(n_black)/total_flows * 100, 3), 
         share = n_black/total_by_flow_type) %>% 
  slice_head(n = 10) %>% 
  select(same_cz, o_cz_name_black, d_cz_name_black,share) %>% 
  distinct() 
# same_cz, o_cz, d_cz, share 
# takeaway: there isn't a clear winner in any of the flow types 
  

# Where aren't young black adults not moving to? 
data %>% 
  mutate(nonzero_black_flows = case_when(n_black == 0 ~ 0,
                                         TRUE ~ 1), 
         total_possible = n()) %>% 
  group_by(nonzero_black_flows) %>% 
  mutate(total_flows_w_noblack = n()/total_possible) %>% 
  distinct(nonzero_black_flows, total_flows_w_noblack) 
  # nonzero_black_flows | total_flows_w_noblack
# Large number of commuting zones that black young adults do not move to. 

# Where are they avoiding that are popular among others? 
# could be helpful to have a map of all the places people aren't moving 
# final piece: map?
# not sure if you can map this...how to represent the unique cz to dcz combinaition. 
# would need to look at flows of white people 
# data %>% 
#   mutate(nonzero_black_flows = case_when(n_black == 0 ~ 0,
#                                          TRUE ~ 1), 
#          total_possible = n())
# black_flows <- read_csv("/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/MigrationPatternsData/od_race.csv") %>% 
#   filter(pool == "Black") %>% 
#   rename_with(~paste0(.x, "_black"), .cols = everything()) %>% 
#   rename(o_cz = o_cz_black, 
#          d_cz = d_cz_black) 
# 
# white_flows <- read_csv("/Users/sammygold/Documents/GitHub/PAM6950ExtendedAbstract/data/MigrationPatternsData/od_race.csv") %>% 
#   filter(pool == "White") %>% 
#   rename_with(~paste0(.x, "_white"), .cols = everything()) %>% 
#   rename(o_cz = o_cz_white, 
#          d_cz = d_cz_white)
# 
# black_flows %>% 
#   inner_join(white_flows, by = c("o_cz", "d_cz")) %>% 
#   filter(n_black == 0) %>% 
#   arrange(desc(n_white)) %>%
#   slice_head(n = 100) %>% 
#   select(o_cz_name, d_cz_name, n_black, n_white)
# 
#   mutate(nonzero_black_flows = case_when(n_black == 0 ~ 0, 
#                                          TRUE ~ 1)) %>% 
#   


# Plot 2 - Seg x Opp ------------------------------------------------------


# Looking at counts instead of percents 
seg_opp_alluvium <- plot_data %>% 
  group_by(origin_segregation_levels, dest_opp_levels) %>% 
  summarize(total_flows= sum(n_black)) %>% 
  filter(!is.na(dest_opp_levels)) %>% 
  group_by(origin_segregation_levels) %>% 
  mutate(sum_origin_flows = sum(total_flows)) %>% 
  ungroup() %>% 
  mutate(perc_flow = total_flows/sum_origin_flows) %>% 
         #, 
  #       origin_segregation_levels = case_when(origin_segregation_levels == "High" ~ "High Segregation", 
  #                                             origin_segregation_levels == "Low" ~ "Low Segregation", 
  #                                             origin_segregation_levels == "Medium" ~ "Medium Segregation"), 
  #       dest_opp_levels = case_when(dest_opp_levels == "High" ~ "High Opportunity",
  #                                   dest_opp_levels == "Medium" ~ "Medium Opportunity", 
  #                                   dest_opp_levels == "Low" ~ "Low Opportunity")) %>%
  group_by(origin_segregation_levels) %>% 
  arrange(desc(perc_flow), origin_segregation_levels) %>% 
  mutate(row_num = row_number(), 
         top_flow = case_when(row_num == 1 ~ 1, TRUE ~ 0), 
         fill = case_when(top_flow == 1 ~ "#0B58C7", 
                          TRUE ~ "#8A96A7")) %>% 
  ungroup() %>% 
  #  mutate(color = case_when(origin_segregation_levels == "Low" & dest_opp_levels == "High" ~ "Green", 
  #                           origin_segregation_levels == "Low" & dest_opp_levels == "Medium" ~ "Green", 
  #                           origin_segregation_levels == "Low" & dest_opp_levels == "Low" ~ "Red", 
  #                           origin_segregation_levels == "Medium" & dest_opp_levels == "High" ~ "Green", 
  #                           origin_segregation_levels == "Medium" & dest_opp_levels == "Medium" ~ "Grey", 
  #                           origin_segregation_levels == "Medium" & dest_opp_levels == "Low" ~ "Red", 
  #                           origin_segregation_levels == "High" & dest_opp_levels == "High" ~ "Grey", 
  #                           origin_segregation_levels == "High" & dest_opp_levels == "Medium" ~ "Red", 
  #                           origin_segregation_levels == "High" & dest_opp_levels == "Low" ~ "Red")) %>% 
  as.data.frame() %>% 
  ggplot(aes(y = total_flows, 
             axis1 = origin_segregation_levels, 
             axis2 = dest_opp_levels)) + 
  #geom_alluvium(aes(fill = factor(color)), width = 1/12) + 
  geom_alluvium(aes(fill = fill), width = 1/12) + 
  geom_stratum(width = 1/12, fill = "black", color = "grey") + 
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) + 
  scale_x_discrete(limits = c("Origin Segregation", "Destination Opportunity"), expand = c(0.05, 0.05)) + 
  theme_minimal() + 
  scale_fill_identity() + 
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_text(size = 12)) 

ggsave(plot = seg_opp_alluvium, 
       filename = paste0(outdir, "plot2_seg_opp_counts.png"), 
       height = 6, 
       width = 10)


# table 2 - seg x seg ------------------------------------------------------
seg_by_seg_alluvium <- plot_data %>% 
  group_by(origin_segregation_levels, dest_seg_levels) %>% 
  summarize(total_flows= sum(n_black)) %>% 
  group_by(origin_segregation_levels) %>% 
  mutate(sum_origin_flows = sum(total_flows)) %>% 
  ungroup() %>% 
  filter(!is.na(dest_seg_levels)) %>% 
#  write_csv(file = paste0(outdir, "seg_to_seg.csv"))
#%>%
  mutate(perc_flow = total_flows/sum_origin_flows) %>%
  group_by(origin_segregation_levels) %>%
  arrange(desc(perc_flow), origin_segregation_levels) %>%
  mutate(row_num = row_number(),
         top_flow = case_when(row_num == 1 ~ 1, TRUE ~ 0),
         fill = case_when(top_flow == 1 ~ "#0B58C7",
                          TRUE ~ "#8A96A7")) %>%
  ungroup() %>%
  as.data.frame() %>%
  ggplot(aes(y = total_flows,
             axis1 = origin_segregation_levels,
             axis2 = dest_seg_levels)) +
  geom_alluvium(aes(fill = fill), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Origin Segregation", "Destination Segregation"), expand = c(0.05, 0.05)) +
  theme_minimal() +
  scale_fill_identity() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12))

ggsave(plot = seg_by_seg_alluvium, 
       filename = paste0(outdir, "plot2_seg_seg_counts.png"), 
       height = 6, 
       width = 10)



# plot 2 - opp x opp ------------------------------------------------------
opp_by_opp <- plot_data %>% 
  group_by(origin_opp_levels, dest_opp_levels) %>% 
  summarize(total_flows = sum(n_black)) %>% 
  group_by(origin_opp_levels) %>% 
  mutate(sum_origin_flows = sum(total_flows)) %>% 
  ungroup() %>% 
  filter(!is.na(origin_opp_levels), 
         !is.na(dest_opp_levels)) %>% 
  select(-sum_origin_flows)

write_csv(opp_by_opp, file = paste0(outdir, "opp_to_opp.csv")) 
opp_opp_alluvium <- opp_by_opp %>% 
  group_by(origin_opp_levels) %>% 
  arrange(desc(total_flows), origin_opp_levels) %>% 
  mutate(row_num = row_number(), 
         top_flow = case_when(row_num == 1 ~ 1, TRUE ~ 0), 
         fill = case_when(top_flow == 1 ~ "#0B58C7", 
                          TRUE ~ "#8A96A7")) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  ggplot(aes(y = total_flows, 
             axis1 = origin_opp_levels, 
             axis2 = dest_opp_levels)) + 
  #geom_alluvium(aes(fill = factor(color)), width = 1/12) + 
  geom_alluvium(aes(fill = fill), width = 1/12) + 
  geom_stratum(width = 1/12, fill = "black", color = "grey") + 
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) + 
  scale_x_discrete(limits = c("Origin Opportunity", "Destination Opportunity"), expand = c(0.05, 0.05)) + 
  theme_minimal() + 
  scale_fill_identity() + 
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_text(size = 12)) 

ggsave(plot = opp_opp_alluvium, 
       filename = paste0(outdir, "plot2_opp_opp_counts.png"), 
       height = 6, 
       width = 10)


# plot 3 - outflows ------------------------------------------------------------------
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


state_lines <- tigris::states(cb = TRUE, resolution = "20m", year = 1990) %>% 
  filter(NAME %in% states()) %>% 
  tigris::shift_geometry()

spat_dat <- sf::st_read(shapefile) %>% 
  inner_join(spat_dat, by = c("cz" = "o_cz")) %>% 
  tigris::shift_geometry() #%>% 
  #filter(! o_state_name_black %in% c("Alaska", "Hawaii"))

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
  ) #+ 
  #ggplot2::labs(
  #  title = "Migration of Young Adults and Segregation in the U.S.",
  #  subtitle = "Percent of outflows and White-Black dissimilarity index by Commuting Zone", 
  #  caption = 
  #    "Percent of outflows represented as the number of people moving out of a given commuting zone divided by the number of people who live in the commuting zone. 
  #      Percent of outflows calculated using migration data from Opportunity Insights and population estimates from the Decennial Census. Note that the migration 
  #      patterns are restricted to adults younger than 26. The black-white dissimilarity index was constructed using population estimates primarily from the 2010 
  #      Decennial Census and supplemented with the 2000 Decennial Census where necessary. Quantile breaks were used to determine the color ramp for both layers of data. 
  #      Map projection used: USA Contiguous Albers Equal Area Conic. Commuting Zone shapefile from the Health Inequality Project."
  #  
  #)

segregation_and_migration <- cowplot::ggdraw() +
  cowplot::draw_plot(segregation_migration_map, 0, 0, 1, 1) +
  cowplot::draw_plot(legend, .83, .03, .2, .2)

ggsave(plot = segregation_and_migration, 
       filename = paste0(outdir, "plot3_segoutflows.png"), 
       height = 10, 
       width = 14)


# plot 3 test - outflows ------------------------------------------------------------------
# Final tweaks to data 
final_data <- data %>% 
  filter(o_cz == d_cz, 
         n_black != 0) %>% 
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


state_lines <- tigris::states(cb = TRUE, resolution = "20m", year = 1990) %>% 
  filter(NAME %in% states()) %>% 
  tigris::shift_geometry()

spat_dat <- sf::st_read(shapefile) %>% 
  inner_join(spat_dat, by = c("cz" = "o_cz")) %>% 
  tigris::shift_geometry() #%>% 
#filter(! o_state_name_black %in% c("Alaska", "Hawaii"))

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
  ) #+ 
#ggplot2::labs(
#  title = "Migration of Young Adults and Segregation in the U.S.",
#  subtitle = "Percent of outflows and White-Black dissimilarity index by Commuting Zone", 
#  caption = 
#    "Percent of outflows represented as the number of people moving out of a given commuting zone divided by the number of people who live in the commuting zone. 
#      Percent of outflows calculated using migration data from Opportunity Insights and population estimates from the Decennial Census. Note that the migration 
#      patterns are restricted to adults younger than 26. The black-white dissimilarity index was constructed using population estimates primarily from the 2010 
#      Decennial Census and supplemented with the 2000 Decennial Census where necessary. Quantile breaks were used to determine the color ramp for both layers of data. 
#      Map projection used: USA Contiguous Albers Equal Area Conic. Commuting Zone shapefile from the Health Inequality Project."
#  
#)

segregation_and_migration <- cowplot::ggdraw() +
  cowplot::draw_plot(segregation_migration_map, 0, 0, 1, 1) +
  cowplot::draw_plot(legend, .83, .03, .2, .2)

ggsave(plot = segregation_and_migration, 
       filename = paste0(outdir, "plot3_segoutflows_restricted.png"), 
       height = 10, 
       width = 14)


# plot 4 - inflows  ------------------------------------------------------------------
## Generating inflows 
#Final tweaks to data 
final_data <- data %>% 
  filter(o_cz == d_cz) %>% 
  mutate(inflows = n_tot_d_black - n_black, 
         perc_inflows = inflows/n_tot_d_black, 
         perc_inflows = case_when(is.na(perc_inflows) ~ 0, 
                                  is.infinite(perc_inflows) ~ 0, # result of random noise being added to publicly available dataset
                                  perc_inflows < 0 ~ 0,          # result of random noise 
                                  TRUE ~ perc_inflows))  


# Create quantiles for dissimilarity
seg_quantiles <- quantile(
  final_data$d_diswbcz, 
  seq(0, 1, length.out = 5)
)

inflow_quantiles <- quantile(
  final_data$perc_inflows, 
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
      d_diswbcz,
      breaks = seg_quantiles,
      include.lowest = TRUE
    ),
    #        absolute_mobility = forcats::fct_rev(absolute_mobility),
    inflows = cut(
      perc_inflows,
      breaks = inflow_quantiles,
      include.lowest = TRUE
    ),
    group = paste0(as.numeric(segregation), "-", as.numeric(inflows))
  ) |>
  dplyr::left_join(bivariate_color_scale, by = "group") 

## Map prep 
font_fam <- "Trebuchet MS"
bivariate_color_scale2 <- bivariate_color_scale |>
  tidyr::separate(group, into = c("segregation", "inflows"), sep = "-") |>
  dplyr::mutate(
    segregation = as.integer(segregation),
    outflow = as.integer(inflows)
  )

legend <- ggplot2::ggplot() +
  ggplot2::geom_tile(
    data = bivariate_color_scale2,
    mapping = ggplot2::aes(
      x = segregation,
      y = inflows,
      fill = fill
    )
  ) +
  ggplot2::scale_fill_identity() +
  ggplot2::labs(
    x = "  Higher segregation \U00BB",
    y = "  Higher inflow \U00BB"
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
  filter(NAME %in% states()) %>% 
  tigris::shift_geometry()

spat_dat <- sf::st_read(shapefile) %>% 
  inner_join(spat_dat, by = c("cz" = "o_cz")) %>% 
  tigris::shift_geometry() #%>% 
#filter(! o_state_name_black %in% c("Alaska", "Hawaii"))

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
  ) #+ 
#ggplot2::labs(
#  title = "Migration of Young Adults and Segregation in the U.S.",
#  subtitle = "Percent of outflows and White-Black dissimilarity index by Commuting Zone", 
#  caption = 
#    "Percent of outflows represented as the number of people moving out of a given commuting zone divided by the number of people who live in the commuting zone. 
#      Percent of outflows calculated using migration data from Opportunity Insights and population estimates from the Decennial Census. Note that the migration 
#      patterns are restricted to adults younger than 26. The black-white dissimilarity index was constructed using population estimates primarily from the 2010 
#      Decennial Census and supplemented with the 2000 Decennial Census where necessary. Quantile breaks were used to determine the color ramp for both layers of data. 
#      Map projection used: USA Contiguous Albers Equal Area Conic. Commuting Zone shapefile from the Health Inequality Project."
#  
#)

segregation_and_migration <- cowplot::ggdraw() +
  cowplot::draw_plot(segregation_migration_map, 0, 0, 1, 1) +
  cowplot::draw_plot(legend, .83, .03, .2, .2)

ggsave(plot = segregation_and_migration, 
       filename = paste0(outdir, "plot4_seginflows.png"), 
       height = 10, 
       width = 14)



# plot 5 - net flows ------------------------------------------------------------------
final_data <- data %>% 
  filter(o_cz == d_cz) %>% 
  mutate(inflows = n_tot_d_black - n_tot_o_black, 
         perc_inflows = 1 + inflows/n_tot_o_black, 
         perc_inflows = case_when(is.na(perc_inflows) ~ 0, 
                                  is.infinite(perc_inflows) ~ 0, # result of random noise being added to publicly available dataset
                                  perc_inflows < 0 ~ 0,          # result of random noise 
                                  TRUE ~ perc_inflows))  


# Create quantiles for dissimilarity
seg_quantiles <- quantile(
  final_data$o_diswbcz, 
  seq(0, 1, length.out = 5)
)

inflow_quantiles <- quantile(
  final_data$perc_inflows, 
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
    inflows = cut(
      perc_inflows,
      breaks = inflow_quantiles,
      include.lowest = TRUE
    ),
    group = paste0(as.numeric(segregation), "-", as.numeric(inflows))
  ) |>
  dplyr::left_join(bivariate_color_scale, by = "group") 

## Map prep 
font_fam <- "Trebuchet MS"
bivariate_color_scale2 <- bivariate_color_scale |>
  tidyr::separate(group, into = c("segregation", "inflows"), sep = "-") |>
  dplyr::mutate(
    segregation = as.integer(segregation),
    outflow = as.integer(inflows)
  )

legend <- ggplot2::ggplot() +
  ggplot2::geom_tile(
    data = bivariate_color_scale2,
    mapping = ggplot2::aes(
      x = segregation,
      y = inflows,
      fill = fill
    )
  ) +
  ggplot2::scale_fill_identity() +
  ggplot2::labs(
    x = "  Higher segregation \U00BB",
    y = "  Higher net flows \U00BB"
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
  filter(NAME %in% states()) %>% 
  tigris::shift_geometry()

spat_dat <- sf::st_read(shapefile) %>% 
  inner_join(spat_dat, by = c("cz" = "o_cz")) %>% 
  tigris::shift_geometry() #%>% 
#filter(! o_state_name_black %in% c("Alaska", "Hawaii"))

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
  ) #+ 
#ggplot2::labs(
#  title = "Migration of Young Adults and Segregation in the U.S.",
#  subtitle = "Percent of outflows and White-Black dissimilarity index by Commuting Zone", 
#  caption = 
#    "Percent of outflows represented as the number of people moving out of a given commuting zone divided by the number of people who live in the commuting zone. 
#      Percent of outflows calculated using migration data from Opportunity Insights and population estimates from the Decennial Census. Note that the migration 
#      patterns are restricted to adults younger than 26. The black-white dissimilarity index was constructed using population estimates primarily from the 2010 
#      Decennial Census and supplemented with the 2000 Decennial Census where necessary. Quantile breaks were used to determine the color ramp for both layers of data. 
#      Map projection used: USA Contiguous Albers Equal Area Conic. Commuting Zone shapefile from the Health Inequality Project."
#  
#)

segregation_and_migration <- cowplot::ggdraw() +
  cowplot::draw_plot(segregation_migration_map, 0, 0, 1, 1) +
  cowplot::draw_plot(legend, .83, .03, .2, .2)

ggsave(plot = segregation_and_migration, 
       filename = paste0(outdir, "plot5_seg_netflows.png"), 
       height = 10, 
       width = 14)



# plot 6 - schools included -----------------------------------------------
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

state_lines <- tigris::states(cb = TRUE, resolution = "20m", year = 1990) %>% 
  filter(NAME %in% states()) %>% 
  tigris::shift_geometry()

spat_dat <- sf::st_read(shapefile) %>% 
  inner_join(spat_dat, by = c("cz" = "o_cz")) %>% 
  tigris::shift_geometry() #%>% 
#filter(! o_state_name_black %in% c("Alaska", "Hawaii"))

schools <- read_csv(schools_fp) %>% 
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
    data = tigris::shift_geometry(sf::st_transform(schools %>% filter(high_mobility == 1), sf::st_crs(spat_dat))), 
    alpha = 0.5, 
    size = 0.75, 
    color = "green", 
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
  ) #+ 
#ggplot2::labs(
#  title = "Migration of Young Adults and Segregation in the U.S.",
#  subtitle = "Percent of outflows and White-Black dissimilarity index by Commuting Zone", 
#  caption = 
#    "Percent of outflows represented as the number of people moving out of a given commuting zone divided by the number of people who live in the commuting zone. 
#      Percent of outflows calculated using migration data from Opportunity Insights and population estimates from the Decennial Census. Note that the migration 
#      patterns are restricted to adults younger than 26. The black-white dissimilarity index was constructed using population estimates primarily from the 2010 
#      Decennial Census and supplemented with the 2000 Decennial Census where necessary. Quantile breaks were used to determine the color ramp for both layers of data. 
#      Map projection used: USA Contiguous Albers Equal Area Conic. Commuting Zone shapefile from the Health Inequality Project."
#  
#)

segregation_and_migration <- cowplot::ggdraw() +
  cowplot::draw_plot(segregation_migration_map, 0, 0, 1, 1) +
  cowplot::draw_plot(legend, .83, .03, .2, .2)

ggsave(plot = segregation_and_migration, 
       filename = paste0(outdir, "plot6_segoutflows_schools.png"), 
       height = 10, 
       width = 14)

