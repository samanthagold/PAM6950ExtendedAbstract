
# Goal --------------------------------------------------------------------
# purpose: plot flows of origin to destination 
# remaining to-dos: Need to fix data so that it is proportions of the area 
# instead of raw numbers. 
# -------------------------------------------------------------------------

options(scipen = 999)
library(tidyverse)
library(ggalluvial)


wd <- "/Users/sammygold/Documents/PAM6950ExtendedAbstract/"

xwalk <- read_csv(paste0(wd, "data/xwalk_regionsdivs.csv")) %>% 
    select(State, Region, Division)

migration <- read_csv(paste0(wd, "data/od_pooled.csv")) 


# Plotting  ---------------------------------------------------------------

# Divisions
migration %>%
    inner_join(xwalk, by = c("o_state_name" = "State")) %>% 
    rename(O_region = Region, 
           O_division = Division) %>% 
    inner_join(xwalk, by = c("d_state_name" = "State")) %>% 
    rename(D_region = Region, 
           D_division = Division) %>% 
    group_by(O_division, D_division) %>% 
    summarize(total_flows = sum(n)) %>% 
    as.data.frame() %>% 
    ggplot(aes(y = total_flows, 
                  axis1 = O_division, 
                  axis2 = D_division)) + 
    geom_alluvium(aes(fill = O_division), width = 1/12) + 
    geom_stratum(width = 1/12, alpha = 0.25, reverse = FALSE) + 
    geom_label(stat = "stratum", aes(label = after_stat(stratum))) + 
    scale_x_discrete(limits = c("Origin", "Destination"), expand = c(0.05, 0.05))


# Regions
migration %>% 
    inner_join(xwalk, by = c("o_state_name" = "State")) %>% 
    rename(O_region = Region, 
           O_division = Division) %>% 
    inner_join(xwalk, by = c("d_state_name" = "State")) %>% 
    rename(D_region = Region, 
           D_division = Division) %>% 
    group_by(O_region, D_region) %>% 
    summarize(total_flows = sum(n)) %>% 
    as.data.frame() %>% 
    ggplot(aes(y = total_flows, 
           axis1 = O_region, 
           axis2 = D_region)) + 
    geom_alluvium(aes(fill = O_region), width = 1/12) + 
    geom_stratum(width = 1/12, fill = "black", color = "grey") + 
    geom_label(stat = "stratum", aes(label = after_stat(stratum))) + 
    scale_x_discrete(limits = c("Origin", "Destination"), expand = c(0.05, 0.05)) + 
    theme_minimal()


# regions - width determined by flows 
migration %>% 
    inner_join(xwalk, by = c("o_state_name" = "State")) %>% 
    rename(O_region = Region, 
           O_division = Division) %>% 
    inner_join(xwalk, by = c("d_state_name" = "State")) %>% 
    rename(D_region = Region, 
           D_division = Division) %>% 
    group_by(O_region, D_region) %>% 
    summarize(total_flows = sum(n)) %>% 
    as.data.frame() %>% 
    ggplot(aes(y = total_flows, 
               axis1 = O_region, 
               axis2 = D_region)) + 
    geom_alluvium(aes(fill = O_region), width = 1/12) + 
    geom_stratum(width = 1/12, fill = "black", color = "grey") + 
    geom_label(stat = "stratum", aes(label = after_stat(stratum))) + 
    scale_x_discrete(limits = c("Origin", "Destination"), expand = c(0.05, 0.05)) + 
    theme_minimal()

# regions - removing same region 
migration %>% 
    inner_join(xwalk, by = c("o_state_name" = "State")) %>% 
    rename(O_region = Region, 
           O_division = Division) %>% 
    inner_join(xwalk, by = c("d_state_name" = "State")) %>% 
    rename(D_region = Region, 
           D_division = Division) %>% 
    group_by(O_region, D_region) %>% 
    summarize(total_flows = sum(n)) %>% 
    filter(O_region != D_region) %>% 
    as.data.frame() %>% 
    ggplot(aes(y = total_flows, 
               axis1 = O_region, 
               axis2 = D_region)) + 
    geom_alluvium(aes(fill = O_region), width = 1/12) + 
    geom_stratum(width = 1/12, fill = "black", color = "grey") + 
    geom_label(stat = "stratum", aes(label = after_stat(stratum))) + 
    scale_x_discrete(limits = c("Origin", "Destination"), expand = c(0.05, 0.05)) + 
    theme_minimal()


# regions - coloring by 1, 2, and 3. 
migration %>% 
    inner_join(xwalk, by = c("o_state_name" = "State")) %>% 
    rename(O_region = Region, 
           O_division = Division) %>% 
    inner_join(xwalk, by = c("d_state_name" = "State")) %>% 
    rename(D_region = Region, 
           D_division = Division) %>% 
    group_by(O_region, D_region) %>% 
    summarize(total_flows = sum(n)) %>% 
    filter(O_region != D_region) %>% 
    group_by(O_region) %>% 
    arrange(O_region, desc(total_flows)) %>% 
    mutate(order = as.character(row_number()))  %>% 
    ggplot(aes(y = total_flows, 
               axis1 = O_region, 
               axis2 = D_region)) + 
    geom_alluvium(aes(fill = order), width = 1/12) + 
    geom_stratum(width = 1/12, fill = "black", color = "grey") + 
    geom_label(stat = "stratum", aes(label = after_stat(stratum))) + 
    scale_x_discrete(limits = c("Origin", "Destination"), expand = c(0.05, 0.05)) + 
    theme_minimal()

## the story here: of the people that are moving away from their origin destination, 
## a lot of them are moving to the South. The South is the number 1 destination 
## for a lot of these young people. Moving out West is the second most desirable, 
## and not a lot are moving to the Northeast. NOTE: not normalized, probably want to 
## do normalized instead because South probably just has the largest population. Not
## necessary for now because you are just experimenting. 


# regions - width NOT determined by total flow (excluding y argument in ggplot)
migration %>% 
    inner_join(xwalk, by = c("o_state_name" = "State")) %>% 
    rename(O_region = Region, 
           O_division = Division) %>% 
    inner_join(xwalk, by = c("d_state_name" = "State")) %>% 
    rename(D_region = Region, 
           D_division = Division) %>% 
    group_by(O_region, D_region) %>% 
    summarize(total_flows = sum(n)) %>% 
    as.data.frame() %>% 
    ggplot(aes(axis1 = O_region, 
               axis2 = D_region)) + 
    geom_alluvium(aes(fill = total_flows)) + 
    geom_stratum(width = 1/12, fill = "black", color = "grey") + 
    geom_label(stat = "stratum", aes(label = after_stat(stratum))) + 
    scale_x_discrete(limits = c("Origin", "Destination"), expand = c(0.05, 0.05)) + 
    theme_minimal()

# trying again with divisions, removing within-division flows
migration %>%
    inner_join(xwalk, by = c("o_state_name" = "State")) %>% 
    rename(O_region = Region, 
           O_division = Division) %>% 
    inner_join(xwalk, by = c("d_state_name" = "State")) %>% 
    rename(D_region = Region, 
           D_division = Division) %>% 
    group_by(O_division, D_division) %>% 
    summarize(total_flows = sum(n)) %>% 
    filter(O_division != D_division) %>% 
    as.data.frame() %>% 
    ggplot(aes(y = total_flows, 
               axis1 = O_division, 
               axis2 = D_division)) + 
    geom_alluvium(aes(fill = O_division), width = 1/12) + 
    geom_stratum(width = 1/12, alpha = 0.25, reverse = TRUE) + 
    geom_label(stat = "stratum", aes(label = after_stat(stratum))) + 
    scale_x_discrete(limits = c("Origin", "Destination"), expand = c(0.05, 0.05))
## Way harder to parse.... need to find a way to illuminate the top destinations 

# divisions - with within-region flows removed and coloring by ordering 
migration %>%
    inner_join(xwalk, by = c("o_state_name" = "State")) %>% 
    rename(O_region = Region, 
           O_division = Division) %>% 
    inner_join(xwalk, by = c("d_state_name" = "State")) %>% 
    rename(D_region = Region, 
           D_division = Division) %>% 
    group_by(O_division, D_division) %>% 
    summarize(total_flows = sum(n)) %>% 
    filter(O_division != D_division) %>% 
    group_by(O_division) %>% 
    arrange(O_division, desc(total_flows)) %>% 
    mutate(order = as.character(row_number())) %>%
    as.data.frame() %>% 
    ggplot(aes(y = total_flows, 
               axis1 = O_division, 
               axis2 = D_division)) + 
    geom_alluvium(aes(fill = order), width = 1/12) + 
    geom_stratum(width = 1/12, alpha = 0.25, reverse = TRUE) + 
    geom_label(stat = "stratum", aes(label = after_stat(stratum))) + 
    scale_x_discrete(limits = c("Origin", "Destination"), expand = c(0.05, 0.05))

# divisions: figuring out filling for top origin 
migration %>%
    inner_join(xwalk, by = c("o_state_name" = "State")) %>% 
    rename(O_region = Region, 
           O_division = Division) %>% 
    inner_join(xwalk, by = c("d_state_name" = "State")) %>% 
    rename(D_region = Region, 
           D_division = Division) %>% 
    group_by(O_division, D_division) %>% 
    summarize(total_flows = sum(n)) %>% 
    filter(O_division != D_division) %>% 
    group_by(O_division) %>% 
    arrange(O_division, desc(total_flows)) %>% 
    mutate(order = as.character(row_number()), 
           fill = case_when(order == 1 ~ "#0B58C7", 
                            TRUE ~ "#8A96A7")) %>% 
    ggplot(aes(y = total_flows, 
               axis1 = O_division, 
               axis2 = D_division, 
               fill = fill)) + 
    geom_alluvium(width = 1/12) + 
    geom_stratum(width = 1/12, alpha = 0.25, reverse = TRUE) + 
    geom_label(stat = "stratum", aes(label = after_stat(stratum))) + 
    scale_x_discrete(limits = c("Origin", "Destination"), expand = c(0.05, 0.05)) + 
    scale_fill_identity()

# Trying all 50 states now 
migration %>% 
    group_by(o_state_name, d_state_name) %>% 
    summarize(total_flows = sum(n)) %>% 
    filter(o_state_name != d_state_name) %>% 
    ggplot(aes(y = total_flows, 
               axis1 = o_state_name, 
               axis2 = d_state_name)) + 
    geom_alluvium(width = 1/12) + 
    geom_stratum(width = 1/12, alpha = 0.25, reverse = TRUE) + 
    geom_label(stat = "stratum", aes(label = after_stat(stratum))) + 
    scale_x_discrete(limits = c("Origin", "Destination"), expand = c(0.05, 0.05)) + 
    scale_fill_identity()

    






