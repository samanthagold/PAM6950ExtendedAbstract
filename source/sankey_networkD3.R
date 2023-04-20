# sankey diagram using networkD3 package - does not work... 
library(tidyverse)
library(networkD3)

wd <- "/Users/sammygold/Documents/PAM6950ExtendedAbstract/"

xwalk <- read_csv(paste0(wd, "data/xwalk_regionsdivs.csv")) %>% 
    select(State, Region, Division)

migration <- read_csv(paste0(wd, "data/od_pooled.csv")) %>%
    inner_join(xwalk, by = c("o_state_name" = "State")) %>% 
    rename(O_region = Region, 
           O_division = Division) %>% 
    inner_join(xwalk, by = c("d_state_name" = "State")) %>% 
    rename(D_region = Region, 
           D_division = Division) %>% 
    group_by(O_region, D_region) %>% 
    summarize(total_flows = sum(n)) 


# Data Prep ---------------------------------------------------------------
nodes <- c(migration %>% pull(O_region), 
           migration %>% pull(D_region)) %>% 
    unique() %>% 
    as_tibble() %>% 
    mutate(id = row_number())

forsankey <- migration %>% 
    inner_join(nodes, by = c("O_region" = "value")) %>% 
    rename(id_origin = id) %>% 
    inner_join(nodes, by = c("D_region" = "value")) %>% 
    rename(id_dest = id) 

sankeyNetwork(Links = forsankey, 
              Nodes = nodes, 
              Source = "id_origin", 
              Target = "id_dest", 
              Value = "total_flows", 
              NodeID = "value",
              sinksRight = FALSE
              )


