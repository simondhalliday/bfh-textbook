#' Scott Cohn
#' Bowles and Halliday
#' DLR Map replication

library(tidyverse)
library(dplyr)
library(ggplot2)
library(maps)
library(mapproj)
library(cowplot)
library(socviz)

dlr_c1 <- read_csv("Documents/UMass/RA/Dropbox/DLR_replication/DLR replication files/Dofiles/dlr_c1.csv")
grays <- gray.colors(25, start = 1, end = 0)

'%!in%' <- function(x,y)!('%in%'(x,y))

dlr_mapvar <- dlr_c1 %>% 
  mutate(incl_fips = ifelse(dlr_c1$mapvar == 11 | dlr_c1$mapvar == 1, dlr_c1$countyfipsnum, NA)) %>%
  select(incl_fips)

dlr_mapvar <- dplyr::pull(dlr_mapvar, incl_fips)


county_full <- left_join(county_map, county_data, by = "id") %>%
  filter(state %!in% c("AK", "HI"))

p0 <- county_full %>%
  mutate(dlr_mapvar = fips %in% dlr_mapvar) %>% 
  ggplot(mapping = aes(x = long, y = lat, fill = dlr_mapvar, group = group)) 

p1 <- p0 + geom_polygon(color = grays[20], size = 0.1) + 
  coord_equal()
  #coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
  #coord_map(projection = "gilbert") +
  # coord_map() 

p2 <- p1 + scale_fill_manual(values = c("TRUE" = "#1F78B4", "FALSE" = "white"), 
                    labels = c("Other counties", "Contiguous border county pairs (with minimum wage difference)")) 

p2 + guides(fill = guide_legend(nrow = 2)) +
  theme_map() +
  theme(legend.position = "top", legend.title = element_blank())
  
ggsave("DLR_C1_Replication.pdf", width = 9, height = 7, units = "in")


