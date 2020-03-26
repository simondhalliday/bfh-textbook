library(ggplot2)
library(readr)

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

homescan_sugar_bev <- read_csv("firmmarketsupply/homescan_sugar_bev.csv")
head(homescan_sugar_bev)

# round values
homescan_sugar_bev <- homescan_sugar_bev %>% 
  mutate_if(is.numeric, ~round(., 0))

sug_bev <- ggplot(homescan_sugar_bev, aes(x = X, y = Y)) +
  geom_point(col = COLA[5]) + 
  geom_line(col = COLA[5]) +
  #geom_bar(stat = "identity") +
  scale_x_continuous(labels = homescan_sugar_bev[["X"]], breaks = homescan_sugar_bev[["X"]]) +
  labs(x = "Household Income ($000s)", 
       y = "Liters purchased per adult equivalent per year") + 
  theme_bw() + 
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 19),
        panel.grid.minor = element_blank())

ggsave("firmmarketsupply/sugar_tax_data.pdf", width = 9, height = 7)