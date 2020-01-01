library(ggplot2)
library(readr)

homescan_sugar_bev <- read_csv("indmarketdemand/homescan_sugar_bev.csv")
head(homescan_sugar_bev)

sug_bev <- ggplot(homescan_sugar_bev, aes(x = X, y = Y)) +
          geom_point() + 
          labs(x = "Household Income ($000s)", 
               y = "Liters purchased per adult equivalent per year") + 
          theme_bw()

ggsave("indmarketdemand/homescan_sugar_bev.pdf", width = 9, height = 7)