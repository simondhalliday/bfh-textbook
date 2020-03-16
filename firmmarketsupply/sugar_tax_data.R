library(ggplot2)
library(readr)

homescan_sugar_bev <- read_csv("firmmarketsupply/homescan_sugar_bev.csv")
head(homescan_sugar_bev)

sug_bev <- ggplot(homescan_sugar_bev, aes(x = X, y = Y)) +
  geom_point(col = "#005824") + 
  geom_line(col = "#238b45") +
  #geom_bar(stat = "identity") + 
  labs(x = "Household Income ($000s)", 
       y = "Liters purchased per adult equivalent per year") + 
  theme_bw() + 
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 19))

ggsave("firmmarketsupply/sugar_tax_data.pdf", width = 9, height = 7)