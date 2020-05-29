library(tidyverse)
library(ggrepel)

inGDP <- read_excel("capitalism/ineq_growth.xlsx")

p <- 
  inGDP %>%
  #filter(country != "Australia") %>%
  ggplot(aes(x = avgini, y = avgdpgrowth)) +
  geom_point(color = "#4DAF4A", size = 3) +
  geom_text_repel(aes(label = country), size = 5) + 
  labs(x = "Long-term average in equality in disposable income",
       y = "Average GDP per capita growth, 1970-2012, %") +
  scale_x_continuous(expand = c(0, .01)) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 21),
        axis.text = element_text(size = 18))

ggsave(file = "capitalism/ineq_gdp_oecd.pdf", width = 9, height = 7, units = "in")
