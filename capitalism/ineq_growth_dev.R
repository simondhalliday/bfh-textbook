library(tidyverse)
library(ggrepel)

inGDP2 <- read_excel("capitalism/ineq_growth_dev.xlsx")

p <- inGDP2 %>%
  ggplot(aes(x = avgini, y = avgdpgrowth)) +
  geom_point(color = "#377EB8", size = 3) +
  geom_text_repel(aes(label = country), size = 5) + 
  labs(x = "Long-term average inequality in disposable income",
       y = "Average GDP per capita growth, 1980-2012, %") +
  scale_x_continuous(expand = c(0, .05)) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 19),
        axis.text = element_text(size = 17))


ggsave("capitalism/ineq_gdp_dev.pdf", width = 9, height = 7, units = "in")


