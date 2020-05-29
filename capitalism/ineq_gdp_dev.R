library(tidyverse)

inGDP2 <- read_excel("capitalism/ineq_growth_dev.xlsx")

Devplot <- 
  inGDP2 %>%
  ggplot(aes(x = avgini, y = avgdpgrowth)) +
  geom_point(color = COLA[4], size = 3) +
  geom_text_repel(aes(label = country), size = 5) + 
  theme_bw() + 
  ylab("Average GDP per capita growth, 1980-2012, %") + 
  xlab("Long-term average inequality in disposable income") + 
  scale_x_continuous(expand = c(0, .05)) + 
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 21),
        axis.text = element_text(size = 18))
#scale_y_continuous(expand = c(0, ))
Devplot

pdf(file = "capitalism/ineq_gdp_dev.pdf", width = 8, height = 5)
Devplot
dev.off()