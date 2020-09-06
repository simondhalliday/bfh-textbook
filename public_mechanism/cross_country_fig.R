library(ggplot2)
library(readxl)
library(dplyr)

CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442","#D55E00")

Data <- read_excel("public_mechanism/cross country figure data.xls")

Data <- Data %>%
  mutate(country  = na_if(country, "Switzerland")) %>%
  mutate(country = na_if(country, "Germany")) %>%
  mutate(country = na_if(country, "Finland")) %>%
  mutate(country = na_if(country, "Netherlands")) %>%
  mutate(country = na_if(country, "Denmark"))



Plot <- ggplot(Data, aes(x = `gini post tax and transfer`, y = `protective services employees per 10000 workers in the labor force`)) + 
  geom_point(aes(group = country), color  = CBCols[1], size = 2) + 
  geom_text(aes(label = country), hjust = 0, vjust = -0.4) + 
  scale_x_continuous(limits = c(.2,.4)) + 
  theme_bw() + 
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
              axis.title = element_text(size = 15),
              axis.text = element_text(size = 15)) + 
  annotate("text", x = 0.282, y = 66.8, label = "Switzerland") + 
  annotate("text", x = 0.29, y = 87, label = "Germany") + 
  annotate("text", x = 0.25, y = 77, label = "Finland") +
  annotate("text", x = 0.249, y = 68, label = "Netherlands") + 
  annotate("text", x = 0.217, y = 62, label = "Denmark") +
  xlab("Protective service employees per  10,000 people in labor force in 2000") + 
  ylab("Income inequality among households in the early 2000s")
  
  
Plot

ggsave(plot = Plot, "public_mechanism/cross_country.pdf", width = 8, height = 6, units = "in")
