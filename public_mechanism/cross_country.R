library(ggplot2)
library(readxl)
library(dplyr)
library(shape)

CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442","#D55E00")

Data <- read_excel("public_mechanism/cross country figure data.xls")

Data <- Data %>%
  mutate(country  = na_if(country, "Switzerland")) %>%
  mutate(country = na_if(country, "Germany")) %>%
  mutate(country = na_if(country, "Finland")) %>%
  mutate(country = na_if(country, "Netherlands")) %>%
  mutate(country = na_if(country, "Denmark"))

Data <- 
  Data %>%
  mutate(highlight = if_else(codecountry == "US", 1, 0))

Plot <- ggplot(Data, aes(x = `gini post tax and transfer`, y = `protective services employees per 10000 workers in the labor force`)) + 
  geom_point(aes(group = country, color = factor(highlight)), size = 3) + 
  geom_text(aes(label = country), hjust = 0, vjust = -0.4, size =5.5) + 
  scale_x_continuous(limits = c(.2,.4)) + 
  scale_color_manual(values = c(CBCols[1], CBCols[2])) +
  theme_bw() + 
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
              axis.title = element_text(size = 19),
              axis.text = element_text(size = 19)) + 
  annotate("text", x = 0.285, y = 66.8, label = "Switzerland", size =5.5 ) + 
  annotate("text", x = 0.293, y = 87, label = "Germany", size =5.5 ) + 
  annotate("text", x = 0.25, y = 77, label = "Finland", size =5.5) +
  annotate("text", x = 0.245, y = 68, label = "Netherlands", size =5.5) + 
  annotate("text", x = 0.213, y = 62, label = "Denmark", size =5.5) +
  annotate("text", x = 0.393, y = 202, label = "(2000)", size =5.5) +
  ylab("Guards per  10,000 people \n in labor force in 2000") + 
  xlab("Inequality in disposable income (Gini coefficient)") + 
  geom_segment(
    aes(x = 0.355, y = 174, xend = 0.371, yend = 202.5),
    data = Data,
    arrow = arrow(length = unit(0.025, "npc"), 
                  type = "closed")
  )
  
  
Plot

ggsave(plot = Plot, "public_mechanism/cross_country.pdf", width = 9, height = 7, units = "in")
