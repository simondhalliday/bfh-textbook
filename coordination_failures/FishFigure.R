require(shape)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(readxl)

fish_data <- read_excel("FishData.xlsx")

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824", "#f0027f")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081", "#9e9ac8","#f0027f")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
COLD <- c("#DA3030","#41ae76","#F7DE04", "#4eb3d3","#AE82FF","#386cb0","#F48318","#41ae76","#6a51a3", "#DA3030")

fish_data_2 <- fish_data %>%
  filter(Year >= 1950)


#Plotting the data 
Fish_plot <- ggplot(fish_data, aes(x = Year, y = Tonnes)) +
  geom_line() +
  #geom_line(aes(linetype = Country)) + #if we differentiate the lines without color
  labs(y = "", color = "") + 
  scale_y_continuous(breaks = seq(0, 600000, by = 100000), limits = c(0,600000)) +
  scale_x_continuous(breaks = seq(1850, 2010, by = 10)) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() 
  #theme(panel.grid.minor = element_blank(),
        # legend.position = c(0.88, 0.83), 
        # legend.title = element_text(size = 15), 
        # axis.title.y = element_text(size = 17, vjust = 1),
        # legend.text = element_text(size = 14),
        # axis.text.x = element_text(size = 14, color = "black"),
        # axis.text.y = element_text(size = 14, color = "black"),  
        # axis.title.x = element_text(size = 17, vjust = -1)) 
        # 
Fish_plot


#print(wealth_share_plot)

#Save plot to PDF
ggsave(Fish_plot, filename = "Fish_plot.pdf", 
       path = "coordination_failures",
       width = 9, height = 7, units = "in")




# dev.off()

