#Graph Designer: Harriet Brookes Gray 
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(scales)
library(openxlsx)
library(ggplot2)
library(tidyverse)
library(shape)
pdf(file = "indmarketdemand/uber_price_elasticities.pdf", width = 18, height = 6)

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
grays <- gray.colors(25, start = 1, end = 0)


# #Data 
City <- c("Chicago", "Los Angeles", "New York", "San Francisco")
Elasticity <- c(0.66, 0.33, 0.61, 0.52)
Uber_elasticity_mat <- cbind(City, Elasticity)
Uber_elasticity_data <- as.data.frame(Uber_elasticity_mat)


plot2 <- Uber_elasticity_data %>% 
  ggplot(aes(x = City, y = Elasticity)) + 
  geom_bar(stat = "identity", colour="#CD2825", fill = "#CD2825") + 
  #scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = scales::percent) + 
  ylab("Price elasticity of demand") +
  xlab("U.S. City") +
  theme_bw() + 
  theme(axis.title = element_text(size = 18),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        panel.grid.minor = element_blank()
  ) + 
  #theme(legend.position = c(0.8,0.85),
  #legend.text.align = 0,
  #axis.title = element_text(size = 20),
  #axis.title.y = element_blank(),
  #axis.text.y = element_text(size = 18),
  #legend.title = element_text(size = 16),
  #legend.text = element_text(size = 18),
  #legend.title = element_text(size = 18),
  #axis.text.x  = element_text(vjust = 0.5, size = 18)) + 
  #geom_text(
  # aes(x = group5, y = utility5, label = utility5, group = type5),
# col = "black",
# hjust = -0.5, size = 4,
# position = position_dodge(width = 1),
# inherit.aes = TRUE
#) +
#ylim(0, 160) +
coord_flip()
plot2






dev.off()
