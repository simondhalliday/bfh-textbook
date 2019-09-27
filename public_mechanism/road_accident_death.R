library(tidyverse)
#setwd("~/Documents/Code/")
# create the data frame called rad,
# short for road accidetn death
color <- c("#238b45","#084081")


COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

rad <- data.frame(
    "Year" = 1970:1978,
    "Law" = c(100, 102, 102, 100, 90, 88, 86, 86, 85),
    "NoLaw" = c(102, 102, 104, 100, 87, 83, 81, 79, 80))

# the date when the Law passed in thirteen states
LawPass <- c(1971.0046,1971.4634,1972.3429,
             1972.3945,1973.1392,1974.0192,
             1974.4086,1974.4597,1974.5109,
             1974.8085,1974.9419,1974.9934,
             1975.0654)
# Plot
plot.rad <- 
    ggplot(data = rad, aes(x = Year)) +
    geom_line(aes( y = Law, color = 'Law'),
              size = 1, linetype = 'dashed') +
    geom_point(aes( y = Law, color = 'Law'), size = 2) +
    geom_line(aes( y = NoLaw, color = 'No Law'),
              size = 1, linetype = 'dotted') +
    geom_point(aes(y = NoLaw, color = 'No Law'), size = 2) +
    geom_vline(xintercept = LawPass,
               aes(color = 'LawPass'),
               alpha = 0.3) +
    labs(y = 'Road Fatalities') +
    scale_x_continuous(breaks = 1970:1978) + 
    #The variables were inverted and I think it's easier just to change the label here
    scale_color_brewer(type = "qual", 
                       palette = "Set1",
                       name = "Type", 
                       breaks = c('Law', 'No Law'), 
                       labels = c("No Law", "Law")
                       ) + 
    theme_bw() + 
    theme(legend.position = c(0.9,0.9), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 16))

# save the figure
ggsave(plot = plot.rad, "public_mechanism/figure-rad.pdf", width = 8, height = 6, units = "in")
