#' Graph Designer: Weikai Chen, Scott Cohn
#' Source: The Efficacy of Seat Belt Legislation, John G.U. Adams 1982
#' Link: http://john-adams.co.uk/wp-content/uploads/2006/SAE%20seatbelts.pdf 

library(tidyverse)
#setwd("~/Documents/Code/")
# create the data frame called rad,
# short for road accidetn death
color <- c("#E41A1C", "#377EB8")

rad <- data.frame(
    "Year" = 1970:1978,
    "Law" = c(100, 102, 102, 100, 90, 88, 86, 86, 85),
    "NoLaw" = c(102, 102, 104, 100, 87, 83, 81, 79, 80))

# the date when the Law passed in thirteen states
LawPass <- c(1971.0046, 1971.4634, 1972.3429,
             1972.3945, 1973.1392, 1974.0192,
             1974.4086, 1974.4597, 1974.5109,
             1974.8085, 1974.9419, 1974.9934,
             1975.0654)
# Plot
plot.rad <- ggplot(data = rad, aes(x = Year)) +
    geom_line(aes( y = Law, color = 'Law'),
              size = 1) +
    geom_point(aes( y = Law, color = 'Law'), size = 2)+
    geom_line(aes( y = NoLaw, color = 'No Law'),
              size = 1) +
    geom_point(aes(y = NoLaw, color = 'No Law'), size = 2)+
    geom_vline(xintercept = LawPass,
               aes(color = 'LawPass'),
               alpha = 0.3) +
    labs(y = 'Road Fatalities') +
    scale_x_continuous(breaks = 1970:1978) + 
    scale_color_brewer(palette = "Set1") + 
    theme_bw() + 
    theme(panel.grid.minor = element_blank(),
          legend.position = c(0.9, 0.9),
          legend.title = element_blank(),
          legend.text = element_text(size = 15),
          axis.title = element_text(size = 22),
          axis.text = element_text(size = 17))

# save the figure
ggsave(plot = plot.rad, "public_mechanism/road_accident_death.pdf", width = 8, height = 6, units = "in")
