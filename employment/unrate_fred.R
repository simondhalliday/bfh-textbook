#' Graph Designer: Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics
#' Unemployment: https://fred.stlouisfed.org/series/UNRATE 
#' 
library("tidyverse")
library("ggplot2")
library("scales")
library("zoo")

COL <- c("#1F78B4","#E31A1C")

UNRATE_FRED <- read_csv("~/Downloads/UNRATE_FRED.csv")

UNRATE_FRED %>% ggplot() + 
  # geom_rect(data = data.frame(xmin = as.double("2007-12-01"),
  #                             xmax = as.double("2009-07-01"),
  #                             ymin = -Inf,
  #                             ymax = Inf),
  #           aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  #           fill = "#E31A1C", alpha = 0.2) +
  geom_point(aes(x = DATE, y = UNRATE), color = COL[1], cex = 2.5) + 
  labs(x = "Date (months)", y = "Unemployment rate") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 17),  
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22))
  
ggsave("employment/unrate_fred.pdf", width = 9, height = 6, units = "in")
