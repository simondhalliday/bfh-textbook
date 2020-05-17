#' Graph Designer: Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics
#' https://www.journals.uchicago.edu/doi/10.1086/682406
#' Lezear Shaw Stanton

library("tidyverse")
library("ggplot2")
library("scales")
library("zoo")

lazear_shaw_stanton <- read_csv("Downloads/lazear_shaw_stanton.csv", col_names = FALSE)

lss <- lazear_shaw_stanton %>% rename(
  date = X1,
  log_OPH = X2
)

lss$date <- as.yearmon(lss$date, "%Y-%m")

COL <- c("#1F78B4","#E31A1C")

lss %>% ggplot() + 
  geom_vline(xintercept = as.numeric(as.yearmon("2009-07")), color = COL[2], lwd = 1) +
  geom_vline(xintercept = as.numeric(as.yearmon("2007-12")), color = COL[2], lwd = 1) +
  geom_point(aes(x = date, y = log_OPH), color = COL[1], cex = 2.5) +
  annotate("text", x = as.numeric(as.yearmon("2008-06")), y = 2.21, label = "Recession", size = 6) +
  annotate("text", x = as.numeric(as.yearmon("2008-06")), y = 2.2, label = "begins Dec 2007", size = 6) + 
  annotate("text", x = as.numeric(as.yearmon("2009-12")), y = 2.21, label = "Recession", size = 6) +
  annotate("text", x = as.numeric(as.yearmon("2009-12")), y = 2.2, label = "ends Jul 2007", size = 6) + 
  labs(x = "Date (months)", y = "Monthly mean log worker productivity") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 17),  
        axis.title.x = element_text(size = 19),
        axis.title.y = element_text(size = 19))

ggsave("employment/lazear_shaw_stanton.pdf", width = 9, height = 6, units = "in")
