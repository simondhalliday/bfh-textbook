#' Scott Cohn
#' Bowles Halliday 
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

lss %>% ggplot() + 
  geom_point(aes(x = date, y = log_OPH)) +
  geom_vline(xintercept = as.numeric(as.yearmon("2009-07"))) +
  geom_vline(xintercept = as.numeric(as.yearmon("2007-12"))) +
  labs(x = "Date", y = "log OPH") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

