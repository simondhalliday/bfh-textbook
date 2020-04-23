# Defunct. See `veblen_bowles_[non]_euro.R` in \indmarketdemand.

library(tidyr)
library(dplyr)

library(readr)
library(ggplot2)
library(mosaic)
library(ggsci)

WorkHoursBase <- read.csv("constrained_optimization/workhours.csv")

WorkHoursSelected <- 
  WorkHoursBase %>%
  select(year, hour, name) %>%
  mutate(year = as.factor(year))

WorkHoursPlot1 <- 
  WorkHoursSelected %>%
  filter(name == "FRA" | name == "NET" | name == "SWI" | name == "GER" | name == "UK" | name == "SWE")

pdf(file = "constrained_optimization/workhours1.pdf", width = 5, height = 4)

WorkHoursPlot1 %>% 
  ggplot(aes(x = year, y = hour, group = name, color = name)) +
  geom_point() +
  geom_line() +
  ylim(1000, 3500) + 
  ylab("Total Hours Worked per year") +
  xlab("Year") +
  labs(colour = "Country") +
  scale_color_d3() +
  #scale_colour_discrete(name = "Country") +
  #scale_shape_discrete(name = "Country") +
  theme_bw() 
dev.off()

WorkHoursPlot2 <- 
  WorkHoursSelected %>%
  filter(name == "AUS" | name == "CAN" | name == "US" | name == "JPN" )

pdf(file = "constrained_optimization/workhours2.pdf", width = 5, height = 4)

WorkHoursPlot2 %>% 
  ggplot(aes(x = year, y = hour, group = name, color = name)) +
  geom_point() +
  geom_line() +
  ylim(1000, 3500) + 
  ylab("Total Hours Worked per year") +
  xlab("Year") +
  labs(colour = "Country") +
  scale_color_d3() +
  #scale_colour_discrete(name = "Country") +
  #scale_shape_discrete(name = "Country") +
  theme_bw() 

dev.off()