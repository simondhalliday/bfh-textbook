library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(mosaic)
library(haven)
library(readxl)
library(zoo)

Lumendata <- read_excel("preface/D-Fig 3-Lumen hours data.xlsx", col_names = TRUE)

options(scipen = 999)

#plot for years before 18000
LumenPlot1 <- 
  Lumendata %>% 
  ggplot(aes(x = Year, y = `Lumen-hours per hour of labor 100,000 BC to AD 1992`)) +
  geom_point() +
  geom_line()

print(LumenPlot1)