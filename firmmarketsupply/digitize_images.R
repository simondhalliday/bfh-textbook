
#if(!require(devtools)) install.packages('devtools')
#devtools::install_github("tpoisot/digitize")

library(digitize)
library(tidyverse)
#mydata <- ReadAndCal("firmmarketsupply/markup_figure_sam.jpg")

#data.points <- DigitData(col = 'red')
#df <- Calibrate(data.points, mydata, 1950, 2015, 1.1, 1.7)
#write_csv(df, "loecker_2018.csv")

loecker <- read_csv("loecker_2018.csv")
loecker <- 
  loecker %>% 
  rename(markup = y, date = x) %>%
  mutate(year = round(date, digits = 0), 
         markup = markup - 1)

# ggplot(df, aes(x = x, y = y-1)) + 
#   geom_line()

# barkaidata <- ReadAndCal("competitionmarkets/barkai_profit.png")
# barkai.points <- DigitData(col = 'red')
# df <- Calibrate(barkai.points, barkaidata, 1983, 2014, 0.0, 0.18)
# write_csv(df, "barkai_2018.csv")



ggplot(df, aes(x = x, y = y)) + 
  geom_line()


