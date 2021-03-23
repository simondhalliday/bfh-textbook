#Graph Designer: Harriet Brookes-Gray 
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
library(tidyverse)
library(lubridate)
library(readxl)
library(readr)
library(latticeExtra)
library(digitize)
library(dplyr)
library(xlsx)

#------Markup data-------

# cal = ReadAndCal('competitionmarkets/markupdata.png')
# data.points = DigitData(col = 'red')
# df = Calibrate(data.points, cal, 1960, 2010, 1.2, 1.6)
# head(df)
# write.xlsx(df, file="competitionmarkets/markup_data.xlsx", 
#            sheetName="data")

CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")

markup_data <- read_excel("competitionmarkets/markup_data.xlsx")

markup_data2 <- markup_data %>% 
  rename(
    year = x,
    markup = y
  )



#------Gini data-------
# cal = ReadAndCal('competitionmarkets/marketincome_gini.png')
# data.points = DigitData(col = 'red')
# df = Calibrate(data.points, cal, 1980, 2015, 0.3, 0.7)
# head(df)
# write.xlsx(df, file="competitionmarkets/market_income_gini_data.xlsx",
#            sheetName="data")

#----Gini data (money income CBO)
# cal = ReadAndCal('competitionmarkets/gini_money_income.png')
# data.points = DigitData(col = 'red')
# df = Calibrate(data.points, cal, 1980, 2015, 0.3, 0.7)
# head(df)
# write.xlsx(df, file="competitionmarkets/money_income_gini_cbo_data.xlsx",
#            sheetName="data")

gini_data <- read_excel("competitionmarkets/gini_data.xlsx")

obj1 <- xyplot(markup_2 ~ year, data = markup_data2,
               xlab=list("Year", fontsize = 16),
               ylab = list("Mark-up ratio", fontsize = 16),
               ylab.right = list("Gini coefficient", fontsize = 16),
               par.settings = simpleTheme(col = 1),
               type = "l",
               col = "#E33024",
               lty=1,
               lwd = 1.5,
               scales=list(x=list(labels = c("1950", "1960", "1970", "1980","1990", "2000","2010"),
                                  cex=1,axs="r"), y=list(cex=6),tck = c(1,0)),
               key = list(type = c("l","l"),
                          lty = c(1,1),
                          text = list(label = c("Mark-up ratio", "Gini (income)")), lines = list(col= c("#E33024", CBCols[2])), column = 1,
                          x = 0.02, y = .9, fontsize = 16, lwd = 1.5))
lines = c(1,2)
obj2 <- xyplot(spliced_data ~ year,data = gini_data ,type = "l",col=CBCols[2],
               lty=lines,lwd = 1.5, 
               scales=list(x=list(labels = c("1950", "1960", "1970", "1980","1990", "2000","2010"),
                                  cex=2,axs="r"), y=list(cex=6))) 



doubleYScale(obj1, obj2)
