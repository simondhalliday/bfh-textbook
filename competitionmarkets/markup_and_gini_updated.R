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

#cal = ReadAndCal('competitionmarkets/markupdata.jpg')
#data.points = DigitData(col = 'red')
#df = Calibrate(data.points, cal, 1960, 2015, 1.2, 1.7)
#head(df)
#write.xlsx(df, file="competitionmarkets/markup_data.xlsx", 
#           sheetName="data")


markup_data <- read_excel("competitionmarkets/markup_data.xlsx")

