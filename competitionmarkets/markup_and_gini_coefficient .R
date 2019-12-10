#Graph Designer: Harriet Brookes-Gray 
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(readr)


markup_data <- read_csv("competitionmarkets/loecker_barkai/LBdf_data.xlsx")
loecker_2018 <- read_csv("competitionmarkets/loecker_barkai/loecker_2018.csv")
