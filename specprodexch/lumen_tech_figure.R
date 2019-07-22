#Graph Designer: Bridget Diana + Scott Cohn + Simon Halliday
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics


require(shape)
library(tidyverse)
library(lubridate)
# ----
#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

#----
# pull in data
# ----
library(readr)
lumen_data <- read_csv("lumen_data.csv")

lumen_data$lnLaborHourPer1000LumenHours <-log(lumen_data$LaborHourPer1000LumenHours)
# graph
# ----

pdf(file = "hours_per_lumenhour.pdf", width = 7, height = 7)

ggplot(data = lumen_data, mapping = aes(x = reorder(Year, -LaborHourPer1000LumenHours), y = LaborHourPer1000LumenHours)) + labs(x = "Year", y = "Hours of Work per 1000 Lumen Hours") + geom_bar(stat = "identity", fill = COLB[4]) + theme_bw()

dev.off()



