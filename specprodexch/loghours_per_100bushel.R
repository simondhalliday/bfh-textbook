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
library(tidyverse)

ag_data <- read_csv("specprodexch/ag_data.csv")

ag_data$lnHoursPerBushel <- log(ag_data$HoursPerBushel)
ag_data$lnBushelsPerHourLabor <- log(ag_data$BushelsPerHourLabor)
ag_data$lnHoursPer100Bushels <- log(ag_data$HoursPer100Bushels)

# ag_data <- 
#   ag_data %>% 
  

# graph
# ----

pdf(file = "hours_per_bushel.pdf", width = 7, height = 7)

ggplot(data = ag_data, mapping = aes(x = as.factor(Year), y = HoursPerBushel)) +  labs(x = "Year", y = "Labor Hours Per One Bushel of Wheat") + geom_bar(stat = "identity", fill = COLB[4]) + annotate("text", x = c(1,2, 3, 4, 5, 6, 7), y = c(2.9, 0.63, 0.37, 0.27, 0.23, 0.25, 0.21), label = c("Walking plow, \n brush harrow, \n hand broadcast of \n seed, sickle, and flail", "Gang plow, seeder, \n harrow, binder, \n thresher, wagons, \n and horses", "3-bottom gang plow, \n tractor, 10-foot \n tandem disk, harrow, \n 12-foot combine, \nand trucks", "tractor, 10-foot plow, \n 12-foot row weeder, \nharrow, 14-foot drill, \n self-propelled \n combine, and trucks", "tractor, 12-foot plow, \n 14-foot drill, 14-foot \n self-propelled \n combine, and trucks", "tractor, 30-foot \n sweep disk, 27-foot \n drill, 22-foot \n self-propelled \n combine, and trucks", "tractor, 35-foot sweep  \n disk, 30-foot drill, \n 25-foot self-propelled \n combine, and trucks") , color="black", size=2) +theme_bw() + ylim(0,3.1)

dev.off()

pdf(file = "hours_per_100bushel.pdf", width = 7, height = 7)

ggplot(data = ag_data, 
       mapping = aes(x = as.factor(Year), y = HoursPer100Bushels)) +  
  labs(x = "Year", y = "Labor Hours Per 100 Bushels of Wheat") + 
  geom_bar(stat = "identity", fill = COLB[4]) + 
  annotate("text", 
           x = c(1,2, 3, 4, 5, 6, 7), 
           y = c(300, 71, 53, 50, 41, 40, 39), 
           label = c("walking plow, \n brush harrow, \n hand broadcast of \n seed, sickle, \n and flail: \n 275 hours", 
                     "gang plow, \n seeder, harrow, \n binder, thresher, \n wagons, \n and horses: \n 45 hours", 
                     "3-bottom \n gang plow, \n tractor, 10-foot \n tandem disk, \n harrow, 12-foot \n combine, and \n trucks:\n 17.5 hours", 
                     "tractor, \n 10-foot plow, \n 12-foot row \n weeder, \nharrow, \n 14-foot drill, \n self-propelled \n combine, \n and trucks: \n 6.5 hours", 
                     "tractor, \n 12-foot plow, \n 14-foot drill,\n  14-foot \n self-propelled \n combine, \n and trucks: \n 5 hours", 
                     "tractor, 30-foot \n sweep disk, \n 27-foot drill, \n 22-foot \n self-propelled \n combine, \n and trucks: \n 3.75 hours", 
                     "tractor, \n 35-foot sweep  \n disk, 30-foot \n drill, 25-foot \n self-propelled \n combine, \n and trucks: \n 3 hours") , 
           color="black", size= 3) + 
  theme_bw() + 
  ylim(0,310)

dev.off()


pdf(file = "specprodexch/loghours_per_100bushel.pdf", width = 7, height = 7)

ggplot(data = ag_data, 
       mapping = aes(x = as.factor(Year), y = lnHoursPer100Bushels)) +  
  labs(x = "Year", y = "Natural Log of Labor Hours Per 100 Bushels of Wheat") + 
  geom_bar(stat = "identity", fill = COLB[4]) + 
  # annotate("text", 
  #          x = c(1, 2, 3, 4, 5, 6, 7), 
  #          y = c(300, 71, 53, 50, 41, 40, 39), 
  #          label = c("walking plow, \n brush harrow, \n hand broadcast of \n seed, sickle, \n and flail: \n 275 hours", 
  #                    "gang plow, \n seeder, harrow, \n binder, thresher, \n wagons, \n and horses: \n 45 hours", 
  #                    "3-bottom \n gang plow, \n tractor, 10-foot \n tandem disk, \n harrow, 12-foot \n combine, and \n trucks:\n 17.5 hours", 
  #                    "tractor, \n 10-foot plow, \n 12-foot row \n weeder, \nharrow, \n 14-foot drill, \n self-propelled \n combine, \n and trucks: \n 6.5 hours", 
  #                    "tractor, \n 12-foot plow, \n 14-foot drill,\n  14-foot \n self-propelled \n combine, \n and trucks: \n 5 hours", 
  #                    "tractor, 30-foot \n sweep disk, \n 27-foot drill, \n 22-foot \n self-propelled \n combine, \n and trucks: \n 3.75 hours", 
  #                    "tractor, \n 35-foot sweep  \n disk, 30-foot \n drill, 25-foot \n self-propelled \n combine, \n and trucks: \n 3 hours") , 
  #          color="black", size= 3) + 
  theme_bw() + 
  ylim(0, 6) + 
  theme(legend.position = "none", 
        legend.title = element_blank(), 
        axis.title.y = element_text(size = 16, vjust = 0.5),
        axis.title.x = element_text(size = 16, vjust = -1),
        legend.text = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)
  ) 


dev.off()




pdf(file = "bushels_per_hour.pdf", width = 7, height = 7)

ggplot(data = ag_data, 
       mapping = aes(x = as.factor(Year), y = BushelsPerHourLabor)) + 
  labs(x = "Year", y = "Bushels of Wheat Per One Hour of Labor") + 
  geom_bar(stat = "identity", fill = COLA[4]) + 
  annotate("text", x = c(1,2, 3, 4, 5, 6, 7), y = c(3.1,5, 9, 18.7, 23, 30, 36), label = c("Walking plow, \n brush harrow, \n hand broadcast of \n seed, sickle, and flail", "Gang plow, seeder, \n harrow, binder, \n thresher, wagons, \n and horses", "3-bottom gang plow, \n tractor, 10-foot \n tandem disk, harrow, \n 12-foot combine, \nand trucks", "tractor, 10-foot plow, \n 12-foot row weeder, \nharrow, 14-foot drill, \n self-propelled \n combine, and trucks", "tractor, 12-foot plow, \n 14-foot drill, 14-foot \n self-propelled \n combine, and trucks", "tractor, 30-foot \n sweep disk, 27-foot \n drill, 22-foot \n self-propelled \n combine, and trucks", "tractor, 35-foot sweep  \n disk, 30-foot drill, \n 25-foot self-propelled \n combine, and trucks") , color="black", size=2) +theme_bw() + ylim(0,40)
#+ annotate("segment", x = c(1,2,3,4), xend = c(1,2,3,4), y = c(4,7,10,18.5) , yend = c(1,3,6,15.5), colour = "black", size=0.4, alpha=1, arrow=arrow()) 

dev.off()

