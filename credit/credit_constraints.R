#Graph Designer: Harriet Brookes Gray 
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(tidyverse)
library(ggplot2)
#pdf(file = "credit/credit_constraints.pdf", width = 8, height = 6)

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
grays <- gray.colors(25, start = 1, end = 0)


#Data 
Income_range1 <- c("<40,000", "<40,000", "<40,000", "<40,000")
Race1 <- c("White", "Black", "Hispanic", "Overall")
Denied1 <- c(40,58,41,43)
Denied_Approved_less1 <- c(48,68,49,51)
Approved_less1 <- c(8, 10, 8, 8)
data1 <- cbind(Income_range1, Race1, Denied1, Denied_Approved_less1, Approved_less1)

Income_range2 <- c("40,000-100,000", "40,000-100,000", "40,000-100,000", "40,000-100,000")
Race2 <- c("White", "Black", "Hispanic", "Overall")
Denied2 <- c(17,41,30,22)
Denied_Approved_less2 <- c(22,57,39,29)
Approved_less2 <- c(5,16,9,7)
data2 <- cbind(Income_range2, Race2, Denied2, Denied_Approved_less2, Approved_less2)

Income_range3 <- c(">100,000", ">100,000", ">100,000", ">100,000")
Race3 <- c("White", "Black", "Hispanic", "Overall")
Denied3 <- c(7,19,17,9)
Denied_Approved_less3 <- c(10,31,22,13)
Approved_less3 <- c(3,12,5,4)
data3 <- cbind(Income_range3, Race3, Denied3, Denied_Approved_less3, Approved_less3)

Income_range4 <- c("All", "All", "All", "All")
Race4 <- c("White", "Black", "Hispanic", "Overall")
Denied4 <- c(19,44,32,24)
Denied_Approved_less4 <- c(24,57,40,31)
Approved_less4 <- c(5,13,8,7)
data4 <- cbind(Income_range4, Race4, Denied4, Denied_Approved_less4, Approved_less4)

credit_outcomes_mat <- rbind(data1,data2,data3,data4) 

credit_outcomes_df <- as.data.frame(credit_outcomes_mat)

credit_outcomes_df <- credit_outcomes_df %>%
  rename("income_range" = "Income_range1", 
         "race" = "Race1", 
         "denied" = "Denied1", 
         "denied_approved_less" = "Denied_Approved_less1", 
         "approved_less" = "Approved_less1") %>%
  gather(type, percent, c(denied, denied_approved_less, approved_less)) 

credit_outcomes_df$percent <- as.numeric(credit_outcomes_df$percent)

#plot 1 

data_credit1 <- credit_outcomes_df %>%
  filter(race != "Overall") %>%
  filter(income_range != "All")

plot1 <- ggplot(data_credit1, aes(x=income_range, y = percent)) + 
  geom_bar(stat="identity", position = "dodge", aes(fill = type))

plot1

#dev.off()
