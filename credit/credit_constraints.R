#Graph Designer: Harriet Brookes Gray 
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(tidyverse)
library(ggplot2)
pdf(file = "credit/credit_constraints.pdf", width = 8, height = 6)

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


# #Data 
# Income_range1 <- c("<40,000", "<40,000", "<40,000", "<40,000")
# Race1 <- c("White", "Black", "Hispanic", "Overall")
# Denied1 <- c(40,58,41,43)
# Approved1 <- c(52, 32, 51,49)
# Denied_Approved_less1 <- c(48,68,49,51)
# Approved_less1 <- c(8, 10, 8, 8)
# data1 <- cbind(Income_range1, Race1, Approved1, Denied1, Denied_Approved_less1, Approved_less1)
# 
# Income_range2 <- c("40,000-100,000", "40,000-100,000", "40,000-100,000", "40,000-100,000")
# Race2 <- c("White", "Black", "Hispanic", "Overall")
# Denied2 <- c(17,41,30,22)
# Approved2 <- c(78,43,61,71)
# Denied_Approved_less2 <- c(22,57,39,29)
# Approved_less2 <- c(5,16,9,7)
# data2 <- cbind(Income_range2, Race2, Approved2, Denied2, Denied_Approved_less2, Approved_less2)
# 
# Income_range3 <- c(">100,000", ">100,000", ">100,000", ">100,000")
# Race3 <- c("White", "Black", "Hispanic", "Overall")
# Denied3 <- c(7,19,17,9)
# Approved3 <- c(90,69,78,87)
# Denied_Approved_less3 <- c(10,31,22,13)
# Approved_less3 <- c(3,12,5,4)
# data3 <- cbind(Income_range3, Race3, Approved3, Denied3, Denied_Approved_less3, Approved_less3)
# 
# Income_range4 <- c("All", "All", "All", "All")
# Race4 <- c("White", "Black", "Hispanic", "Overall")
# Denied4 <- c(19,44,32,24)
# Approved4 <- c(76,43,60,69)
# Denied_Approved_less4 <- c(24,57,40,31)
# Approved_less4 <- c(5,13,8,7)
# data4 <- cbind(Income_range4, Race4, Approved4, Denied4, Denied_Approved_less4, Approved_less4)
# 
# credit_outcomes_mat <- rbind(data1,data2,data3,data4) 
# 
# credit_outcomes_df <- as.data.frame(credit_outcomes_mat)
# 
# credit_outcomes_df <- credit_outcomes_df %>%
#   rename("income_range" = "Income_range1", 
#          "race" = "Race1", 
#          "approved" = "Approved1",
#          "denied" = "Denied1", 
#          "denied_approved_less" = "Denied_Approved_less1", 
#          "approved_less" = "Approved_less1") %>%
#   gather(type, percent, c(denied, approved, denied_approved_less, approved_less)) 
# 
# credit_outcomes_df$percent <- as.numeric(credit_outcomes_df$percent)
# 
# #plot 1 - Denied and Approved but received less than requested. 
# 
# data_credit1 <- credit_outcomes_df %>%
#   filter(income_range != "All") %>%
#   filter(type != "denied_approved_less") %>%
#   filter(type != "approved") %>%
#   filter(race == "Overall")
# 
# data_credit1$income_range <- factor(data_credit1$income_range, levels = c("<40,000", "40,000-100,000", ">100,000"))
# 
# plot1 <- ggplot(data_credit1, aes(x=income_range, y = percent)) + 
#   geom_bar(stat="identity", position = "dodge", aes(fill = type))
# 
# plot1

# plot2 - 6 horizontal bars. 

x <- c("<40,000", "40,000-100,000", "100,000<", "Cannot cover $400 sudden expense with cash, savings or credit card", "Cannot cover emergency expenses by any means", "Borrowing at credit card rates")
y <- c(51,29, 13, 37, 30, 52)
mat <- cbind(x, y)
data <- as.data.frame(mat)
data$y <- as.numeric(data$y)

#data$x <- factor(data$x, levels = c("<40,000", "40,000-100,000", "100,000<", "Can't cover $400 sudden expenses with cash, savings or credit card", "Cannot cover emergency expenses by any means", "Borrowing at credit card rates"))

data$x <- factor(data$x, levels = c("Borrowing at credit card rates","Cannot cover emergency expenses by any means", "Cannot cover $400 sudden expense with cash, savings or credit card", "100,000<", "40,000-100,000","<40,000"))


plot2 <- 
  data %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_bar(stat = "identity") + 
  xlab("") +
  ylab("") +
  theme_bw() + 
  #theme(legend.position = c(0.8,0.85),
        #legend.text.align = 0,
        #axis.title = element_text(size = 20),
        #axis.title.y = element_blank(),
        #axis.text.y = element_text(size = 18),
        #legend.title = element_text(size = 16),
        #legend.text = element_text(size = 18),
        #legend.title = element_text(size = 18),
        #axis.text.x  = element_text(vjust = 0.5, size = 18)) + 
  #geom_text(
   # aes(x = group5, y = utility5, label = utility5, group = type5),
   # col = "black",
   # hjust = -0.5, size = 4,
   # position = position_dodge(width = 1),
   # inherit.aes = TRUE
  #) +
  #ylim(0, 160) +
  coord_flip()
plot2





dev.off()
