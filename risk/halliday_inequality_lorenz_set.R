#Graph Designer: Simon Halliday & Riley Boeth '17
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(pBrackets)
library(ineq)
library(tidyverse)
library(readxl)

#Edited the margins to cater for the larger LHS labels
df <- read_xlsx("risk/dev_data.xlsx")

income <- 
  df %>% 
  select(-Country) %>% 
  gather(country, value, - x)

plot1 <- 
  income %>% 
  ggplot(aes(x = x, y = value, linetype = country)) + 
  geom_line() + 
  scale_x_continuous(breaks = seq(0.1, 1, by = 0.1),
                     labels = seq(0.1, 1, by = 0.1)) + 
  #scale_x_discrete(limit = seq(0.1, 1, by = 0.1)) +
  xlab("Cumulative population proportion") +
  ylab("Cumulative income") + 
  scale_color_discrete(name = "Economy") +
  ggtitle("Lorenz Curves for 10 Artificial Economies") +
  theme_bw()

pdf(file = "risk/halliday_inequality_comparison.pdf", width = 6, height = 4)
plot1
dev.off()

