#' Graph Designer(s): Simon Halliday,  Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library("tidyverse")
library("dplyr")
library("ggthemes")

londonrealwage <- read_csv("capitalism/data/londonrealwage.csv", 
                           skip = 1)

p <- londonrealwage %>% ggplot(aes(x = Year, y = London_craftsman_real_wage)) +
  geom_line(color = "#377eba")

p1 <- p + scale_x_continuous(breaks = round(seq(1250, 
                                                max(londonrealwage$Year), 
                                                by = 50), 2000)) +
  labs(y = "London craftsmen's real wages") 

p2 <- p1 + theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 20))

ggsave("capitalism/londonwage.pdf", width = 9, height = 7, units = "in")