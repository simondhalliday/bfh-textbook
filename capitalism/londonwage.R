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

p1 <- p + 
  scale_x_continuous(breaks = round(c(seq(1300, 
                                                max(londonrealwage$Year), 
                                                by = 100), 2000))) +
  labs(y = "London craftsmen's real wages") 

p2 <- p1 + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 17))

ggsave("capitalism/londonwage.pdf", width = 6, height = 4, units = "in")