#Graph Designer: Harriet BG
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics


library(ggplot2)


p1 <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) + ylab("") +
  scale_y_continuous(breaks = NULL) + 
  theme_bw() + 
  xlab("x") +
  ylab("Conintuous Probability Distribution") +
  annotate(geom="text", x=0, y=0.2, label="Total Area = 1",
           color="black", size = 7) + 
  theme(axis.title = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.text.x  = element_text(size = 13)
  )
p1


ggsave(p1, filename = "continuous_probability_distribution.pdf", 
       path = "mathappendix",
       width = 9, height = 7, units = "in")


dev.off()