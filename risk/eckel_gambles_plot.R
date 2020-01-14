library(tidyverse)

COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

lottery <- read_csv("risk/eckel_gamble.csv")

lottery <- 
  lottery %>% 
  mutate(Lottery = factor(Lottery, labels = c("L1", "L2", "L3", "L4", "L5", "L6")))

gamble_plot <- lottery %>% ggplot(aes(x = Lottery, y = Value, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c(COLA[3], COLB[3])) +
  theme_bw() + 
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18)) +
  labs(y = "Proportion")
  

ggsave(filename = "risk/eckel_gambles_plot.pdf", gamble_plot, width = 9, height = 7)


