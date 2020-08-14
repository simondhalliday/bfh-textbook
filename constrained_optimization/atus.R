library(tidyverse)
library(readxl)


COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

ATUS <- read_excel("constrained_optimization/atus.xlsx")

ATUS2 <- 
  ATUS %>% 
  gather(Gender, hours, -Category)

ATUS2 %>% 
  filter(!Category %in% c("Communication","Other", "Civil & Religious", "Education")) %>%
  ggplot(aes(x = reorder(Category, hours), y = hours, group = Gender, color = Gender, fill = Gender)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  ylab("Hours") +
  theme_bw() +
  scale_fill_manual(values = c("#41ae76", "#084081")) +
  scale_color_manual(values = c("#41ae76", "#084081")) +
  #scale_fill_brewer(type = "qual", palette = "Accent") + 
  #scale_color_brewer(type = "qual", palette = "Accent") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        text = element_text(size = 12), 
        axis.title.x=element_blank())
ggsave(file = "constrained_optimization/atus.pdf", device = "pdf", width = 6, height = 4)