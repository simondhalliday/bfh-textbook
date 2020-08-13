library(tidyverse)
library(ggrepel)
library(readxl)

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824", "#f0027f")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081", "#9e9ac8","#f0027f")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
COLD <- c("#DA3030","#41ae76","#F7DE04", "#4eb3d3","#AE82FF","#386cb0","#F48318","#41ae76","#6a51a3", "#DA3030")


bloom_gini<-read_excel("capitalism/bloom_gini.xlsx")

bloom_gini <- 
  bloom_gini %>% 
  filter(!country %in% c("Greece", "Poland", "Slovenia")) %>% #Dropped from both figures
  filter(!country %in% c("South Africa", "Uruguay", "Guatemala", "Brazil",  "Peru", "Colombia", "Egypt", "Mexico", "China", "India"))


p <- 
  bloom_gini %>%
  #filter(country != "Australia") %>%
  ggplot(aes(x = DisposableGini, y = bloombergscore)) +
  geom_point(color = "#4DAF4A", size = 3) +
  geom_text_repel(aes(label = country), size = 5) + 
  labs(x = "Long-term average in equality in disposable income",
       y = "Innovation index score") +
  scale_x_continuous(expand = c(0, .01)) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 21),
        axis.text = element_text(size = 18))

ggsave(file = "capitalism/bloom_gini.pdf", width = 9, height = 7, units = "in")
