library(tidyverse)

#hA <- c(4.6, 2.7 ,2.7)
#hj <- c(4.6, 2.7 ,2.7)
#uA <- c(21.3, 40.9, 229.1)
#uj <- c(21.3, 40.9, 20)
w <- c(213, 409.1,409.1)
group2 <- c("NE", "Social", "Private")
#dfn10 <- tibble(hA, hj, uA, uj,w,type)
hours <- c(4.6, 2.7 ,2.7, 4.6, 2.7 ,2.7)
utility <- c(21.3, 40.9, 229.1, 21.3, 40.9, 20)
type <- c("A", "A", "A","j", "j", "j")
group <- c("NE", "Social", "Private","NE", "Social", "Private")
dfn10 <- tibble(hours, utility, type, group)
dfn10_total <- tibble(w, group2)

w5 <- c(375,281.25,375)
group2_5 <- c("Tioli", "NE", "Social")
#dfn10 <- tibble(hA, hj, uA, uj,w,type)
hours5 <- c(5, 7.5, 5, 5, 7.5 ,5)
utility5 <- c(150, 56.25,75,56.25,56.25,75)
type5 <- c("A", "A", "A","j", "j", "j")
group5 <- c("Tioli", "NE", "Social","Tioli", "NE", "Social")
dfn5 <- tibble(hours5, utility5, type5, group5)
dfn5_total <- tibble(w5, group2_5)





ggsave("coordination_failures/bargraph_n.pdf", plot1, width = 11, height = 7)

