library(tidyverse)

#hA <- c(4.6, 2.7 ,2.7)
#hj <- c(4.6, 2.7 ,2.7)
#uA <- c(21.3, 40.9, 229.1)
#uj <- c(21.3, 40.9, 20)
w <- c(213, 409.1,409.1)
group2 <- c("NE", "Social", "Private")
#dfn10 <- tibble(hA, hj, uA, uj,w,type)
hours <- c(4.6, 4.6 ,2.7, 2.7, 2.7 ,2.7)
utility <- c(21.3,21.3, 40.9, 40.9, 229.1, 20)
type <- c("A_NE","j_NE", "A_s","j_s", "A_p", "j_p")
group <- c("NE", "NE", "Social","Social", "Private", "Private")
dfn10 <- tibble(hours, utility, type, group)
dfn10_total <- tibble(w, group2)

w5 <- c(375,281.25,375)
group2_5 <- c("Tioli", "NE", "Social")
#dfn10 <- tibble(hA, hj, uA, uj,w,type)
hours5 <- c(5,5,7.5,7.5,5,5)
utility5 <- c(150,56.25,56.25,56.25,75,75)
type5 <- c("A_t","j_t", "A_NE", "j_NE", "A_s", "j_s")
group5 <- c("Tioli", "Tioli", "NE","NE", "Social", "Social")
dfn5 <- tibble(hours5, utility5, type5, group5)
dfn5_total <- tibble(w5, group2_5)


plot1 <- ggplot(dfn10, aes(x = type, y = utility)) + geom_bar(stat = "identity", position = position_dodge())
plot1


ggsave("coordination_failures/bargraph_n.pdf", plot1, width = 11, height = 7)

