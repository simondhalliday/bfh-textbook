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

w5 <- c(281.25,375, 375)
group2_5 <- c("NE", "Social", "TIOLI")
#dfn10 <- tibble(hA, hj, uA, uj,w,type)
hours5 <- c(5,5,7.5,7.5,5,5)
utility5 <- c(56.25,56.25,75,75, 150,56.25)
type5 <- c("u^A","u^J", "u^A","u^J",  "u^A","u^J")
group5 <- c( "NE","NE", "Social", "Social","TIOLI", "TIOLI")
dfn5 <- tibble(hours5, utility5, type5, group5)
dfn5_total <- tibble(w5, group2_5)

dfn5 <- 
  dfn5 %>% 
  mutate(group5 = factor(group5, levels = c("TIOLI", "Social","NE")))

dfn5_total <- 
  dfn5_total %>% 
  mutate(group2_5 = factor(group2_5, levels = c("TIOLI", "Social","NE")))


plot1 <- 
  dfn5 %>% 
  ggplot(aes(x = group5, y = utility5, fill = type5)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  xlab("") +
  ylab("Individual utility") +
  scale_fill_manual(values=c("#377eb8","#e41a1c", "#41AE76","#FFEF66","#386cb0"),
                      name = "", 
                      breaks = c("u^J","u^A"),
                      labels = c( "Others' utility", "A's utility")
                      ) + 
  theme_bw() + 
  theme(legend.position = c(0.8,0.85),
        legend.text.align = 0,
        axis.title = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 18),
        #legend.title = element_text(size = 16),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.x  = element_text(vjust = 0.5, size = 18)) + 
    geom_text(
    aes(x = group5, y = utility5, label = utility5, group = type5),
    col = "black",
    hjust = -0.5, size = 4,
    position = position_dodge(width = 1),
    inherit.aes = TRUE
  ) +
  ylim(0, 160) +
  coord_flip()
plot1

plot2 <- 
  dfn5 %>% 
  ggplot(aes(x = group5, y = hours5, fill = type5)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  xlab("") +
  ylab("Individual hours") +
  scale_fill_manual(values = c("#4daf4a", "#984ea3"),
                    #values=c("#377eb8","#e41a1c", "#41AE76","#FFEF66","#386cb0"),
                    name = "", 
                    breaks = c("u^J","u^A"),
                    labels = c( "Others' hours", "A's hours")
  ) + 
  theme_bw() + 
  theme(legend.position = c(0.8,0.85),
        legend.text.align = 0,
        axis.title = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 18),
        #legend.title = element_text(size = 16),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.x  = element_text(vjust = 0.5, size = 18)) + 
  geom_text(
    aes(x = group5, y = hours5, label = hours5, group = type5),
    col = "black",
    hjust = -0.5, size = 4,
    position = position_dodge(width = 1),
    inherit.aes = TRUE
  ) +
  ylim(0, 9) +
  coord_flip()
plot2

plot3 <- 
  dfn5_total %>% 
  ggplot(aes(x = group2_5, y = w5)) + 
  geom_bar(stat = "identity", position = position_dodge(), fill = "#ff7f00") + 
  xlab("") +
  ylab("Total utility") +
  theme_bw() + 
  theme(legend.position = c(0.8,0.85),
        legend.text.align = 0,
        axis.title = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 18),
        #legend.title = element_text(size = 16),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.x  = element_text(vjust = 0.5, size = 18)) + 
  geom_text(
    aes(x = group2_5, y = w5, label = w5),
    col = "black",
    hjust = -0.5, size = 4,
    position = position_dodge(width = 1),
    inherit.aes = TRUE
  ) +
  ylim(0, 420) +
  coord_flip()
plot3

require(gridExtra)
plots <- grid.arrange(plot2, plot1, plot3, ncol = 1, heights = c(2, 2, 1))

ggsave("coordination_failures/bargraph_n.pdf", plots, width = 7, height = 12)

