#' Graph Designer(s): Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library("tidyverse")
library("ggforce")

n <- 12 # number of points you want on the unit circle
pts.circle <- t(sapply(1:n,function(r)c(cos(2*r*pi/n),sin(2*r*pi/n))))

df <- as.data.frame(pts.circle) %>%
  rename("x" = "V1", "y" = "V2")
df$pop <- c(2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1)
df$pop1 <- c(2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1)
df$pop2 <- c(2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1)

p <- df %>% ggplot(aes(x = x, y = y, color = as.factor(pop))) + 
  geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = FALSE) +
  geom_point(size = 7) +
  scale_color_manual(values = c("#377EB8", "#4DAF4A")) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave("society/schelling_1.pdf", plot = p, width = 6, height = 6)

p1 <- df %>% ggplot(aes(x = x, y = y, color = as.factor(pop1))) + 
  geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = FALSE) +
  geom_point(size = 7) +
  scale_color_manual(values = c("#377EB8", "#4DAF4A")) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave("society/schelling_2.pdf", plot = p1, width = 6, height = 6)

p2 <- df %>% ggplot(aes(x = x, y = y, color = as.factor(pop2))) + 
  geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = FALSE) +
  geom_point(size = 7) +
  scale_color_manual(values = c("#377EB8", "#4DAF4A")) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave("society/schelling_3.pdf", plot = p2, width = 6, height = 6)

