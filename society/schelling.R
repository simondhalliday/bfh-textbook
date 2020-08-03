#' Graph Designer(s): Scott Cohn & Simon Halliday
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library("tidyverse")
library("ggforce")
library("extrafont")

n <- 12 # number of points you want on the unit circle
pts.circle <- t(sapply(1:n,
                       function(r)c(cos(2*r*pi/n),sin(2*r*pi/n)))
                )

df <- as.data.frame(pts.circle) %>%
  rename("x" = "V1", "y" = "V2")
df$positions <- c(2, 1, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3)
df$pop <- c(2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1)
df$pop1 <- c(2, 1, 2, 2, 1, 1, 2, 1, 2, 1, 2, 1)
df$pop2 <- c(2, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 1)
df$pop3 <- c(2, 1, 2, 2, 1, 2, 2, 2, 1, 1, 1, 1)
df$pop4 <- c(2, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1)
df$pop5 <- c(2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1)
df$pop6 <- c(2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1)

#x = -0.86, y = 0.5, xend = -0.5, yend = 0.86

# dfcurve1 <- 
#   df %>% 
#   slice(1:2) %>%
#   select(x,y) %>% 
#   pivot_wider(
#     names_from = c(x, y),
#     values_from = c(x, y)
#   ) 
# colnames(dfcurve1) <- c("x1", "x2", "y1", "y2")

#Baseline with no steps
p <- df %>% ggplot(aes(x = x, y = y, color = as.factor(pop))) + 
  geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = FALSE) +
  geom_point(size = 7) + 
  # geom_curve(
  #   aes(x = -0.86, y = 0.5, xend = -0.5, yend = 0.86),
  #   arrow = arrow(length = unit(0.03, "npc"), type = "closed", end = "both"), col = "black"
  # ) +
  scale_color_manual(values = c("#377EB8", "#4DAF4A")) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave("society/schelling_baseline.pdf", plot = p, width = 6, height = 6)

#Step 1
p1 <- df %>% ggplot(aes(x = x, y = y, color = as.factor(pop1))) + 
  geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = FALSE) +
  geom_point(size = 7) + 
  geom_curve(
    aes(x = -0.86, y = 0.5, xend = -0.5, yend = 0.86),
    arrow = arrow(length = unit(0.03, "npc"), type = "closed", end = "both"), col = "black"
  ) +
  scale_color_manual(values = c("#377EB8", "#4DAF4A")) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave("society/schelling_1.pdf", plot = p1, width = 6, height = 6)


#Step 2
p2 <- df %>% ggplot(aes(x = x, y = y, color = as.factor(pop2))) + 
  geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = FALSE) +
  geom_point(size = 7) +
  geom_curve(
    aes(x = -0.0183, y = -1, xend = -0.5, yend = -0.86),
    arrow = arrow(length = unit(0.03, "npc"), type = "closed", end = "both"), col = "black"
  ) +
  scale_color_manual(values = c("#377EB8", "#4DAF4A")) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave("society/schelling_2.pdf", plot = p2, width = 6, height = 6)

#Step 3
p3 <- df %>% ggplot(aes(x = x, y = y, color = as.factor(pop3))) + 
  geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = FALSE) +
  geom_point(size = 7) +
  geom_curve(
    aes(x = 0.86, y = -0.5, xend = -1, yend = -0.0122),
    arrow = arrow(length = unit(0.03, "npc"), type = "closed", end = "both"), col = "black"
  ) +
  scale_color_manual(values = c("#377EB8", "#4DAF4A")) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave("society/schelling_3.pdf", plot = p3, width = 6, height = 6)


p4 <- df %>% ggplot(aes(x = x, y = y, color = as.factor(pop4))) + 
  geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = FALSE) +
  geom_point(size = 7) +
  geom_curve(
    aes(x = -0.5, y = -0.86, xend = -0.86, yend = 0.5),
    arrow = arrow(length = unit(0.03, "npc"), type = "closed", end = "both"), col = "black"
  ) +
  scale_color_manual(values = c("#377EB8", "#4DAF4A")) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave("society/schelling_4.pdf", plot = p4, width = 6, height = 6)

p5 <- df %>% ggplot(aes(x = x, y = y, color = as.factor(pop5))) + 
  geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = FALSE) +
  geom_point(size = 7) +
  geom_curve(
    aes(x = -0.86, y = -.5, xend = 0.5, yend = .86),
    arrow = arrow(length = unit(0.03, "npc"), type = "closed", end = "both"), col = "black"
  ) +
  scale_color_manual(values = c("#377EB8", "#4DAF4A")) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave("society/schelling_5.pdf", plot = p5, width = 6, height = 6)

p6 <- df %>% ggplot(aes(x = x, y = y, color = as.factor(pop6))) + 
  geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = FALSE) +
  geom_point(size = 7) +
  # geom_curve(
  #   aes(x = -0.86, y = -.5, xend = 0.5, yend = .86),
  #   arrow = arrow(length = unit(0.03, "npc"), type = "closed", end = "both"), col = "black"
  # ) +
  scale_color_manual(values = c("#377EB8", "#4DAF4A")) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave("society/schelling_ideal.pdf", plot = p6, width = 6, height = 6)


p7 <- df %>% ggplot(aes(x = x, y = y, color = as.factor(pop))) + 
  geom_circle(aes(x0 = 0, y0 = 0, r = 1), inherit.aes = FALSE, col = "gray") +
  geom_text(aes(label = positions), size = 10, family="Times", fontface="bold") +
  # geom_curve(
  #   aes(x = -0.86, y = -.5, xend = 0.5, yend = .86),
  #   arrow = arrow(length = unit(0.03, "npc"), type = "closed", end = "both"), col = "black"
  # ) +
  scale_color_manual(values = c("#377EB8", "#4DAF4A")) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave("society/schelling_clock.pdf", plot = p7, width = 6, height = 6)
