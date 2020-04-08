library(tidyverse)

#data for the figure 
x <- c("Nash Equilibrium", "Impartial Spectator", "Private Ownership","Nash Equilibrium", "Impartial Spectator", "Private Ownership")
y1 <- c(144,150,280,0,0,20)
y2 <- c("h=12","h=10","h=10",NA,NA,"h=10")
y3 <- c("a","a","a","b","b","b")

df <- tibble(x, y1, y2, y3)

df$x <- factor(df$x,levels = c("Private Ownership","Impartial Spectator","Nash Equilibrium"))


#figure 
plot1 <- df %>% 
  ggplot(aes(x = x, y = y1)) +
  geom_bar(aes(group = y3, fill = y3), stat = "identity", position = position_dodge())  +
  scale_y_continuous(breaks = seq(0,300,50),
                     labels = seq(0,300,50),
                     limits = c(0,300)) +
  scale_fill_manual(values=c("#386cb0","#41AE76"), 
                    name = "",
                    breaks = c("a","b"), 
                    labels = c(expression(paste("A's Utility, ", u^A)), expression(paste("B's Utility, ", u^B))))+
  #xlab("Point in the Edgeworth Box") + 
  theme_bw() +
  ylab("Utility") +
  xlab("") +
  theme(legend.position = "right",
        legend.text.align = 0,
        axis.title.y = element_text(size=20),
        axis.title.x = element_text(size=20),
        axis.text.y = element_text(size = 18),
        #legend.title = element_text(size = 16),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.x  = element_text(vjust = 0.5, size = 18)) + 
  geom_text(
    aes(x = x, y = y1, label = y2, group = y3), 
    hjust = -0.5, size = 6,
    position = position_dodge(width = 1),
    inherit.aes = TRUE
  ) + 
  coord_flip() 
plot1 



ggsave("coordination_failures/figureA.pdf", plot1, width = 13, height = 7)

