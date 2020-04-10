library(tidyverse)

#data for the figure 
x <- c("Nash Equilibrium","TIOLI power" ,"Impartial Spectator10","Impartial Spectator4", "Private Ownership","Nash Equilibrium","TIOLI power" ,"Impartial Spectator10","Impartial Spectator4", "Private Ownership")
y1 <- c(21.3,153.2,40.91,90,300, NA,21.3,NA,NA,20)
y2 <- c("h=4.62","h=11.30","h=2.73","h=6","h=6",NA,"h=1.14",NA,NA,"h=6")
y3 <- c("a","a","a","a","a","b","b","b","b","b")

df <- tibble(x, y1, y2, y3)

df$x <- factor(df$x,levels = c("Private Ownership","Impartial Spectator","TIOLI power", "Nash Equilibrium"))

#figure 
plot1 <- dfs %>% 
  ggplot(aes(x = x, y = y1)) +
  geom_bar(aes(group = y3, fill = y3), stat = "identity", position = position_dodge())  +
  scale_y_continuous(breaks = seq(0,300,50),
                     labels = seq(0,300,50),
                     limits = c(0,300)) +
  scale_x_discrete(labels=c("Nash Equilibrium" = "Nash equilibrium, n=10 \n total hours:46.15", "TIOLI power" = "A has TIOLI power, n=10 \n total hours:21.56", "Impartial Spectator10" ="Impartial Spectator, n=10 \n total hours:27.27",
                            "Impartial Spectator" = "Impartial Spectator, n=4 \n total hours:24", "Private Ownership" = expression(paste("A owns the lake, n=4 \n total hours:24")))) +
  scale_fill_manual(values=c("#386cb0","#41AE76"), 
                    name = "",
                    breaks = c("a","b"), 
                    labels = c(expression(paste("A's Utility")), expression(paste("Others (not A) "))))+
  #xlab("Point in the Edgeworth Box") + 
  theme_bw() +
  ylab("Utility") +
  xlab("") +
  theme(legend.position = "right",
        legend.text.align = 0,
        axis.title.y = element_text(size=20),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 18),
        #legend.title = element_text(size = 16),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.x  = element_text(vjust = 0.5, size = 18)) + 
  geom_text(
    aes(x = x, y = y1, label = y2, group = y3), 
    hjust = -0.5, size = 6,
    position = position_dodge(width = 1),
    inherit.aes = TRUE) + 
  coord_flip() 

plot1 



ggsave("coordination_failures/figureC.pdf", plot1, width = 19, height = 7)

