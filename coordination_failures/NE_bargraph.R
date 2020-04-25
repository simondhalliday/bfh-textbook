library(tidyverse)
par(mar =  c(12, 12, 4, 4))
x <- c(11:1)
utility <- c(18.37,21.30,25.00, 29.75, 36.00, 44.44,56.25, 73.47, 100.00, 144.00,225.00)
df <- tibble(x,utility)


plot1 <- df %>% 
  ggplot(aes(x = x, y = utility)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = "#377eb8" ) + 
  geom_text(aes(x=x,y=utility,label=utility), vjust = -0.4, cex = 6) + 
  geom_segment(aes(x=0.5, xend=11.5, y=20, yend=20), linetype=2) + 
  #geom_text(aes(x = point, y = Utility, label = Utility)) +
  #scale_x_discrete(labels = xaxislabs) + 
  scale_x_continuous(breaks = seq(0,11,1)) +
  scale_y_continuous(breaks = seq(0,240,20),
                     labels = seq(0,240,20),
                     limits = c(0,240)) +
  scale_fill_manual(values=c("#386cb0")) + theme_bw() + ylab("Utility, u") + xlab("Number of people, n") + 
  #annotate("text", x = 10.5, y = 50, label = "Fallback position = 20", cex = 6) + 
  theme(
        axis.title = element_text(size = 24),
        axis.title.y = element_text(size = 24),
        axis.text.y = element_text(size = 18),
        #legend.title = element_text(size = 16),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.x  = element_text(vjust = 0.5, size = 16))
plot1 



ggsave("coordination_failures/NE_bargraph.pdf", plot1, width = 11, height = 7)

# scale_fill_discrete(name = "", 
#                     breaks = c("ua", "ub", "totalu"), 
#                     labels = c(expression(paste("A's Utility, ", u^A)), expression(paste("B's Utility, ",u^B), "Total Utility"))
# ) +
#   scale_color_discrete(name = "", 
#                        breaks = c("ua", "ub", "totalu"), 
#                        labels = c(expression(paste("A's Utility, ", u^A)), expression(paste("B's Utility, ",u^B), "Total Utility"))
#   ) +
