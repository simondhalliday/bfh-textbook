library(tidyverse)
economy <- c("Germany", "S.Korea", "Singapore", "Switzerland", "Sweden", "Israel", "Finland", "Denmark", "U.S.", "France", "Austria", "Japan", "Netherlands", "Belgium", "China", "Ireland", "Norway", "U.K.", "Italy", "Australia")
totalscore <- c(88.21, 88.16, 87.01, 85.67, 85.50, 85.03, 84.00, 83.22, 83.17, 82.75, 82.40, 82.31, 81.28, 79.93, 78.80, 78.65, 76.93, 76.03, 75.76, 74.13, 73.93, 73.11)
df <- tibble(point, ua, ub, totalu)
dfnar <- 
  df %>% 
  gather(type, economy, -point) %>% 
  arrange(point)
xaxislabs <- c("Economy")
dfplot <-
  dfnar %>%
  mutate(point = factor(point, levels = c("z", "ta", "tb", "b", "a")),
         type = factor(type, levels = c("ua","ub", "totalu")))

dfnar$point <- factor(dfnar$point, levels = c("a", "b", "tb", "ta", "z"))

plot1 <- dfnar %>% 
  ggplot(aes(x = economy, y = totalscore)) +
  geom_bar(aes(group = type, fill = type), stat = "identity", position = position_dodge()) + 
  #geom_text(aes(x=point,y=Utility,label=Utility),vjust=90) + 
  #geom_text(aes(x = point, y = Utility, label = Utility)) +
  #scale_x_discrete(labels = xaxislabs) + 
  #scale_x_discrete(labels=c("z" = x1, "ta" = x2,
   #                         "tb" = x3, "b" = expression(paste("Legislated hours and wages, ",bold(b))), "a" = x5)) +
  scale_y_continuous(breaks = seq(0,1000,200),
                     labels = seq(0,1000,200),
                     limits = c(0,1000)) +
  scale_fill_manual(values=c("#386cb0","#41AE76","#FFEF66"), 
                    name = "Type",
                    breaks = c("ub","ua", "totalu"), 
                    labels = c(expression(paste("B's Utility, ", u^B)), expression(paste("A's Utility, ", u^A)), paste("Total Utility")))+
  #xlab("Point in the Edgeworth Box") + 
  theme_bw() +
  theme(legend.position = "top",
        legend.text.align = 0,
        axis.title = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 18),
        #legend.title = element_text(size = 16),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.x  = element_text(vjust = 0.5, size = 18)) + 
  geom_text(
    aes(x = point, y = Utility, label = Utility, group = type), 
    hjust = -0.5, size = 5,
    position = position_dodge(width = 1),
    inherit.aes = TRUE
  ) + 
  coord_flip() 
plot1 



ggsave("property/unions_bargraph.pdf", plot1, width = 11, height = 7)

# scale_fill_discrete(name = "", 
#                     breaks = c("ua", "ub", "totalu"), 
#                     labels = c(expression(paste("A's Utility, ", u^A)), expression(paste("B's Utility, ",u^B), "Total Utility"))
# ) +
#   scale_color_discrete(name = "", 
#                        breaks = c("ua", "ub", "totalu"), 
#                        labels = c(expression(paste("A's Utility, ", u^A)), expression(paste("B's Utility, ",u^B), "Total Utility"))
#   ) +