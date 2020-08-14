library(tidyverse)
par(mar =  c(12, 12, 4, 4))
ua <- c(225, 144, 150, 112, 150,156,145)
ub <- c(NA, 144, 150, 188, 150,144,139)
point <- c("1", "2", "3", "4", "5","6","7")
totalu <- c(NA, 288, 300, 300, 300,300,284)
df <- tibble(point, ua, ub, totalu)
dfnar <- 
  df %>% 
  gather(type, Utility, -point) %>% 
  arrange(point)
#xaxislabs <- c("a",a  "b",  expression(paste(t^A)), expression(paste(t^B)), "z")
x1 <- c(expression(paste("Aram fishes alone; \n Bina is a farmer \n not a fisherman (15 Hours)")))
x2 <- c(expression(paste("Nash equilibrium of the symmetric  game \n (24 Hours)")))
x3 <- c(expression(paste("Social optimum (Impartial Spectator \n implemented) \n (20 Hours)")))
x4 <- c(expression(paste("Bina owns the lake (both permits and \n employment) (20 Hours)")))
x5 <- c(expression(paste("Optimal tax on fishing time (20 Hours)")))
x6 <- c(expression(paste("Aram is first mover  with TIOLI power \n (19.96 Hours)")))
x7 <- c(expression(paste("Aram is first mover with fishing time \n setting power (25.64 Hours)")))
dfplot <-
  dfnar %>%
  mutate(point = factor(point, levels = c("1", "2", "3", "4", "5","6","7")),
         type = factor(type, levels = c("ua","ub", "totalu")))

dfnar$point <- factor(dfnar$point, levels = c("7", "6", "5", "4", "3","2","1"))

#colors Vermiliion = rgb(213/255,94/255,0)
#blue = rgb(0,114/255,178/255)
#blue-ish green rgb(0, 158/255,115/255)
# rgb(230/255, 159/255, 0)
#https://jfly.uni-koeln.de/color/
#
plot1 <- dfnar %>% 
  ggplot(aes(x = point, y = Utility)) +
  geom_bar(aes(group = type, fill = type), stat = "identity", position = position_dodge()) + 
  #geom_text(aes(x=point,y=Utility,label=Utility),vjust=90) + 
  #geom_text(aes(x = point, y = Utility, label = Utility)) +
  #scale_x_discrete(labels = xaxislabs) + 
  scale_x_discrete(labels=c("1" = "Aram fishes alone; Bina is a farmer  \n not a fisherman (15 Hours)", "2" = "Nash equilibrium of the  symmetric \n game (24 Hours)",
                            "3" = "Social optimum (Impartial Spectator \n implemented) (20 Hours)", "4" = x4, "5" = x5, "6" = x6, "7" = x7)) +
  scale_y_continuous(breaks = seq(0,350,50),
                     labels = seq(0,350,50),
                     limits = c(0,350)) +
  scale_fill_manual(values=c("#009E73","#0072B2","#E69F00"), 
                    name = "Type",
                    breaks = c("ua", "ub", "totalu"), 
                    labels = c(expression(paste("A's Utility, ", u^A)),
                               expression(paste("B's Utility, ", u^B)), 
                               paste("Total Utility")))+
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
    hjust = -0.5, size = 6,
    position = position_dodge(width = 1),
    inherit.aes = TRUE
  ) + 
  coord_flip() 
plot1 



ggsave("coordination_failures/bargraph.pdf", plot1, width = 11, height = 7)

# scale_fill_discrete(name = "", 
#                     breaks = c("ua", "ub", "totalu"), 
#                     labels = c(expression(paste("A's Utility, ", u^A)), expression(paste("B's Utility, ",u^B), "Total Utility"))
# ) +
#   scale_color_discrete(name = "", 
#                        breaks = c("ua", "ub", "totalu"), 
#                        labels = c(expression(paste("A's Utility, ", u^A)), expression(paste("B's Utility, ",u^B), "Total Utility"))
#   ) +
