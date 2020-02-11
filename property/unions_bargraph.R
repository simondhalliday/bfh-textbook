library(tidyverse)
ua <- c(400, 652, 508, 540, 548)
ub <- c(256, 256, 400, 351, 360)
point <- c("z", "ta", "tb", "b", "a")
totalu <- c(656, 908, 908, 891, 908)
df <- tibble(point, ua, ub, totalu)
dfnar <- 
  df %>% 
  gather(type, Utility, -point) %>% 
  arrange(point)
xaxislabs <- c("a",  "b",  expression(paste(t^A)), expression(paste(t^B)), "z")
x1 <- c("Endowment allocation,\n no trade, z")
x2 <- c(expression(paste("A has TIOLI power,", t^A)))
x3 <- c(expression(paste("B has TIOLI power,", t^B)))
x5 <- c("Negotiated allocation \n after legislation, a")
dfplot <-
  dfnar %>%
  mutate(point = factor(point, levels = c("z", "ta", "tb", "b", "a")),
         type = factor(type, levels = c("ub","ua", "totalu")))

plot1 <- dfnar %>% 
  ggplot(aes(x = point, y = Utility)) +
  geom_bar(aes(group = type, fill = type), stat = "identity", position = position_dodge()) + 
  #geom_text(aes(x=point,y=Utility,label=Utility),vjust=90) + 
  #geom_text(aes(x = point, y = Utility, label = Utility)) +
  #scale_x_discrete(labels = xaxislabs) + 
  scale_x_discrete(labels=c("z" = x1, "ta" = x2,
                            "tb" = x3, "b" = " Legislated hours\n and wages, b", "a" = x5)) +
  scale_y_continuous(breaks = seq(0,1000,200),
                   labels = seq(0,1000,200),
                   limits = c(0,1000)) +
  scale_fill_manual(values=c("#386cb0","#41AE76","#FFEF66"), 
                    name = "Type",
                    breaks = c("ub", "ua", "totalu"), 
                    labels = c(expression(paste("B's Utility,", u^B)), expression(paste("A's Utility,", u^A)), "Total Utility"))+
  #xlab("Point in the Edgeworth Box") + 
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 18),
        #legend.title = element_text(size = 16),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.x  = element_text(vjust = 0.5, size = 18)) + 
  geom_text(
    aes(x = point, y = Utility, label = Utility, group = type), 
    hjust = -0.5, size = 4,
    position = position_dodge(width = 1),
    inherit.aes = TRUE
  ) + 
  coord_flip() 
plot1 



ggsave("property/unions_bargraph.pdf", plot1, width = 10, height = 7)

# scale_fill_discrete(name = "", 
#                     breaks = c("ua", "ub", "totalu"), 
#                     labels = c(expression(paste("A's Utility, ", u^A)), expression(paste("B's Utility, ",u^B), "Total Utility"))
# ) +
#   scale_color_discrete(name = "", 
#                        breaks = c("ua", "ub", "totalu"), 
#                        labels = c(expression(paste("A's Utility, ", u^A)), expression(paste("B's Utility, ",u^B), "Total Utility"))
#   ) +
