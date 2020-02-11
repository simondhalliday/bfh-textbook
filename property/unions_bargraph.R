library(tidyverse)
ua <- c(400, 652, 400, 540, 548)
ub <- c(256, 256, 508, 351, 360)
point <- c("z", "ta", "tb", "b", "a")
totalu <- c(656, 908, 908, 891, 908)
df <- tibble(point, ua, ub, totalu)
dfnar <- 
  df %>% 
  gather(type, Utility, -point) %>% 
  arrange(point)
xaxislabs <- c("a",  "b",  expression(paste(t^A)), expression(paste(t^B)), "z")
x1 <- c(expression(paste("Endowment allocation,no trade,", bold(z))))
x2 <- c(expression(paste("Employer A has TIOLI power, ", bold(t^A))))
x3 <- c(expression(paste("Union B has TIOLI power, ", bold(t^B))))
x5 <- c(expression(paste("Negotiated allocation after legislation,",bold(a))))
dfplot <-
  dfnar %>%
  mutate(point = factor(point, levels = c("z", "ta", "tb", "b", "a")),
         type = factor(type, levels = c("ub","ua", "totalu")))

dfnar$type <- factor(dfnar$type, levels = c("totalu","ub","ua"))

plot1 <- dfnar %>% 
  ggplot(aes(x = point, y = Utility)) +
  geom_bar(aes(group = type, fill = type), stat = "identity", position = position_dodge()) + 
  #geom_text(aes(x=point,y=Utility,label=Utility),vjust=90) + 
  #geom_text(aes(x = point, y = Utility, label = Utility)) +
  #scale_x_discrete(labels = xaxislabs) + 
  scale_x_discrete(labels=c("z" = x1, "ta" = x2,
                            "tb" = x3, "b" = expression(paste("Legislated hours and wages,",bold(b))), "a" = x5)) +
  scale_y_continuous(breaks = seq(0,1000,200),
                   labels = seq(0,1000,200),
                   limits = c(0,1000)) +
  scale_fill_manual(values=c("#386cb0","#41AE76","#FFEF66"), 
                    name = "Type",
                    breaks = c("ua", "ub", "totalu"), 
                    labels = c(expression(paste("A's Utility, ", u^A)), expression(paste("B's Utility, ", u^B)), paste("Total Utility")))+
  #xlab("Point in the Edgeworth Box") + 
  theme_bw() +
  theme(legend.position = "right",
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
    hjust = -0.5, size = 4,
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
