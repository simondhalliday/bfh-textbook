library(tidyverse)
ua <- c(252, 0, 140, 148)
ub <- c(0, 252, 95, 104)
point <- c("ta", "tb", "b", "a")
totalu <- c(252, 252, 235, 252)
df <- tibble(point, ua, ub, totalu)
dfnar <- 
  df %>% 
  gather(type, Utility, -point) %>% 
  arrange(point)
xaxislabs <- c("a",  "b",  expression(paste(t^A)), expression(paste(t^B)), "z")
#x1 <- c(expression(paste("Endowment allocation, no trade, ", bold(z))))
x2 <- c(expression(paste("Employer (A) has TIOLI power, ", bold(t^A))))
x3 <- c(expression(paste("Union (B) has TIOLI power, ", bold(t^B))))
x5 <- c(expression(paste("Negotiated allocation after legislation, ", bold(a))))
dfplot <-
  dfnar %>%
  mutate(point = factor(point, levels = c("z", "ta", "tb", "b", "a")),
         type = factor(type, levels = c("ua","ub", "totalu")))

dfnar$point <- factor(dfnar$point, levels = c("a", "b", "tb", "ta", "z"))

plot1 <- dfnar %>% 
  ggplot(aes(x = point, y = Utility)) +
  geom_bar(aes(group = type, fill = type), stat = "identity", position = position_dodge()) + 
  #geom_text(aes(x=point,y=Utility,label=Utility),vjust=90) + 
  #geom_text(aes(x = point, y = Utility, label = Utility)) +
  #scale_x_discrete(labels = xaxislabs) + 
  scale_x_discrete(labels=c("z" = x1, "ta" = x2,
                            "tb" = x3, "b" = expression(paste("Legislated hours and wages, ",bold(b))), "a" = x5)) +
  scale_y_continuous(breaks = seq(0,1000,200),
                     labels = seq(0,1000,200),
                     limits = c(0,1000)) +
  scale_fill_manual(values=c("#009E73","#0072B2","#E69F00"), 
                    name = "Type",
                    breaks = c("ub","ua", "totalu"), 
                    labels = c(expression(paste("B's Utility, ", u^B)), expression(paste("A's Utility, ", u^A)), paste("Gains from exchange")))+
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



ggsave("property/unions_bargraph_rents.pdf", plot1, width = 11, height = 7)

# scale_fill_discrete(name = "", 
#                     breaks = c("ua", "ub", "totalu"), 
#                     labels = c(expression(paste("A's Utility, ", u^A)), expression(paste("B's Utility, ",u^B), "Total Utility"))
# ) +
#   scale_color_discrete(name = "", 
#                        breaks = c("ua", "ub", "totalu"), 
#                        labels = c(expression(paste("A's Utility, ", u^A)), expression(paste("B's Utility, ",u^B), "Total Utility"))
#   ) +
