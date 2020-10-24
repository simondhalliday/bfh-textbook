library(tidyverse)
ua <- c(400, 652, 400, 540, 548)
ub <- c(256, 256, 508, 351, 360)
point <- c("z", "ta", "tb", "b", "a")
totalu <- c(656, 908, 908, 891, 908)
df <- tibble(point, ua, ub, totalu)
df <- df %>% 
  mutate(rentsA = ua - 400, 
         rentsB = ub - 256, 
         gains = totalu - 656,
         ) %>%
  select(-ua, -ub, -totalu) 

dfnar <- 
  df %>% 
  gather(type, Rents, -point) %>% 
  arrange(point) %>% 
  filter(point != "z")

xaxislabs <- c("a",  "b",  expression(paste(t^A)), expression(paste(t^B)))
#x1 <- c(expression(paste("Endowment allocation, no trade, ", bold(z))))
x2 <- c(expression(paste("Employer (A) has TIOLI power, ", bold(t^A))))
x3 <- c(expression(paste("Union (B) has TIOLI power, ", bold(t^B))))
x5 <- c(expression(paste("Negotiated allocation after legislation, ", bold(a))))
dfplot <-
  dfnar %>%
  mutate(point = factor(point, levels = c("a", "b", "tb", "ta")),
         type = factor(type, levels = c("gains","rentsB", "rentsA"))
         )

#dfnar$point <- factor(dfnar$point, levels = c("a", "b", "tb", "ta"))

plot1 <- dfplot %>% 
  ggplot(aes(x = point, y = Rents)) +
  geom_bar(aes(group = type, fill = type),width = 0.7, stat = "identity", position = position_dodge(width = 0.7)) + 
  #geom_text(aes(x=point,y=Utility,label=Utility),vjust=90) + 
  #geom_text(aes(x = point, y = Utility, label = Utility)) +
  #scale_x_discrete(labels = xaxislabs) + 
  scale_x_discrete(labels=c( "ta" = x2,
                            "tb" = x3, "b" = expression(paste("Legislated hours and wages, ",bold(b))), "a" = x5)) +
  scale_y_continuous(breaks = seq(0,300,100),
                     labels = seq(0,300,100),
                     limits = c(0,300)) +
  scale_fill_manual(values=c("#009E73","#0072B2","#E69F00"), 
                    name = "Type",
                    breaks = c("rentsA","rentsB", "gains"), 
                    labels = c(expression(paste("A's rents")), expression(paste("B's rents")), paste("Gains from exchange")))+
  #xlab("Point in the Edgeworth Box") + 
  theme_bw() +
  theme(legend.position = "top",
        legend.text.align = 0,
        axis.title = element_text(size = 22),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 18),
        #legend.title = element_text(size = 16),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.x  = element_text(vjust = 0.5, size = 18)) + 
  geom_text(
    aes(x = point, y = Rents, label = Rents, group = type), 
    hjust = -0.5, size = 6,
    position = position_dodge(width = 0.7),
    inherit.aes = TRUE
  ) + 
  coord_flip() 
plot1 



ggsave("property/unions_bargraph_rents.pdf", plot1, width = 11, height = 7)