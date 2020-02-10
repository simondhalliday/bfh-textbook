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
dfplot <-
  dfnar %>%
  mutate(point = factor(point, levels = c("z", "ta", "tb", "b", "a")), 
         type = factor(type))



plot1 <- dfnar %>% 
  ggplot(aes(x = point, y = Utility)) +
  geom_bar(aes(group = type, fill = type, color = type), stat = "identity", position = position_dodge()) + 
  #geom_text(aes(x = point, y = Utility, label = Utility)) +
  scale_fill_discrete(name = "", 
                      breaks = c("ua", "ub", "totalu"), 
                      labels = c(expression(paste("A's Utility, ", u^A)), expression(paste("B's Utility, ",u^B), "Total Utility"))
                      ) +
scale_color_discrete(name = "", 
                    breaks = c("ua", "ub", "totalu"), 
                    labels = c(expression(paste("A's Utility, ", u^A)), expression(paste("B's Utility, ",u^B), "Total Utility"))
) +
  scale_x_discrete(labels = xaxislabs) + 
  scale_y_continuous(breaks = seq(0,1000,200), 
                   labels = seq(0,1000,200)) +
  xlab("Point in the Edgeworth Box") + 
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text.y = element_text(size = 18),
        #legend.title = element_text(size = 16),
        legend.text = element_text(size = 18),
        axis.text.x  = element_text(vjust = 0.5, size = 18 )
  ) + 
  coord_flip()
plot1
ggsave("property/unions_bargraph.pdf", plot1, width = 7, height = 7)
