library(tidyverse)

hours <- c(12, 12, 5,5,7.5,7.5,5,5, 4.6, 4.6 ,2.7, 2.7, 2.7 , 2.7)
utility <- c(144, 144, 56.25, 56.25, 75, 75, 150, 56.25, 21.3, 21.3, 40.9, 40.9, 229.1, 20 )
type <- c("u^A","u^J", "u^A","u^J", "u^A","u^J", "u^A","u^J", "u^A","u^J", "u^A","u^J", "u^A","u^J")
group <- c("NE (n = 2)","NE (n = 2)", 
           "NE (n = 5)","NE (n = 5)", 
           "Social Optimum (n = 5)", "Social Optimum (n = 5)",
           "TIOLI (n = 5)", "TIOLI (n = 5)",
           "NE (n = 10)", "NE (n = 10)", 
           "Social Optimum (n = 10)","Social Optimum (n = 10)", 
           "Private Ownership (n = 10)", "Private Ownership (n = 10)"
           )
dfpool <- tibble(hours, utility, type, group)
dfpool <- 
  dfpool %>% 
  mutate(group = factor(group, levels = c("Private Ownership (n = 10)",
                                          "Social Optimum (n = 10)", 
                                          "NE (n = 10)",
                                          "TIOLI (n = 5)",
                                          "Social Optimum (n = 5)", 
                                          "NE (n = 5)",
                                          "NE (n = 2)"
                                          ))
         )

plotP <- 
  dfpool %>% 
  ggplot(aes(x = group, y = utility, fill = type)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  xlab("") +
  ylab("Individual utility") +
  scale_fill_manual(values = c("#4daf4a", "#984ea3"),
                    #values=c("#377eb8","#e41a1c", "#41AE76","#FFEF66","#386cb0"),
                    name = "", 
                    breaks = c("u^J","u^A"),
                    labels = c( "Others' utility", "A's utility")
  ) + 
  theme_bw() + 
  theme(legend.position = c(0.8, 0.95),
        legend.text.align = 0,
        axis.title = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 18),
        #legend.title = element_text(size = 16),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.x  = element_text(vjust = 0.5, size = 18)) + 
  geom_text(
    aes(x = group, y = utility, label = utility, group = type),
    col = "black",
    hjust = -0.5, size = 4,
    position = position_dodge(width = 1),
    inherit.aes = TRUE
  ) +
  ylim(0, 250) +
  coord_flip()
#plotP
ggsave("coordination_failures/bargraph_pooled_u.pdf", plotP, width = 12, height = 12)

plotPh <- 
  dfpool %>% 
  ggplot(aes(x = group, y = hours, fill = type)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  xlab("") +
  ylab("Individual utility") +
  scale_fill_manual(values = c("#4daf4a", "#984ea3"),
                    #values=c("#377eb8","#e41a1c", "#41AE76","#FFEF66","#386cb0"),
                    name = "", 
                    breaks = c("u^J","u^A"),
                    labels = c( "Others' hours", "A's hours")
  ) + 
  theme_bw() + 
  theme(legend.position = c(0.8, 0.8),
        legend.text.align = 0,
        axis.title = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 18),
        #legend.title = element_text(size = 16),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.x  = element_text(vjust = 0.5, size = 18)) + 
  geom_text(
    aes(x = group, y = hours, label = hours, group = type),
    col = "black",
    hjust = -0.5, size = 4,
    position = position_dodge(width = 1),
    inherit.aes = TRUE
  ) +
  ylim(0, 13) +
  coord_flip()
ggsave("coordination_failures/bargraph_pooled_hrs.pdf", plotPh, width = 12, height = 12)

