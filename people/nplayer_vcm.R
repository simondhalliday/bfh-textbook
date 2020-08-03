library(tidyverse)
contribute <- seq(-5,10,5)
dont <- seq(0,15,5)
df <- tibble(contribute, dont)
df2 <- df %>% gather(action, payoff)
df2 <- df2 %>% mutate(action = factor(action),
                      others = rep(seq(0,3,1),2))
vcmplot <- 
  df2 %>% 
  ggplot(aes(x = others, y = payoff, group = action, color = action, fill = action)) + 
  geom_bar(stat = "identity", width = 0.8, position = position_dodge(width = 0.95)) + 
  xlab("Number of others contributing") + 
  ylab("Payoff net of endowment, $") + 
  scale_fill_brewer(type = "qual", 
                    palette = "Accent",
                    name = "Action", 
                    breaks = c("contribute", "dont"), 
                    labels = c("Contribute", "Don't")) + 
  scale_color_brewer(type = "qual", 
                     palette = "Accent",
                     name = "Action", 
                     breaks = c("contribute", "dont"), 
                     labels = c("Contribute", "Don't")) +
  theme_bw()
vcmplot
ggsave("people/nplayer_vcm.pdf", vcmplot , width = 6, height = 4)
  
vcmlines <- 
  df2 %>% 
  ggplot(aes(x = others, y = payoff, group = action, color = action, fill = action)) + 
  geom_line(lwd = 0.8) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "black") +
  xlab("Number of others playing Contribute") + 
  ylab("Payoff net of endowment, $") + 
  scale_fill_brewer(type = "qual", 
                    palette = "Set1",
                    name = "Action", 
                    breaks = c("contribute", "dont"), 
                    labels = c("Contribute", "Don't contribute")) + 
  scale_color_brewer(type = "qual", 
                     palette = "Set1",
                     name = "Action", 
                     breaks = c("contribute", "dont"), 
                     labels = c("Contribute", "Don't contribute")) +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 17),
        axis.text = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.84),
        legend.text = element_text(size = 14),
        axis.text.y.right = element_text(margin = unit(c(5, 5, 5, 1), "mm")))
  
  
vcmlines
ggsave("people/nplayer_vcm_lines.pdf", vcmlines , width = 6, height = 4)


