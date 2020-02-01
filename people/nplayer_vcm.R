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
  
  
