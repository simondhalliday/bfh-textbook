library(tidyverse)
library(data.table)

dates <- c(1975, 1985, 1995, 2005, 2015)
tangible <- c(83, 68, 32, 20, 13)
intangible <- c(17, 32, 68, 80, 87)
df <- data.frame(dates, tangible, intangible)
df2 <- 
  df %>% 
  gather(type, value, -dates) %>% 
  mutate(dates = factor(dates))

intangible <- 
  df2 %>% 
  ggplot(aes(x = dates, y = value, fill = type)) + 
  geom_bar(stat = "identity", position = "fill") + 
  xlab("Year") + 
  ylab("Percentage share") + 
  scale_fill_brewer( type = "qual", palette = "Paired", 
                     breaks = c ("tangible", "intangible"),
                     labels = c ("Tangible", 
                                 "Intangible"), 
                     name = "Type of asset") +
  theme_bw()

pdf(file = "capitalism/intangible.pdf", width = 4, height = 3)
intangible
dev.off()
