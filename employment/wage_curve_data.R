library(tidyverse)
library(mosaic)

Emp1 <- read.csv("employment/machin_data.csv")

Emp1 <- 
  Emp1 %>%
  mutate(totemp = 100 - uerate) %>%
  mutate_each(funs(as.character(.)), earnings) %>%
  mutate_each(funs(gsub(",", "", .)), earnings) %>%
  mutate_each(funs(as.numeric(.)), earnings)

pdf(file = "employment/wage_curve_data.pdf", width = 7, height = 5)

Emp1 %>% 
  ggplot(aes(x = totemp, y = earnings)) +
  geom_point(color = "#0868ac") +
  geom_line(color = "#0868ac") +
  xlim(80, 100) +
  ylab("Total earnings (2013 $)") +
  xlab("Hours of employment as a fraction of labor supply, H") +
  scale_y_continuous(breaks=c(42000,44000,46000,48000), labels=c("42,000","44,000", "46,000", "48,000"), limits = c(42000, 49000)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17)
        )
dev.off()