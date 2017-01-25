library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(mosaic)
library(haven)
library(readxl)
library(zoo)
GDPdata <- read_excel("what_can_markets_do/mpd_2013-01.xlsx", col_names = TRUE)

names(GDPdata) <- gsub(" ", "_", names(GDPdata))
valid_column_names <- make.names(names=names(GDPdata), unique=TRUE, allow_ = TRUE)
names(GDPdata) <- valid_column_names

ColdWar <- 
  GDPdata %>%
  #select(Date, USA, F._USSR_, S._Korea_, Brazil_) %>%
  select(Date, USA, F._USSR_) %>% 
  #rename(USSR = F._USSR_, date = Date, 
  #       S.Korea = S._Korea_, Brazil = Brazil_) %>%
  rename(USSR = F._USSR_, date = Date) %>%
  filter(date > 1899) %>%
  gather(country, pcgdp, -date)
ColdWar <- 
  ColdWar %>%
  mutate(pcgdp = na.approx(ColdWar$pcgdp)) %>%
  mutate(period = ifelse(date > 1928 & date < 1940, 1, 
                         ifelse(date > 1939 & date < 1946, 2, NA)
                         )
         )
         
GreatDepression <- 
  ColdWar %>% 
  filter(date > 1928 & date < 1940)
WW2 <- 
  ColdWar %>% 
  filter(date > 1938 & date < 1946)

CWPlot <- 
  ColdWar %>% 
  ggplot(aes(x = date, y = pcgdp, group = country)) +
  geom_line(aes(color = country)) +
  scale_color_discrete("Country") +
  scale_x_continuous(breaks = round(seq(min(ColdWar$date), max(ColdWar$date), by = 10),1)) +
  ylab("Per Capita GDP (1990 International $)") +
  xlab("Year") +
  annotate("text", x = 1928, y = 4000, label = "First 5-year plan") +
  geom_segment(aes(x = 1928, y = 3400, xend = 1928, yend = 1500), 
               size = 0.7, arrow = arrow(type = "closed", 
                                         length = unit(0.25, "cm"), 
                                         angle = 25)) +
  annotate("text", x = 1933, y = 11000, label = "Great") +
  annotate("text", x = 1933, y = 10000, label = "depression") +
    geom_segment(aes(x = 1933, y = 9500, xend = 1933, yend = 5200), 
               size = 0.7, arrow = arrow(type = "closed", 
                                         length = unit(0.25, "cm"), 
                                         angle = 25)) +
  annotate("text", x = 1945, y = 5800, label = "WWII Ends") +
  geom_segment(aes(x = 1945, y = 5300, xend = 1945, yend = 2200), 
               size = 0.7, arrow = arrow(type = "closed", 
                                         length = unit(0.25, "cm"), 
                                         angle = 25)) +
  annotate("text", x = 1959, y = 16000, label = "Kitchen Debates") +
  geom_segment(aes(x = 1959, y = 15500, xend = 1959, yend = 11500), 
               size = 0.7, arrow = arrow(type = "closed", 
                                         length = unit(0.25, "cm"), 
                                         angle = 25)) +
  annotate("text", x = 1991, y = 11000, label = "Communism Ends,") +
  annotate("text", x = 1991, y = 10000, label = "Capitalism Begins") +
  geom_segment(aes(x = 1991, y = 9400, xend = 1991, yend = 7000), 
              size = 0.7, arrow = arrow(type = "closed", 
                                        length = unit(0.25, "cm"), 
                                        angle = 25)) +
  theme_bw() 

CWPlot + 
  geom_ribbon(data = GreatDepression, 
              aes(ymin = 0, ymax = pcgdp), 
              fill = "blue2", alpha = 0.75) +
  geom_ribbon(data = WW2, 
              aes(ymin = 0, ymax = pcgdp), 
              fill = "grey70", alpha = 0.75)


pdf(file = "what_can_markets_do/usa_vs_ussr.pdf", width = 8, height = 6)
CWPlot
dev.off()
