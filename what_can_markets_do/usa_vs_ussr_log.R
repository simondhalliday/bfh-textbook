library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(mosaic)
library(haven)
library(readxl)
library(zoo)


#a random edit for Harrieet
GDPdata <- read_excel("what_can_markets_do/mpd_2013-01.xlsx", col_names = TRUE)

names(GDPdata) <- gsub(" ", "_", names(GDPdata))
valid_column_names <- make.names(names=names(GDPdata), unique=TRUE, allow_ = TRUE)
names(GDPdata) <- valid_column_names

ColdWar <- 
  GDPdata %>%
  select(Date, USA, F._USSR, S._Korea, Brazil) %>%#, Argentina_) %>%
  #select(Date, USA, F._USSR_) %>% 
  rename(USSR = F._USSR, date = Date, 
         S.Korea = S._Korea) %>% #, 
  #Argentina = Argentina_) %>%
  #rename(USSR = F._USSR_, date = Date) %>%
  filter(date > 1912) %>%
  gather(country, pcgdp, -date)
ColdWar <- 
  ColdWar %>%
  mutate(pcgdp = na.approx(ColdWar$pcgdp)) %>%
  mutate(period = ifelse(date > 1928 & date < 1940, 1, 
                         ifelse(date > 1939 & date < 1946, 2, NA)
  )
  )

ColdWar$trans_pcgdp <- log(ColdWar$pcgdp)

GreatDepression <- 
  ColdWar %>% 
  filter(date > 1928 & date < 1940)
WW2 <- 
  ColdWar %>% 
  filter(date > 1938 & date < 1946)

CWPlot <- 
  ColdWar %>% 
  ggplot(aes(x = date, y = trans_pcgdp, group = country)) +
  geom_line(aes(color = country)) +
  scale_color_discrete("Country") +
  scale_x_continuous(breaks = round(seq(min(ColdWar$date), max(ColdWar$date), by = 10),1)) +
  ylab("Natural Log of Per Capita GDP (1990 International $)") +
  xlab("Year") +
  annotate("text", x = 1928, y = 7.6, label = "First 5-year plan") +
  geom_segment(aes(x = 1928, y = 7.5, xend = 1928, yend = 7.222512),
               size = 0.7, arrow = arrow(type = "closed", 
                                         length = unit(0.25, "cm"), 
                                         angle = 25)) +
  annotate("text", x = 1933, y = 9.2, label = "Great") +
  annotate("text", x = 1933, y = 9, label = "depression") +
  geom_segment(aes(x = 1933, y = 8.95, xend = 1933, yend = 8.571550), 
                size = 0.7, arrow = arrow(type = "closed", 
                                        length = unit(0.25, "cm"), 
                                         angle = 25)) +
  annotate("text", x = 1945, y = 9.8, label = "WWII Ends") +
  geom_segment(aes(x = 1945, y = 9.7, xend = 1945, yend = 9.388083), 
                size = 0.7, arrow = arrow(type = "closed", 
                                         length = unit(0.25, "cm"), 
                                          angle = 25)) +
  annotate("text", x = 1959, y = 9.6, label = "Kitchen Debate") +
  geom_segment(aes(x = 1959, y = 9.5, xend = 1959, yend = 9.346359), 
               size = 0.7, arrow = arrow(type = "closed", 
                                          length = unit(0.25, "cm"), 
                                        angle = 25)) +
  annotate("text", x = 1991, y = 9.3, label = "Central Planning") +
  annotate("text", x = 1991, y = 9.1, label = "Ends") +
  geom_segment(aes(x = 1991, y = 9, xend = 1991, yend = 8.777963),
               size = 0.7, arrow = arrow(type = "closed",
                                         length = unit(0.25, "cm"),
                                         angle = 25)) +
  # annotate("text", x = 2008, y = 10.8, label = "Global") +
  # annotate("text", x = 2008, y = 10.7, label = "Financial") +
  # annotate("text", x = 2008, y = 10.6, label = "Crisis") +
  # geom_segment(aes(x = 2008, y = 10.5, xend = 1991, yend = 10.359815),
  #             size = 0.7, arrow = arrow(type = "closed",
  #                                       length = unit(0.25, "cm"),
  #                                       angle = 25)) +
  theme_bw() 

print(CWPlot)

# CWPlot + 
#   geom_ribbon(data = GreatDepression, 
#               aes(ymin = 0, ymax = pcgdp), 
#               fill = "blue2", alpha = 0.75) +
#   geom_ribbon(data = WW2, 
#               aes(ymin = 0, ymax = pcgdp), 
#               fill = "grey70", alpha = 0.75)
# 
# 
# pdf(file = "what_can_markets_do/usa_vs_ussr.pdf", width = 8, height = 6)
# CWPlot
# dev.off()
# 
# 
# 
# 
# ColdWar2 <- 
#   GDPdata %>%
#   select(Date, USA, F._USSR, S._Korea, Chile, Brazil) %>%#, Argentina_) %>%
#   #select(Date, USA, F._USSR_) %>% 
#   rename(USSR = F._USSR, date = Date, 
#          S.Korea = S._Korea) %>% #, 
#   #Argentina = Argentina_) %>%
#   #rename(USSR = F._USSR_, date = Date) %>%
#   filter(date > 1912) %>%
#   gather(country, pcgdp, -date)
# ColdWar2 <- 
#   ColdWar2 %>%
#   mutate(pcgdp = na.approx(ColdWar$pcgdp)) %>%
#   mutate(period = ifelse(date > 1928 & date < 1940, 1, 
#                          ifelse(date > 1939 & date < 1946, 2, NA)
#   )
#   )
# CWPlot2 <- 
#   ColdWar2 %>% 
#   ggplot(aes(x = date, y = pcgdp, group = country)) +
#   geom_line(aes(color = country)) +
#   scale_color_brewer( type = "qual", palette = "Set1", name = "Country") +
#   scale_x_continuous(breaks = round(seq(min(ColdWar$date), max(ColdWar$date), by = 20),1)) +
#   ylab("Per Capita GDP (1990 International $)") +
#   xlab("Year") +
#   theme_bw() 
# pdf(file = "capitalism/gdp_growth.pdf", width = 4, height = 3)
# CWPlot2
# dev.off()
