#' Graph Designer(s): Simon Halliday,  Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library("tidyverse")
library("dplyr")
library("ggthemes")

GDPData <- read_csv("capitalism/data/GDPData.csv")


# Wrangle -----------------------------------------------------------------

GDP2 <- 
  GDPData %>%
  mutate(China = parse_number(`China GDP per capita`), 
         Britain = parse_number(`Britain GDP per capita`), 
         Italy = parse_number(`Italy GDP per capita`), 
         Japan = parse_number(`Japan GDP per capita`), 
         India = parse_number(`India GDP per capita`)) 

GDP3 <- 
  GDP2 %>%
  select(Year, China, Britain, Italy, Japan, India) %>% 
  gather(country, gdp, China:India) # wide to long

history <- data.frame(year = c(1334, 1350, 1680, 1776, 1793, 1867, 1989), 
                      event = c("Ibn Battuta Visits India", 
                                "Bubonic Plague Strikes Europe", 
                                "Haiti & Cuba Richer than US Colonies", 
                                "Adam Smith's Wealth of Nations Published", 
                                "India Colonized by British", 
                                "Marx publishes Capital", 
                                "End of Soviet Rule; Market Reforms in China & Russia"), 
                      country = c("India", "Italy", "Britain", 
                                  "Britain", "India", "Britain", "China"))

plotGDP <- GDP3 %>% 
  ggplot(aes(x = Year, y = gdp, color = country)) +
  geom_line() + 
  scale_color_brewer(palette = "Set1") + 
  labs(x = "Year", y = "GDP per capita", color = "Country") +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.1, 0.9),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15))

plotGDP + 
  geom_vline(data = history, mapping = aes(xintercept = year), lty = 2, color = "gray") +
  geom_text(data = history, mapping = aes(x = year, y = 5000, label = event), color = 
              "black", size = 5, angle = 90, vjust = 1, hjust = 0)

ggsave("capitalism/gdp.pdf", height = 7, width = 9, units = "in")


