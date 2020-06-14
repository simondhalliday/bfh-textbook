#' Graph Designer(s): Simon Halliday, Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library("tidyverse")
library("dplyr")
library("ggthemes")

GDPData <- read_csv("capitalism/data/GDPData.csv")
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)

# Wrangle -----------------------------------------------------------------

# GDP2 <- 
#   GDPData %>%
#   mutate(China = parse_number(`China GDP per capita`), 
#          Britain = parse_number(`Britain GDP per capita`), 
#          Italy = parse_number(`Italy GDP per capita`), 
#          Japan = parse_number(`Japan GDP per capita`), 
#          India = parse_number(`India GDP per capita`)) 
GDP2 <- 
  GDPData %>%
  rename("China" = "China GDP per capita", 
         "Britain" = "Britain GDP per capita", 
         "Italy" = "Italy GDP per capita", 
         "Japan" = "Japan GDP per capita", 
         "India" = "India GDP per capita") 

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

# Plot --------------------------------------------------------------------

plotGDP <- GDP3 %>% 
  ggplot(aes(x = Year, y = gdp, color = country)) +
  geom_line() + 
  scale_color_brewer(palette = "Set1") + 
  labs(x = "Year", y = "GDP per capita", color = "Country") +
  # annotate(geom = "text", x = 1280, y = 5000, label = "Ibn Battuta \nvisits India \n(1334)") +
  # annotate(
  #   geom = "curve", x = 1280, y = 3500, xend = 1334, yend = 1000, 
  #   curvature = 0, arrow = arrow(length = unit(2, "mm"))
  # ) +
  # annotate(geom = "text", x = 1430, y = 5000, label = "Bubonic plague \nstrikes Europe \n(1350)") +
  # annotate(
  #   geom = "curve", x = 1430, y = 3500, xend = 1350, yend = 1000, 
  #   curvature = 0, arrow = arrow(length = unit(2, "mm"))
  # ) +
  # annotate(geom = "text", x = 1550, y = 8500, label = "Haiti & Cuba \nricher than \nUS colonies \n(1680)") +
  # annotate(
  #   geom = "curve", x = 1550, y = 6200, xend = 1680, yend = 1000, 
  #   curvature = 0, arrow = arrow(length = unit(2, "mm"))
  # ) +
  # annotate(geom = "text", x = 1680, y = 13000, label = "Adam Smith's \nWealth of Nations \npublished \n(1776)") +
  # annotate(
  #   geom = "curve", x = 1680, y = 11000, xend = 1776, yend = 2000, 
  #   curvature = 0, arrow = arrow(length = unit(2, "mm"))
  # ) +
  # annotate(geom = "text", x = 1800, y = 9000, label = "India \ncolonized \nby British \n(1793)") +
  # annotate(
  #   geom = "curve", x = 1800, y = 6700, xend = 1793, yend = 1000, 
  #   curvature = 0, arrow = arrow(length = unit(2, "mm"))
  # ) +
  # annotate(geom = "text", x = 1880, y = 13000, label = "Marx \npublishes \nCapital \n(1867)") +
  # annotate(
  #   geom = "curve", x = 1880, y = 10700, xend = 1880, yend = 3700, 
  #   curvature = 0, arrow = arrow(length = unit(2, "mm"))
  # ) +
  # annotate(geom = "text", x = 1900, y = 17000, label = "End of Soviet rule; \nmarket reforms in \nChina & Russia \n(1989)") +
  # annotate(
  #   geom = "curve", x = 1920, y = 15000, xend = 1989, yend = 3700, 
  #   curvature = -0.1, arrow = arrow(length = unit(2, "mm"))
  # ) +
  # xlim(1250, max(GDP3$Year)) + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.1, 0.85),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 16))

ggsave("capitalism/gdp.pdf", height = 7, width = 9, units = "in")


