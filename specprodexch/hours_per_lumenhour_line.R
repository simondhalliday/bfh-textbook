#Graph Designer: Scott Cohn 
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics


# Setup -------------------------------------------------------------------

library(shape)
library(tidyverse)
library(lubridate)
library(scales)

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

# Import Data -------------------------------------------------------------

lumen_data <- read_csv("specprodexch/lumen_data.csv")

lumen_data[["lnLaborHourPer1000LumenHours"]] <-log(lumen_data[["LaborHourPer1000LumenHours"]])


# Plot  -------------------------------------------------------------------

lumen_data <-
  lumen_data %>%
  mutate(
    millionLumen = 10000 * LaborHourPer1000LumenHours,
    logLabor = log(millionLumen)
  ) %>%
  filter(!Year %in% c(-100000, -40000, -1750))

lumen_data %>% ggplot(aes(
    x = reorder(Year, -LaborHourPer1000LumenHours),
    y = logLabor
  )) +
  #geom_line(group="identity", colour=COLB[4])  +
  geom_line(color = COLA[4], group = 1) +
  #scale_x_continuous(breaks = pretty(lumen_data[["Year"]], n = 10)) +
  theme_bw() +
  labs(x = "Year", y = "Log of Hours of Work per 10,000,000 Lumen Hours")  +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    axis.title.y = element_text(size = 16, vjust = 0.5),
    axis.title.x = element_text(size = 16, vjust = -1),
    legend.text = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) 

# Save as PDF -------------------------------------------------------------

ggsave("specprodexch/hours_per_lumenhour_line.pdf", width = 7, height = 7)





