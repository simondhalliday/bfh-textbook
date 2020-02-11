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

ag_data <- read_csv("specprodexch/ag_data.csv")

ag_data[["lnHoursPerBushel"]] <- log(ag_data[["HoursPerBushel"]])
ag_data[["lnBushelsPerHourLabor"]] <- log(ag_data[["BushelsPerHourLabor"]])
ag_data[["lnHoursPer100Bushels"]] <- log(ag_data[["HoursPer100Bushels"]])


# Plot --------------------------------------------------------------------

ag_data %>% ggplot(mapping = aes(x = Year, y = lnHoursPer100Bushels)) +  
    labs(x = "Year", y = "Natural Log of Labor Hours Per 100 Bushels of Wheat") + 
    geom_line(color = COLB[4]) + 
    scale_x_continuous(breaks = pretty(ag_data[["Year"]], n = 10)) +
    theme_bw() + 
    ylim(0, 6) + 
    theme(legend.position = "none", 
          legend.title = element_blank(), 
          axis.title.y = element_text(size = 16, vjust = 0.5),
          axis.title.x = element_text(size = 16, vjust = -1),
          legend.text = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          panel.grid.minor.x = element_blank()
    ) 

# Save as PDF -------------------------------------------------------------

ggsave("specprodexch/loghours_per_100bushel_line.pdf", width = 7, height = 7)



