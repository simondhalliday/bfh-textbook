#Graph Designer: Harriet Brookes-Gray 
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
library(tidyverse)
library(lubridate)
library(readxl)
library(readr)
library(latticeExtra)


markup_data <- read_excel("competitionmarkets/loecker_barkai/LBdf_data.xlsx")
gini_data <- read_excel("competitionmarkets/Gini Index Data/US_gini_index.xlsx")

gini_data$`Gini Index` <- as.numeric(as.character(gini_data$`Gini Index`))

p <-  ggplot() +
  geom_line(data = markup_data, aes(x=year, y=markup, color = "markup"), na.rm = TRUE) +
  geom_line(data = gini_data, aes(x=Year, y= gini_index, color = "gini", group = 1), na.rm = TRUE) +
  ylab("Gini Index and Markup Ratio") + 
  xlab("Year") +
  theme_minimal() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction="horizontal", 
        legend.title = element_blank()) + 
  scale_colour_manual(values=c("#0868ac","#41ae76"))

p

obj1 <- xyplot(gini_index ~ Year, gini_data, type = "l")
obj2 <- xyplot(markup ~ year, markup_data, type = "l")
plot1 <- doubleYScale(obj2, obj1,add.ylab2 = TRUE)

#Save plot to PDF
ggsave(p, filename = "markup_and_gini_coefficient1.pdf", 
       path = "competitionmarkets",
       width = 7, height = 7, units = "in")


# ----

dev.off()