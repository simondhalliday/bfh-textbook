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
gini_data <- read_excel("competitionmarkets/Gini Index Data/gini_data_USA.xlsx")

gini_data$gini <- as.numeric(as.character(gini_data$gini))

markup_data1 <- markup_data[-c(53:61), ]

markup_data2 <- markup_data1 %>%
  filter(year >= 1959.023)

gini_data1 <- gini_data %>%
  filter(year >= 1960)

CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")
# p <-  ggplot() +
#   geom_line(data = markup_data, aes(x=year, y=markup, color = "markup"), na.rm = TRUE) +
#   geom_line(data = gini_data, aes(x=Year, y= gini_index, color = "gini", group = 1), na.rm = TRUE) +
#   ylab("Gini Index and Markup Ratio") + 
#   xlab("Year") +
#   theme_minimal() +
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14)) +
#   guides(color = guide_legend(reverse = TRUE)) +
#   theme(legend.position='top', 
#         legend.justification='left',
#         legend.direction="horizontal", 
#         legend.title = element_blank()) + 
#   scale_colour_manual(values=c("#0868ac","#41ae76"))
# 
# p
# 
# colors = c("#7fc97f", "#beaed4")
# 
key = list(type = c("l"),
           text = list(label = c("Gini"), cex = 1.2), points = list(col= c("#0868ac"), pch = 1:5), column = 2,
           space = "top")

obj1 <- xyplot(gini ~ year, gini_data, type = "l", ylab = "Gini", par.settings = simpleTheme(col = 1),
               col = c("#0868ac"),key = list(type = c("l"),
                                                                                                      text = list(label = c("Gini", "Markup")), lines = list(col= c("#0868ac", "#FF801D")), column = 1,
                                                                                                      space = "right"))
obj2 <- xyplot(markup ~ year, markup_data, type = "l", ylab = "Markup Ratio", xlab = "Year", col = c("#FF801D"))
plot1 <- doubleYScale(obj2, obj1, add.ylab2 = TRUE)
print(plot1)


pdf(file = "capitalism/markup_and_gini.pdf", width = 8, height = 8)

obj1 <- xyplot(markup ~ year, data = markup_data2,
               xlab=list("Year", fontsize = 16),
               ylab = list("Markup Ratio", fontsize = 16),
               ylab.right = list("Gini Coefficient", fontsize = 16),
               par.settings = simpleTheme(col = 1),
               type = "l",
               col = "#E33024",
               lty=1,
               lwd = 1.5,
               scales=list(x=list(labels = c("1950", "1960", "1970", "1980","1990", "2000","2010"),
                                  cex=1,axs="r"), y=list(cex=6),tck = c(1,0)),
               key = list(type = c("l","l"),
                          lty = c(1,1),
                          text = list(label = c("Share Weighted Markup", "Gini")), lines = list(col= c("#E33024", CBCols[2])), column = 1,
                          x = 0.02, y = .9, fontsize = 16, lwd = 1.5))

obj2 <- xyplot(gini~year,data = gini_data1 ,type = "l",col=CBCols[2],
               lty=1,lwd = 1.5, 
               scales=list(x=list(labels = c("1950", "1960", "1970", "1980","1990", "2000","2010"),
                                  cex=2,axs="r"), y=list(cex=6)))

doubleYScale(obj1, obj2)
#rot=90
#space = "right"
# ----

dev.off()
