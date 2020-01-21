#Graph Designer: Harriet Brookes-Gray 
#Authors: Bowles and Halliday
#Title: Microeconomics: Competition, Conflict and Coordination

library(tidyverse)
library(digitize)
library(jpeg)
library(scales)
library(openxlsx)

#Using digitize to get data points 
cal = ReadAndCal("constrained_optimization/Change_HAPI_FLFP.jpg")
# 
data.points = DigitData(col = 'red')
Germany = Calibrate(data.points, cal, -0.3, -0.1, 5, 15 )
# 
data.points1 = DigitData(col = 'red')
France = Calibrate(data.points1, cal, -0.3, -0.1, 5, 15 )
# 
data.points2 = DigitData(col = 'red')
Ireland = Calibrate(data.points2, cal, -0.3, -0.1, 5, 15)

data.points3 = DigitData(col = 'red')
Belgium = Calibrate(data.points3, cal, -0.3, -0.1, 5, 15)

data.points4 = DigitData(col = 'red')
Italy = Calibrate(data.points4, cal, -0.3, -0.1, 5, 15)

data.points5 = DigitData(col = 'red')
Luxembourg = Calibrate(data.points5, cal, -0.3, -0.1, 5, 15)

data.points6 = DigitData(col = 'red')
UK = Calibrate(data.points6, cal, -0.3, -0.1, 5, 15)

data.points7 = DigitData(col = 'red')
Denmark = Calibrate(data.points7, cal, -0.3, -0.1, 5, 15)

data.points8 = DigitData(col = 'red')
Greece = Calibrate(data.points8, cal, -0.3, -0.1, 5, 15)

data.points9 = DigitData(col = 'red')
US = Calibrate(data.points9, cal, -0.3, -0.1, 5, 15)

data.points10 = DigitData(col = 'red')
Netherlands = Calibrate(data.points10, cal, -0.3, -0.1, 5, 15)

Germany$group <- "Germany"
France$group <- "France"
Ireland$group <- "Ireland"
Belgium$group <- "Belgium"
Italy$group <- "Italy"
Luxembourg$group <- "Luxembourg"
UK$group <- "UK"
US$group <- "US"
Denmark$group <- "Denmark"
Netherlands$group <- "Netherlands"
# 
data_final <- rbind(Germany,France, Ireland, Belgium, Italy, Luxembourg, UK, US, Denmark, Netherlands)
data_final$group <- factor(data_final$group)
# 
write.xlsx(data_final, 'constrained_optimization/FLFP_HAPI_countries.xlsx')
data_final_1 <- read.xlsx('constrained_optimization/FLFP_HAPI_countries.xlsx')


plot1 <- data_final_1 %>% ggplot(aes(x = x, y = y, group = group)) +
  geom_point(aes(color = group)) +
  geom_text(aes(label = group, color = group), vjust = -0.5, hjust = 1, size = 6) + 
  xlab("Changes in the HAPI") +
  ylab("Change in FLFP") +
  scale_y_continuous(breaks = seq(5, 15, by = 1), limits = c(5,15)) + 
  scale_x_continuous(breaks = seq(-0.3, -0.1, by = 0.05), limits = c(-0.3,-0.1)) +
  theme_bw() + 
  theme(legend.position = "none", 
        legend.title = element_blank(), 
        axis.title.y = element_text(size = 16, vjust = 0.5),
        axis.title.x = element_text(size = 16, vjust = -1),
        legend.text = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)
  ) 


print(plot1)

#Save plot to PDF
ggsave(plot1, filename = "FLFP_HAPI_countries.pdf", 
       path = "constrained_optimization",
       width = 7, height = 7, units = "in")




