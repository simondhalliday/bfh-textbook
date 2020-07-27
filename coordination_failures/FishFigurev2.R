require(shape)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(readxl)
library(digitize)

options(scipen=999) #Makes sure we don't have scienfitic notation in the figure 

fish_data <- read_excel("coordination_failures/FishData.xlsx")



fish_data_2 <- fish_data %>%
  filter(Year >= 1950)

#-----------GGplot---------
Fish_plot <- ggplot(fish_data, aes(x = Year, y = Tonnes)) +
  geom_line() +
  labs(y = "", color = "") + 
  scale_y_continuous(breaks = seq(0, 600000, by = 100000), limits = c(0,600000)) +
  scale_x_continuous(breaks = seq(1850, 2010, by = 10)) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() 
#theme(panel.grid.minor = element_blank(),
# legend.position = c(0.88, 0.83), 
# legend.title = element_text(size = 15), 
# axis.title.y = element_text(size = 17, vjust = 1),
# legend.text = element_text(size = 14),
# axis.text.x = element_text(size = 14, color = "black"),
# axis.text.y = element_text(size = 14, color = "black"),  
# axis.title.x = element_text(size = 17, vjust = -1)) 
# 
Fish_plot


#print(wealth_share_plot)

#Save plot to PDF
#ggsave(Fish_plot, filename = "Fish_plot.pdf", 
#       path = "coordination_failures",
#       width = 9, height = 7, units = "in")



#-----------Digitize---------

# Using digitize to get data points 
cal = ReadAndCal("coordination_failures/Fish.jpg")
# 
# Tonnes of fish
data.points = DigitData(col = 'red')
tonnes = Calibrate(data.points, cal, 0, 60, 0, 600000 )

# 
write.xlsx(tonnes, 'FishData_training.xlsx')

data_fish <- read.xlsx('coordination_failures/FishData_training.xlsx')

Years <- c("1950", "1960", "1970", "1980", "1990", "2000", "2010")

Fish_plot2 <- ggplot(data_fish, aes(x = x, y = y)) +
  geom_line(color = "#0868ac") +
  labs(y = "Landings (tons)", x = "Year", color = "") + 
  scale_color_brewer(palette = "Set1") +
  coord_cartesian(xlim = c(0, 60), ylim = c(0, 600000), expand = FALSE) + 
  scale_x_continuous(labels = Years) + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 17, vjust = 1),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),  
        axis.title.x = element_text(size = 17, vjust = -1)) 
# 
Fish_plot2


# dev.off()

