library(tidyverse)
library(digitize)
library(jpeg)
library(scales)

pdf(file = "indmarketdemand/preventative_healthcare_demand.pdf", width = 9, height = 7)

#Using digitize to get data points 
#cal = ReadAndCal("indmarketdemand/health-pricing-takeup.jpg")

#Kenya bednets data 
#data.points = DigitData(col = 'red')
df_kenya_bednets = Calibrate(data.points, cal, 0.3, 5.9, 0.01, 1 )

#Soap in Uganda data 
#data.points1 = DigitData(col = 'red')
df_soap_uganda = Calibrate(data.points1, cal, 0.3, 5.9, 0.01, 1 )

#Deworming in keyna 
#data.points2 = DigitData(col = 'red')
df_deworming_kenya = Calibrate(data.points2, cal, 0.3, 5.9, 0.01, 1 )

#water filters Ghana
#data.points3 = DigitData(col = 'red')
df_water_filters_ghana = Calibrate(data.points3, cal, 0.3, 5.9, 0.01, 1 )

#Clorin in Kenya
#data.points4 = DigitData(col = 'red')
df_clorin_kenya = Calibrate(data.points4, cal, 0.3, 5.9, 0.01, 1 )

#Plot 
p = ggplot() + 
  geom_point(data = df_kenya_bednets, aes(x=x, y=y), color = "blue", shape = 15) +
  geom_line(data = df_kenya_bednets, aes(x=x, y=y), color = "blue") +
  geom_point(data = df_soap_uganda, aes(x=x, y=y), color = "sienna1", shape = 17) +
  geom_line(data = df_soap_uganda, aes(x=x, y=y), color = "sienna1") +
  geom_line(data = df_deworming_kenya, aes(x=x,y=y), color = "tomato3") + 
  geom_point(data = df_deworming_kenya, aes(x=x,y=y), color = "tomato3") +
  geom_line(data = df_water_filters_ghana, aes(x=x,y=y), color = "light blue") + 
  geom_point(data = df_water_filters_ghana, aes(x=x,y=y), color = "light blue") +
  geom_line(data = df_clorin_kenya, aes(x=x,y=y), color = "springgreen1") + 
  geom_point(data = df_clorin_kenya, aes(x=x,y=y), color = "springgreen1", shape = 2) +
  xlab("Price in USD") +
  ylab("Household Take-up Rate") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = percent) + 
  scale_x_continuous(breaks = seq(0, 6, by = 1), labels = dollar) +
  theme_minimal() + 
  ggtitle("Demand for Preventative Healthcare Products") +
  theme(plot.title = element_text(hjust = 0.5)) 

print(p)

dev.off()

