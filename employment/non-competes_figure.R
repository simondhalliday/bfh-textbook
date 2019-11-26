library(tidyverse)
library(digitize)
library(jpeg)
library(scales)
library(openxlsx)

cal = ReadAndCal("Non-compete.jpg")

#<HS data 
data.points = DigitData(col = 'red')
HS = Calibrate(data.points, cal, 0, 1, 0, 0.45 )

#HS grad
data.points1 = DigitData(col = 'red')
HS_grad = Calibrate(data.points1, cal, 0, 1, 0, 0.45 )

#<1 year college
data.points2 = DigitData(col = 'red')
one_year_college = Calibrate(data.points2, cal, 0, 1, 0, 0.45 )

#>1 year college
data.points3 = DigitData(col = 'red')
one_year_college_more = Calibrate(data.points3, cal, 0, 1, 0, 0.45 )

#Associates
data.points4 = DigitData(col = 'red')
associates = Calibrate(data.points4, cal, 0, 1, 0, 0.45 )

#BA
data.points5 = DigitData(col = 'red')
BA = Calibrate(data.points5, cal, 0, 1, 0, 0.45 )

#MA
data.points6 = DigitData(col = 'red')
MA = Calibrate(data.points6, cal, 0, 1, 0, 0.45 )

#Prof degree 
data.points7 = DigitData(col = 'red')
Prof_degree = Calibrate(data.points7, cal, 0, 1, 0, 0.45 )

#Doctorate 
data.points8 = DigitData(col = 'red')
doctorate= Calibrate(data.points8, cal, 0, 1, 0, 0.45 )

#categorizing the data
HS$group <- "<HS"
HS_grad$group <- "HS grad"
one_year_college$group <- "<1 year college"
one_year_college_more$group <- ">1 year of college"
associates$group <- "Associates"
BA$group <- "BA"
MA$group <- "MA"
Prof_degree$group <- "Prof Degree"
doctorate$group <- "Doctorate"


data_final <- rbind(HS, HS_grad, one_year_college,one_year_college_more,associates,BA, MA, Prof_degree, doctorate)
data_final$group <- factor(data_final$group)


data_final_1 <- data_final %>%
  select(-x)

group=factor(c("<HS", "HS grad", "<1 year college",">1 year of college","Associates", "BA","MA","Prof Degree","Doctorate"),levels=c("<HS","HS Grad","<1 year of college",">1 year of college", "Associates", "BA", "MA", "Prof Degree", "Doctorate"))

write.xlsx(data_final_1, 'incidence_of_non_competes_data.xlsx')

incidence_of_non_competes <- read.xlsx('incidence_of_non_competes_data.xlsx')

p1 <- ggplot(incidence_of_non_competes, aes(x=group, y=y)) +
  geom_bar(stat = "identity") + 
  xlab("Education Level") +
  ylab("Incidence of Non-competes") +
  scale_y_continuous(breaks = seq(0, 0.45, by = 0.05), labels = percent) + 
  theme_bw()  

print(p1)



