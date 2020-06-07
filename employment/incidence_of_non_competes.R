library(tidyverse)
library(digitize)
library(jpeg)
library(scales)
library(openxlsx)
require(shape)

#The code that has been commented out below was used to create the data for the figure. 
#using digitize
# cal = ReadAndCal("Non-compete.jpg")
# 
# #<HS data 
# data.points = DigitData(col = 'red')
# HS = Calibrate(data.points, cal, 0, 1, 0, 0.45 )
# 
# #HS grad
# data.points1 = DigitData(col = 'red')
# HS_grad = Calibrate(data.points1, cal, 0, 1, 0, 0.45 )
# 
# #<1 year college
# data.points2 = DigitData(col = 'red')
# one_year_college = Calibrate(data.points2, cal, 0, 1, 0, 0.45 )
# 
# #>1 year college
# data.points3 = DigitData(col = 'red')
# one_year_college_more = Calibrate(data.points3, cal, 0, 1, 0, 0.45 )
# 
# #Associates
# data.points4 = DigitData(col = 'red')
# associates = Calibrate(data.points4, cal, 0, 1, 0, 0.45 )
# 
# #BA
# data.points5 = DigitData(col = 'red')
# BA = Calibrate(data.points5, cal, 0, 1, 0, 0.45 )
# 
# #MA
# data.points6 = DigitData(col = 'red')
# MA = Calibrate(data.points6, cal, 0, 1, 0, 0.45 )
# 
# #Prof degree 
# data.points7 = DigitData(col = 'red')
# Prof_degree = Calibrate(data.points7, cal, 0, 1, 0, 0.45 )
# 
# #Doctorate 
# data.points8 = DigitData(col = 'red')
# doctorate= Calibrate(data.points8, cal, 0, 1, 0, 0.45 )
# 
# #categorizing the data
# HS$group <- "<HS"
# HS_grad$group <- "HS grad"
# one_year_college$group <- "<1 year college"
# one_year_college_more$group <- ">1 year of college"
# associates$group <- "Associates"
# BA$group <- "BA"
# MA$group <- "MA"
# Prof_degree$group <- "Prof Degree"
# doctorate$group <- "Doctorate"
# 
# 
# data_final <- rbind(HS, HS_grad, one_year_college,one_year_college_more,associates,BA, MA, Prof_degree, doctorate)
# data_final$group <- factor(data_final$group)
# 
# 
# data_final_1 <- data_final %>%
#   select(-x)
# 
# group=factor(c("<HS", "HS grad", "<1 year college",">1 year of college","Associates", "BA","MA","Prof Degree","Doctorate"),levels=c("<HS","HS Grad","<1 year of college",">1 year of college", "Associates", "BA", "MA", "Prof Degree", "Doctorate"))
# 
# write.xlsx(data_final_1, 'incidence_of_non_competes_data.xlsx')

incidence_of_non_competes <- read.xlsx('employment/incidence_of_non_competes_data.xlsx')

incidence_of_non_competes$group <- factor(incidence_of_non_competes$group)

# p1 <- ggplot(incidence_of_non_competes, aes(x = group, y = y)) +
#   geom_bar(stat = "identity", fill = "#0868ac", color="#0868ac") + 
#   scale_x_discrete(limits=c("<HS","HS grad","<1 year of college",">1 year of college", "Associates", "BA", "MA", "Prof Degree", "Doctorate"), labels=c("<HS" = "<HS", "HS grad" = "HS grad", 
#                             "<1 year of college" = "<1 year of college", ">1 year of college" = ">1 year of college", "Prof Degree" = "Prof \n Degree")) +
incid_filter <- incidence_of_non_competes %>%
  filter(group %in% c("<HS", "HS grad", "BA", "Prof Degree"))

p0 <- ggplot(incid_filter, aes(x = group, y = y)) +
  geom_bar(stat = "identity", fill = "#0868ac", color="#0868ac") + 
  scale_x_discrete(limits=c("<HS","HS grad", "BA", "Prof Degree"), 
                   labels=c("<HS" = "<HS", "Less than\n high school" = "High school\n graduate", "BA" = "University\n graduate", "Prof Degree" = "Professional\n degree")) 
  
p1 <- p0 + xlab("Education Level") +
  ylab("Incidence of Non-Competes") +
  scale_y_continuous(breaks = seq(0, 0.45, by = 0.05), labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(size = 19),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        #axis.title = element_text(size = 24), 
        axis.title.x = element_text(size = 20, vjust = -0.5),
        axis.title.y = element_text(size = 20, vjust = 2),
        panel.grid.minor = element_blank())

print(p1)



#Save plot to PDF
ggsave(p1, filename = "incidence_of_non_competes.pdf", 
       path = "employment",
       width = 9, height = 7, units = "in")


dev.off()
