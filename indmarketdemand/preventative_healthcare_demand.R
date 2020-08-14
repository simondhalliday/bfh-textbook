library(digitize)
library(jpeg)
library(scales)
library(openxlsx)
library(ggplot2)
library(tidyverse)
library(shape)
library(ggthemes)

#pdf(file = "indmarketdemand/preventative_healthcare_demand.pdf", width = 9, height = 7)

COLD <- c("#DA3030","#41ae76","#F7DE04", "#4eb3d3","#AE82FF","#386cb0","#F48318", "#6a51a3", "#FB6AAA","#66FFB7")

# #Using digitize to get data points 
# cal = ReadAndCal("health-pricing-takeup.jpg")
# 
# #Kenya bednets data 
# data.points = DigitData(col = 'red')
# df_kenya_bednets_pwomen = Calibrate(data.points, cal, 0.3, 5.9, 0.01, 1 )
# 
# #Soap in Uganda data 
# data.points1 = DigitData(col = 'red')
# df_soap_uganda = Calibrate(data.points1, cal, 0.3, 5.9, 0.01, 1 )
# 
# #Deworming in keyna 
# data.points2 = DigitData(col = 'red')
# df_deworming_kenya = Calibrate(data.points2, cal, 0.3, 5.9, 0.01, 1 )
# 
# #water filters Ghana
# data.points3 = DigitData(col = 'red')
# df_water_filters_ghana = Calibrate(data.points3, cal, 0.3, 5.9, 0.01, 1 )
# 
# #Clorin in Kenya
# data.points4 = DigitData(col = 'red')
# df_clorin_kenya = Calibrate(data.points4, cal, 0.3, 5.9, 0.01, 1 )
# 
# #Vitamins India
# data.points5 = DigitData(col = 'red')
# df_vita_india = Calibrate(data.points5, cal, 0.3, 5.9, 0.01, 1 )
# 
# #bednets Kenya 
# data.points6 = DigitData(col = 'red')
# df_bednets_kenya = Calibrate(data.points6, cal, 0.3, 5.9, 0.01, 1 )
# 
# #cement latrine slabs Tanzania 
# data.points7 = DigitData(col = 'red')
# df_latrine_tanzania = Calibrate(data.points7, cal, 0.3, 5.9, 0.01, 1 )
# 
# #vitamins Uganda 
# data.points8 = DigitData(col = 'red')
# df_vita_uganda = Calibrate(data.points8, cal, 0.3, 5.9, 0.01, 1 )
# 
# #plastic latrine slabs Tanzania 
# data.points9 = DigitData(col = 'red')
# df_latrice_tanzania_plastic = Calibrate(data.points9, cal, 0.3, 5.9, 0.01, 1 )
# 
# #soap guatemala
# data.points10 = DigitData(col = 'red')
# df_soap_guat = Calibrate(data.points10, cal, 0.3, 5.9, 0.01, 1 )
# 
# #clorin Zambia 
# data.points11 = DigitData(col = 'red')
# df_clorin_zambia = Calibrate(data.points11, cal, 0.3, 5.9, 0.01, 1 )
# 
# #vitamins Guatemala
# data.points12 = DigitData(col = 'red')
# df_vita_guat = Calibrate(data.points12, cal, 0.3, 5.9, 0.01, 1 )
# 
# df_clorin_kenya$group <- "$0.4 clorin (Kenya 2004)"
# df_deworming_kenya$group <- "$2 deworming (Kenya 2001)"
# df_kenya_bednets_pwomen$group <- "$7 bednets (Kenya 2007, pregnant women)"
# df_soap_uganda$group <- "$2 soap (Uganda 2009)"
# df_water_filters_ghana$group <- "$15 water filters (Ghana 2009)"
# df_vita_india$group <- "$1 vitamin (India 2009)"
# df_bednets_kenya$group <- "$7 bednets (Kenya 2007)"
# df_latrine_tanzania$group <- "$5 cement latrine slabs (Tanzania 2015)"
# df_vita_uganda$group <- "$2 vitamins (Uganda 2009)"
# df_latrice_tanzania_plastic$group <- "$15 plastic latrine slab (Tanzania 2015)"
# df_soap_guat$group <- "$2 soap (Guatemala 2009) "
# df_clorin_zambia$group <- "$0.26 clorin (Zambia 2006)"
# df_vita_guat$group <- "$2 vitamins (Guatemala 2009)"
# 
# data_final <- rbind(df_clorin_kenya, df_deworming_kenya, df_kenya_bednets_pwomen, df_soap_uganda, df_water_filters_ghana, df_vita_india, df_bednets_kenya, df_latrine_tanzania, df_vita_uganda, df_latrice_tanzania_plastic, df_soap_guat, df_clorin_zambia, df_vita_guat)
# data_final$group <- factor(data_final$group)
# 
# write.xlsx(data_final, 'preventative_healthcare.xlsx')

data_final_1 <- read.xlsx('indmarketdemand/preventative_healthcare.xlsx')

colnames(data_final_1)[3] <- "product"

data_final_2 <- 
  data_final_1 %>%
  filter(!product %in% c('Vitamins (Guatemala 2009)', 
                         'Vitamin (India 2009)', 
                         'Soap (Uganda 2009)', 
                         "Vitamins (Uganda 2009)", 
                         "Clorin (Kenya 2004)")
         )  


# p1 <- ggplot(data_final_1, aes(x=x, y=y, group=group, color=group)) +
#   geom_point() +
#   geom_line() + 
#   xlab("Price in USD") +
#   ylab("Household Take-up Rate") +
#   scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = percent) + 
#   scale_x_continuous(breaks = seq(0, 6, by = 1), labels = dollar) +
#   theme_minimal() + 
#   ggtitle("Demand for Preventative Healthcare Products") +
#   theme(plot.title = element_text(hjust = 0.5)) + 
#   theme(legend.position= c(0.9,2), 
#         legend.title=element_blank()) +
#   guides(colour=guide_legend(nrow=5)) 
# 
# print(p1)

p2 <- ggplot(data_final_2, aes(x = x, y = y, group=product, color = product)) +
  geom_point() + 
  geom_line(lwd = 0.7) + 
  xlab("Price in USD") +
  ylab("Household Take-up Rate") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = percent) + 
  scale_x_continuous(breaks = seq(0, 6, by = 1), labels = dollar) +
  theme_bw() + 
  theme(legend.position = c(0.77, 0.77)) +
  #guides(colour = guide_legend(override.aes=list(fill=NA),nrow = 2)) +
  labs(color = 'Healthcare Products') +
  theme(legend.title = element_blank()) +
  coord_flip() +
  scale_color_colorblind() + 
  theme(axis.title = element_text(size = 24),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 13),
        axis.text.x = element_text(size = 15, angle = 45, vjust = 0.5),
        panel.grid.minor = element_blank()
  )

print(p2)


# p3 <- ggplot(data_final_1, aes(x=x, y=y, group=group, color=group)) +
#   geom_point() + 
#   geom_line() + 
#   annotate("text", x = 5.779143389, y=0.34, label = "Plastic Latrine Slabs (Tanzania)", size = 3) +
#   annotate("text", x = 	5.884860335, y=0.08106436, label = "Water Filters (Ghana)", size = 3) +
#   annotate("text", x = 		4.085474860, y=0.12153465	, label = "Cement Latrine Slabs 
#   (Tanzania)", size = 3) +
#   annotate("text", x = 	2.886219739, y=0.14663366	, label = "Bednets 
# (Kenya)", size = 3) +
#   annotate("text", x = 0.550000000, y=0.02980198, label = "Clorin 
# (Kenya)", size = 3) +
#   annotate("text", x = 0.654283054, y=0.17173267, label = "Deworming 
# (Kenya)", size = 3) +
#   annotate("text", x = 	1.408826816, y=0.97933168, label = "Soap (Uganda)", size = 3) +
#   annotate("text", x = 	1.25, y=0.80846535, label = "Vitamins (Uganda)", size = 3) +
#   xlab("Price in USD") +
#   ylab("Household Take-up Rate") +
#   scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = percent) + 
#   scale_x_continuous(breaks = seq(0, 6, by = 1), labels = dollar) +
#   theme_bw() + 
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.position = "none",
#         legend.title = element_blank()
#         ) +
#   coord_flip() 
# 
# print(p3)
# p2
# #Plot 
# p = ggplot() + 
#   geom_point(data = df_kenya_bednets, aes(x=x, y=y), color = "blue", shape = 15) +
#   geom_line(data = df_kenya_bednets, aes(x=x, y=y), color = "blue") +
#   geom_point(data = df_soap_uganda, aes(x=x, y=y), color = "sienna1", shape = 17) +
#   geom_line(data = df_soap_uganda, aes(x=x, y=y), color = "sienna1") +
#   geom_line(data = df_deworming_kenya, aes(x=x,y=y), color = "tomato3") + 
#   geom_point(data = df_deworming_kenya, aes(x=x,y=y), color = "tomato3") +
#   geom_line(data = df_water_filters_ghana, aes(x=x,y=y), color = "light blue") + 
#   geom_point(data = df_water_filters_ghana, aes(x=x,y=y), color = "light blue") +
#   geom_line(data = df_clorin_kenya, aes(x=x,y=y), color = "springgreen1") + 
#   geom_point(data = df_clorin_kenya, aes(x=x,y=y), color = "springgreen1", shape = 2) +
#   xlab("Price in USD") +
#   ylab("Household Take-up Rate") +
#   scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = percent) + 
#   scale_x_continuous(breaks = seq(0, 6, by = 1), labels = dollar) +
#   theme_minimal() + 
#   ggtitle("Demand for Preventative Healthcare Products") +
#   theme(plot.title = element_text(hjust = 0.5)) 
# 
# print(p)

ggsave(p2, filename = "preventative_healthcare_demand.pdf", 
      path = "indmarketdemand",
      width = 9, height = 7, units = "in")

#dev.off()


