install.packages("WDI")
install.packages("tidyr")
install.packages("ggplot2")
library(ggplot2)
Data<-Data_Extract_From_World_Development_Indicators_2
Country<-Data$`Country Code`
Gini_Coef_2014<-Data$`2014 [YR2014]`[Data_Extract_From_World_Development_Indicators_2$`2014 [YR2014]`>=1]
print(Gini_Coef_2014)
pdf(file = "capitalism/gini_plot.pdf", width = 8, height = 6)
ggplot(Data_Extract_From_World_Development_Indicators_2, 
       aes(x=Country, y=Gini_Coef_2014))
+geom_bar()
dev.off()


