library(tidyverse)
library(jtools)
library(huxtable)
library(ggrepel)
#setwd("/home/weikaichen/Documents/Code/")
COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")


# read the data
m20gbk <- read.csv("firmmarketsupply/data/M20GBK-man-hours.csv")

#rename the variables
names(m20gbk) <-c("Time","TotalProduct","ManHours")

# generate cumulative product
m20gbk <- mutate(m20gbk, CumulativeProduct = cumsum(TotalProduct))
m20gbk <- m20gbk[-1,]
# Plot for preliminary results
ggplotRegression <- function (fit) {
  require(ggplot2)
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = 'blue') +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

LogManHours <- log(m20gbk$ManHours)
LogTotalProduct <- log(m20gbk$TotalProduct)
LogCumulative <- log(m20gbk$CumulativeProduct)


fit1 <- lm(data = m20gbk, ManHours ~ TotalProduct)
fit2 <- lm(data = m20gbk, ManHours ~ CumulativeProduct)
fit3 <- lm(data = m20gbk, log(ManHours) ~ log(TotalProduct))
fit4 <- lm(data = m20gbk, log(ManHours) ~ log(CumulativeProduct))
fit5 <- lm(log(ManHours) ~ log(TotalProduct) + log(CumulativeProduct), data = m20gbk)


#export_summs(fit1,fit2,fit3,fit4,fit5,to.file = "pdf",file.name = "m20gbk.pdf")

# define a function of fit4
fit4.predict <- function(x) exp(fit4$coefficients[1])*x^(fit4$coefficients[2])

#Plot

fig1 <- ggplot(data = m20gbk, aes(x = TotalProduct, y = ManHours, label = Time)) +
  geom_point(aes( y = ManHours),size = 1.5, color = COLB[4]) +
  geom_text_repel(aes(label=Time),hjust=1, vjust=0.5, color = "black", segment.alpha = 0.5, segment.color = "darkgray") +
  labs(x = "Output", y = "Cost (Hours)") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) 
  
  
fig2 <- ggplot(data = m20gbk, aes(x = CumulativeProduct, y = ManHours, label = Time)) +
  labs(x = "Cumulative output in all previous months", y = "Cost (Hours)") +
  stat_function(fun = fit4.predict, color = COLA[4]) +
  geom_point(aes( y = ManHours), size = 1.5, color = COLB[4]) +
  geom_text_repel(aes(label=Time),hjust=1, vjust=0.5, color = "black", segment.alpha = 0.5, segment.color = "darkgray") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) 
  
  #geom_line(aes(x= CumulativeProduct, y = exp(predict(lm(log(ManHours)~ log(CumulativeProduct),data = m20gbk)))))
# Estimate
# save the figure
ggsave(plot =fig1, "firmmarketsupply/m20gbk_1.pdf", width = 8, height = 6, units = "in")
ggsave(plot =fig2, "firmmarketsupply/m20gbk_2.pdf", width = 8, height = 6, units = "in")
# ggsave(plot = ggplotRegression(fit1), "Monthly.pdf", width = 8, height = 6, units = "in")
# ggsave(plot = ggplotRegression(fit2), "Cumulative.pdf", width = 8, height = 6, units = "in")

