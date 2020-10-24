#Graph Designer: Harriet Brookes Gray 
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(scales)
library(openxlsx)
library(ggplot2)
library(tidyverse)
library(shape)


#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
grays <- gray.colors(25, start = 1, end = 0)


# #Data 
City <- c("Chicago", "Los Angeles", "New York City", "San Francisco")
Elasticity <- c(0.66, 0.33, 0.61, 0.52)
Uber_elasticity_data <- tibble(City, Elasticity)
#Uber_elasticity_data <- as.data.frame(Uber_elasticity_mat)

pdf(file = "indmarketdemand/uber_price_elasticities_bars.pdf", width = 9, height = 5)
#jpeg(file = "indmarketdemand/uber_price_elasticities_bars.jpg", width = 860, height = 480)
plot2 <- Uber_elasticity_data %>% 
  ggplot(aes(x = City, y = Elasticity)) + 
  geom_bar(stat = "identity", colour="#CD2825", fill = "#CD2825", width = 0.6) + 
  #scale_y_continuous(breaks = seq(0, 1, by = 0.2)) + 
  scale_y_continuous(breaks = seq(0, 0.75, 0.1),
                     labels = seq(0, 0.75, 0.1),
                     limits = c(0, 0.75)) +
  ylab("Price elasticity of demand") +
  xlab("") +
  theme_bw() + 
  theme(axis.title = element_text(size = 28),
        axis.text.y = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        panel.grid.minor = element_blank()
  ) + 
  #theme(legend.position = c(0.8,0.85),
  #legend.text.align = 0,
  #axis.title = element_text(size = 20),
  #axis.title.y = element_blank(),
  #axis.text.y = element_text(size = 18),
  #legend.title = element_text(size = 16),
  #legend.text = element_text(size = 18),
  #legend.title = element_text(size = 18),
  #axis.text.x  = element_text(vjust = 0.5, size = 18)) + 
  geom_text(
  aes(x = City, y = Elasticity, label = Elasticity),
  col = "black",
  hjust = -0.3, size = 8,
  position = position_dodge(width = 1),
  inherit.aes = TRUE
 ) +
#ylim(0, 160) +
coord_flip()
plot2


dev.off()


pdf(file = "indmarketdemand/uber_price_elasticities_curves.pdf", width = 10, height = 6)

par(mar =  c(6, 6, 2, 2))
xlims <- c(0, 3)
ylims <- c(2, 8)
xlims2 <- c(10, 0)
ylims2 <- c(15, 0)

elasticity1 <- function(x, A = 7, b = 1) {
  A*x^(1/b)
}

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = "Quantity of the good, X",
     ylab = "Price per unit of x, p", 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

ticksy <- seq(from = 0, to = 15, by = 1)
ylabels <- seq(from = 0, to = 15, by = 1)
ticksx <- seq(from = 0, to = 10, by = 1)
xlabels <- seq(from = 0, to = 10, by = 1)
axis(1, at = ticksx, pos = 2, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0, cex.axis = labelsize)

axis.break(axis = 2, breakpos = 2.2, pos = 0,
           bgcol = "white", breakcol = "black",
           style = "slash", brw = 0.02)

npts <- 500 
npts2 <- 501

#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
yy1 <- elasticity1(xx1, A = 5, b = -0.66)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
yy2 <- elasticity1(xx2, A = 5, b = -0.33)
xx3 <- seq(xlims[1], xlims[2], length.out = npts)
yy3 <- elasticity1(xx3, A = 5, b = -0.61)
xx4 <- seq(xlims[1], xlims[2], length.out = npts)
yy4 <- elasticity1(xx4, A = 5, b = -0.52)

lines(xx1,yy1, col = COL[3], lwd = graphlinewidth)
lines(xx2,yy2, col = COLB[2], lwd = graphlinewidth)
lines(xx3,yy3, col = COLA[3], lwd = graphlinewidth)
lines(xx4,yy4, col = COLB[4], lwd = graphlinewidth)

points(1, 5, pch = 16, col = "black", cex = 1.5)

legend(1.15, 8, 
       legend=c(expression(paste("Chicago, ", abs(eta) == 0.66)), expression(paste("Los Angeles, ", abs(eta) == 0.33)),expression(paste("New York, ", abs(eta) == 0.61)), expression(paste("San Francisco, ", abs(eta) == 0.52))),
       col=c(COL[3], COLB[2],COLA[3],COLB[4]), 
       lty=1, cex=labelsize, xpd =TRUE)






dev.off()
