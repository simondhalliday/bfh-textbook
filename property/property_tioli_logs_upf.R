require(shape)
require(plotrix)
library(tidyverse)
pdf(file = "property/property_tioli_logs_upf.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 3
arrowwidth <- 0.5

#Colors
COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

upf <- function(ua) {
  log(5*sqrt(6) - exp(ua))
}



uAlog <- function(xA, yA, alpha = 1/2){
  alpha*log(xA) + (1-alpha)*log(yA)
}

paretoEC <- function(x, ybar = 15, xbar = 10) {
  (ybar/xbar)*x
}



x <- seq(0.01, 10, by = 0.01)
y <- paretoEC(x)
uA <- uAlog(x,y, alpha = 1/2)
uB <- rev(uA)
data <- data.frame(x,y,uA, uB)

# ggplot(data, aes(uA, rev(uA))) + 
#   geom_point() + 
#   xlim(0, 2.5) + 
#   ylim(0, 2.5)

#uAlog(9,1, alpha = 1/2)
#[1] 1.098612

#uAlog(1,4, alpha = 1/2)
#[1] 1.319529

#Notes
#ua = (xa^0.5)*(ya^0.5)
#ub = ((10-xa)^05)*((15-ya)^0.5)

#ua = (10^0.5)*(15^0.5) = 12.24745
#ua_e = u(8,2) = (8^0.5)*(2^0.5) = 4



#Point g
# => x^A = 4/((3/2)^0.5) = 3.27 => x^B = 6.73
# => y^A = 3/2(x^A) = 4.9 => y^B = 10.1
#ua_g(4.9^0.5)*(3.27^0.5) = 4.002874
#ub_g(6.73^0.5)*(10.1^0.5) = 8.244574
#W = (4.002874^0.5)*(8.244574^0.5) = 5.744736

#COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")
par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 3)
ylims <- c(0, 3)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("A's Utility, ", u^A)),
     ylab = expression(paste("B's Utility, ", u^B)),
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(xlims[1], xlims[2], length.out = npts2)

#polygons
#I need something like xx1 with npts for 

#2.141474
#line 245 to 756 = 512 inclusive
#xrev <- data %>% 
#  filter(uA >= 1.0988206 & uB >= 1.317874)

polyxx <- seq(uAlog(9,1), 2.14, length.out = 200)
xpoly1 <- c(uAlog(9,1), 2.14, polyxx, uAlog(9,1), uAlog(9,1))
ypoly1 <- c(uAlog(1,14), upf(2.14), upf(polyxx), upf(uAlog(9,1)), uAlog(1,14))
polygon(x = xpoly1, y = ypoly1, col=COL[4], density=NULL, border = NA)

#xpoly2 = c(0, 0, 12.24745, xlims[2], xlims[2], 0)
#ypoly2 = c(ylims[2], 12.24745, 0, 0, ylims[2], ylims[2])
#polygon(x = xpoly2, y = ypoly2, col=COL[4], density=NULL, border = NA)



#Draw the lines for the graphs
lines(xx1, upf(xx1), col = COL[1], lwd = graphlinewidth)

#Customize ticks and labels for the plot
ticksy <- c(0,1, 2, uAlog(1, 14), 3)
ylabels <-  c(NA,1, 2, expression(paste(u[z]^B)), 3)
ticksx <-  c(0, 1, 2, uAlog(9, 1), 3)
xlabels <-  c(NA, 1, 2, expression(paste(u[z]^A)), 3)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Annotation of the  graphs
#UPF
text(1.9, 0.8, expression(paste("Utility Possibilities Frontier")))
text(1.9, 0.68, expression(paste(u^B == ln*(bar(W) - e^u^A))))

#SWF
#text(11.5, 4.5, expression(paste("Social Planner's")))
#text(11.5, 4, expression(paste("Iso-welfare Curves")))

#Annotate Point f
#u^A = (5.84^0.5)*(8.77^0.5) = 7.16
#u^B = (4.16^0.5)*(6.23^0.5) = 5.09
#W = (7.16^0.5)*(5.09^0.5) = 6.03692


#text(5.3, 7.3, expression(f))


#Annotate Point i
#(5^0.5)*(7.5^0.5) = 6.123724
#W = (6.123724^0.5)(6.123724^0.5) = 6.123724
#points(6.123724, 6.123724, pch = 16, col = "black", cex = 1.5)
#text(6.3, 6.3, expression(i))

#Annotate point g
#ua_g(2.44949^0.5)*(((3/2)*2.44949)^0.5) = 3
#ub_g((10 - 2.44949)^0.5)*(15 - (3/2)*2.44949)^0.5) = 9.247448
#W = (3^0.5)*(9.247448^0.5) = 5.2671
#points(8.24, 4, pch = 16, col = "black", cex = 1.5)
#text(8.4, 4.2, expression(g))

#Annotate point e.
#ua_e = (8^0.5)*(2^0.5) = 4
#ub_e = (2^0.5)*(13.1^0.5) = 5.118594
#W = (4^0.5)*(5.118594^0.5) = 4.524862
segments(uAlog(9,1), 0, uAlog(9,1), ylims[2], lty = 2, col = "darkgray", lwd = 1.5)
segments(0, uAlog(1,14), xlims[2], uAlog(1,14), lty = 2, col = "darkgray", lwd = 1.5)

#Annotate z
points(uAlog(9,1), uAlog(1,14), pch = 16, col = "black", cex = 1.5)
text(uAlog(9,1)-0.05, uAlog(1,14)-0.05, expression(z))

points(2.14, upf(2.14), pch = 16, col = "black", cex = 1.5)

points(uAlog(9,1), upf(uAlog(9,1)), pch = 16, col = "black", cex = 1.5)

points(uAlog(5, 7.5), uAlog(5, 7.5), pch = 16, col = "black", cex = 1.5)


#Annotate f
points(5.12, 7.12, pch = 16, col = "black", cex = 1.5)
text(5.3, 7.3, expression(f))
#Annotate g
points(8.24, 4, pch = 16, col = "black", cex = 1.5)
text(8.4, 4.2, expression(g))

#Annotate m
points(5.703502, 4.389995, pch = 16, col = "black", cex = 1.5)
text(5.9, 4.6, expression(m))

#Label economic rents
Arrows(1.5, 2.7, 1.5, 1.7, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(1.5, 2.8, expression(paste("Economic Surplus")))

#Point h
#points(4, 10, pch = 16, col = "black", cex = 1.5)
#u^A_h(6, 5) = (6^0.5)*(5^0.5) = 5.477226
#u^A_h(4, 10) = (4^0.5)*(10^0.5) = 6.324555
#points(6.12, 4, pch = 16, col = "black", cex = 1.5)
#text(5.92, 3.8, expression(h))

#Annotate Point i
#(5^0.5)*(7.5^0.5) = 6.123724
#W = (6.123724^0.5)(6.123724^0.5) = 6.123724
points(6.123724, 6.123724, pch = 16, col = "black", cex = 1.5)
text(6.3, 6.3, expression(i))

#Label Participation Constraints
#Aisha's
text(11, 4.4, expression(paste("A's Participation Constraint ", u[z]^A)))

#Betty's
text(7.1, 12.5, expression(paste("B's Participation Constraint ", u[z]^B)))

#Arrows showing Social Planner's choices
#Arrows(5.2, 4.2, 5.9, 4.2, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = arrowwidth)
#Arrows(6.12, 4.2, 6.12, 5.7, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = arrowwidth)

dev.off()

