require(shape)
library(extrafont)
loadfonts()
#Help from http://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html
pdf(file = "indmarketdemand/demand_jane_elasticity.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 6, 4, 4))

mrsA <- function(x, rmax = 20, xmax = 10) {
  rmax - (rmax/xmax)*x
}

# uA <- function(x, y, rmax = 20, xmax = 10) {
#   y + rmax*x - (1/2)(rmax/xmax)*x^2
# }

xlims <- c(0, 11)
ylims <- c(0, 22)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(46.08, 55, 64)
b <- c(46.08, 55, 64)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs = "i", 
     yaxs = "i")

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
# ticksx <- seq(from = 0, to = xlims[2], by = 2)
# xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(0, 10, 20, ylims[2])
ylabels <- c(NA, expression(paste(p == 10)), expression(paste(bar(r) == 20)), NA)
ticksx <- c(0, 5, 10, xlims[2])
xlabels <- c(NA, expression(paste(x,"*") == 5), expression(paste(bar(x)==10)), NA)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Polygon for CS
xpoly <- c(0, 5, 0)
ypoly <- c(10, 10, 20)
polygon(x = xpoly, y = ypoly, col = COL[4], density=NULL, border = NA)

#Polygon for CE
xpoly3 <- c(0, 5, 5, 0, 0)
ypoly3 <- c(0, 0, 10, 10, 0)
polygon(x = xpoly3, y = ypoly3, col = COLB[1], density=NULL, border = NA)


#Lines for mrs graph
lines(xx1, mrsA(xx1, rmax = 20, xmax = 10), col = COLA[4], lwd = graphlinewidth)

segments(0, 10, xlims[2], 10, lty = 1, col = COL[2] , lwd = graphlinewidth)
segments(5, 0, 5, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)

#Label axes
mtext(expression(paste("Quantity of fish in kilograms, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-1.6, 0.5*ylims[2], expression(paste("Price per kilogram, ", p[x])), xpd = TRUE, cex = axislabelsize, srt = 90) 

points(5, 10, pch = 16, col = "black", cex = 1.5)
text(5.25, 10.5, expression(i))

#text(x = 2.5, y = 15, '{', srt = 235, cex = 8, family = 'Helvetica Neue UltraLight')


#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
text(9, 10.5, expression("Market price, p = 10"), cex = labelsize)


#Label mrs function
text(8.6, 5.5, expression(paste(p(x) == 20 - 2*x)))
#Arrows(10, 7.5, 10, 5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)


dev.off()
