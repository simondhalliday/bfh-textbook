#Graph Designer: Scott Cohn
#Authors: Bowles and Halliday
#Title: Microeconomics: Competition, Conflict and Coordination

require(shape)
pdf(file = "indmarketdemand/consumersurplus_jane_market.pdf", width = 9, height = 7)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5


COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 7, 1, 1))

mrsA <- function(x, rmax = 20, xmax = 10) {
  rmax - (rmax/xmax)*x
}

uA <- function(x, y, rmax = 20, xmax = 10) {
  y + rmax*x - (1/2)(rmax/xmax)*x^2
}

xlims <- c(0, 115)
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
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
# ticksx <- seq(from = 0, to = xlims[2], by = 2)
# xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(0, 10, 20, ylims[2])
ylabels <- c(NA, expression(paste(p == 10)), expression(paste(bar(p) == 20)), NA)
ticksx <- c(0, 5, 10, xlims[2])
xlabels <- c(NA, expression(paste(x[i])), expression(paste(bar(x))), NA)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Lines for mrs graph
lines(xx1, mrsA(xx1, rmax = 20, xmax = 10), col = COLA[4], lwd = graphlinewidth)

segments(0, 10, xlims[2], 10, lty = 1, col = COL[2] , lwd = graphlinewidth)
segments(5, 0, 5, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)

#Polygon for CS
xpoly <- c(0, 5, 0)
ypoly <- c(10, 10, 20)
polygon(x = xpoly, y = ypoly, col = COLA[1], density=NULL, border = NA)

#Polygon for CE
xpoly3 <- c(0, 5, 5, 0, 0)
ypoly3 <- c(0, 0, 10, 10, 0)
polygon(x = xpoly3, y = ypoly3, col = COLB[1], density=NULL, border = NA)

#Label axes
mtext(expression(paste("Kilograms of fish, ", x)), side=1, line = 3, cex = axislabelsize)
text(-16.2, 0.5*ylims[2], expression(paste("Price per kilogram, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 

points(5, 10, pch = 16, col = "black", cex = 1.5)
text(7, 10 + 0.5, expression(i), cex = labelsize)

#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
text(90, 10.5, expression("Market price, p = 10"), cex = labelsize, xpd = TRUE)
#text(7.3, 2.5, expression("Curve"))

#Label mrs function
text(30.5, 3.0, expression(paste("Inverse demand")), cex = labelsize)
text(30.5, 1.5, expression(paste(p(x) == 20 - 2*x)), cex = labelsize)
#Arrows(10, 7.5, 10, 5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label highest willingness to pay
text(20.7, 12.8, expression("Consumer surplus"), cex = labelsize)
#text(3.5, 16, expression(paste(CS==frac(1, 2)*bgroup("(",20 - 10,")")*5, phantom()== 25)))
Arrows(20.5, 12, 3.5, 12, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(30, 6, expression("Consumer expenditure"), cex = labelsize)
Arrows(20.5, 5.2, 3.5, 5.2, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)


dev.off()