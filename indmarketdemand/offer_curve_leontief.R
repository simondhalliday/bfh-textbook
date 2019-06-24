#Graph Designer: Simon Halliday + Scott Cohn
#Authors: Bowles and Halliday
#Title: Microeconomics: Competition, Conflict and Coordination


require(shape)
pdf(file = "indmarketdemand/offer_curve_leontief.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 6, 4, 4))

mrsA <- function(x, rmax = 10, xmax = 20) {
  rmax - (rmax/xmax)*x
}


uA <- function(x, y, rmax = 2, xmax = 12) {
  y + rmax*x - (1/2)*(rmax/xmax)*x^2
}

indiffA1 <- function(x, uA = 10, rmax = 2.5, xmax = 10) {
  uA - rmax*x + (1/2)*(rmax/xmax)*(x^2)
}

offerCurve <- function(x, w, rmax = 2, xmax = 12) {
  w - rmax*x + (rmax/xmax)*x^2
}

bcA <- function(x, w = 20, px = 2, py = 2) {
  w/(py) - (px/py)*x
}


xlims <- c(0, 12)
ylims <- c(0, 12)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(13, 16.75, 19.25)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")


ticksy <- c(0, 4, 5 , 6, ylims[2])
ylabels <- c(NA, expression(paste(y[1])), expression(paste(y[2])), expression(paste(y[3])), NA)
ticksx <- c(0, 4, 5, 6,  xlims[2])
xlabels <- c(NA, expression(paste(x[1])==4), expression(paste(x[2])==5), expression(paste(x[3])==6), NA)


axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#lines(xx1, bcA(xx1, w = 10, p = 1.5), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 20, px = 1.333), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 20, px = 2), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 20, px = 3), col = COLB[3], lwd = graphlinewidth)


#Label the axes
mtext(expression(paste("Quantity of knives, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-2, 0.5*ylims[2], expression(paste("Quantity of forks, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the indifference curves
text(4.25, 11, expression(u[1]), cex = labelsize)
text(5.25, 11, expression(u[2]), cex = labelsize)
text(6.25, 11, expression(u[3]), cex = labelsize)

#Label the price lines
text(7, 1.5, expression(paste(bc[1])))
text(7, 1, expression(paste(p[x] == 1.33)))
text(10, 1.5, expression(paste(bc[2])))
text(10, 1, expression(paste(p[x] == 2)))
text(11.5, 3.5, expression(paste(bc[3])))
text(11.5, 3, expression(paste(p[x] == 3)))

#Label the offer curve
text(11, 10.25, expression("Price"), cex = labelsize)
text(11, 9.75, expression("Offer"), cex = labelsize)
text(11, 9.25, expression("Curve"), cex = labelsize)

#Segments for points on Offer curve
segments(0, 4, 4, 4, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(4, 0, 4, 4, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 5, 5, 5, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(5, 0, 5, 5, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 6, 6, 6, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(6, 0, 6, 6, lty = 2, col = "gray" , lwd = segmentlinewidth)

# Add Leontief
segments(4, 4, 4, 22, lty = 1, col = COLA[3] , lwd = graphlinewidth)
segments(4, 4, 19, 4, lty = 1, col = COLA[3] , lwd = graphlinewidth)

segments(5, 5, 5, 22, lty = 1, col = COLA[3] , lwd = graphlinewidth)
segments(5, 5, 22, 5, lty = 1, col = COLA[3] , lwd = graphlinewidth)

segments(6, 6, 6, 22, lty = 1, col = COLA[3] , lwd = graphlinewidth)
segments(6, 6, 25, 6, lty = 1, col = COLA[3] , lwd = graphlinewidth)

#Add the offer curve (superimposed on the indifference curves tangent to the price lines)
segments(2, 2, 23, 23, lty = 1, col = COL[3], lwd = graphlinewidth)

#Annotate points on offer curve mrs = p for each of p = 1, 0.5, 0.25
#Where mrs = 2 - (1/6)*x
points(4, 4, pch = 16, col = "black", cex = 1.5)
points(5, 5, pch = 16, col = "black", cex = 1.5)
points(6, 6, pch = 16, col = "black", cex = 1.5)

text(3.85, 3.5, expression(paste(a)), cex = labelsize)
text(4.85, 4.5, expression(paste(b)), cex = labelsize)
text(5.85, 5.5, expression(paste(c)), cex = labelsize)

dev.off()
