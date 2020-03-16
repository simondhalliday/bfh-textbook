#Graph Designer: Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
require(pBrackets)
pdf(file = "constrained_optimization/offer_curve_qql_demand_stacked.pdf", width = 9, height = 12)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 7, 1, 1), mfrow = c(2,1))

# offer_curve_qql -----------------------------------------------------


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

bcA <- function(x, w = 20, p = 2) {
  w - p*x
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
     xaxs = "i", 
     yaxs = "i")


ticksy <- c(0, 4, 5.5, 7.375 , ylims[2])
ylabels <- c(NA, expression(paste(y[1])), expression(paste(y[2])), expression(paste(y[3])), NA)
ticksx <- c(0, 6, 9, 10.5,  xlims[2])
xlabels <- c(NA, expression(paste(x[1])==6), expression(paste(x[2])==9), expression(paste(x[3])==10.5), NA)


axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#lines(xx1, bcA(xx1, w = 10, p = 1.5), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 10, p = 1), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 10, p = 0.5), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 10, p = 0.25), col = COLB[3], lwd = graphlinewidth)


#Label the axes
mtext(expression(paste("Quantity of fish in kilograms, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-2, 0.5*ylims[2], expression(paste("Money left over, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the indifference curves
text(11.8, 1.3, expression(u[1]), cex = labelsize)
text(11.8, 5.1, expression(u[2]), cex = labelsize)
text(11.8, 7.6, expression(u[3]), cex = labelsize)

#Label the price lines
text(8, 1.7, expression(paste(bc[1])))
text(8, 1.3, expression(paste(p[x] == 1)))
text(11.3, 4.1, expression(paste(bc[2])))
text(11.3, 3.7, expression(paste(p[x] == 0.5)))
text(11.3, 6.9, expression(paste(bc[3])))
text(11.3, 6.5, expression(paste(p[x] == 0.25)))

#Label the offer curve
text(10.8, 9.7, expression("Offer Curve"), cex = labelsize)

#Add the contour plot for the indifference curves
contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLA[3],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE)

#Add the offer curve (superimposed on the indifference curves tangent to the price lines)
xx2 <- seq(2, xlims[2], length.out = npts)
lines(xx2, offerCurve(xx2, w = 10, rmax = 2, xmax = 12), col = COL[3], lwd = graphlinewidth)

#Segments for points on Offer curve
segments(0, 4, 6, 4, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(6, 0, 6, 4, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 5.5, 9, 5.5, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(9, 0, 9, 5.5, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 7.375, 10.5, 7.375, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(10.5, 0, 10.5, 7.375, lty = 2, col = "gray" , lwd = segmentlinewidth)

#Annotate points on offer curve mrs = p for each of p = 1, 0.5, 0.25
#Where mrs = 2 - (1/6)*x
points(6, 4, pch = 16, col = "black", cex = 1.5)
points(9, 5.5, pch = 16, col = "black", cex = 1.5)
points(10.5, 7.375, pch = 16, col = "black", cex = 1.5)

text(6, 4.4, expression(paste(a)), cex = labelsize)
text(9, 5.9, expression(paste(b)), cex = labelsize)
text(10.5, 7.775, expression(paste(c)), cex = labelsize)



# offer_demand --------------------------------------------------------

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

bcA <- function(x, w = 20, p = 2) {
  w - p*x
}


xlims <- c(0, 12)
ylims <- c(0, 2.5)

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
     xaxs = "i", 
     yaxs = "i")

ticksy <- c(0, 0.25, 0.5, 1, 2, ylims[2])
ylabels <- c(NA, expression(paste(p[x] == 0.25)), expression(paste(p[x]==0.5)), expression(paste(p[x]==1)), expression(paste(p[x]==2)), NA)
ticksx <- c(0, 6, 9, 10.5,  xlims[2])
xlabels <- c(NA, expression(paste(x[1]) == 6), expression(paste(x[2])==9), expression(paste(x[3])==10.5), NA)




axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#lines(xx1, bcA(xx1, w = 10, p = 1.5), col = COLB[3], lwd = graphlinewidth)
lines(xx1, mrsA(xx1, rmax = 2, xmax = 12), col = COLB[3], lwd = graphlinewidth)
#lines(xx1, bcA(xx1, w = 10, p = 0.5), col = COLB[3], lwd = graphlinewidth)
#lines(xx1, bcA(xx1, w = 10, p = 0.25), col = COLB[3], lwd = graphlinewidth)


#Label the axes
mtext(expression(paste("Quantity of fish in kilograms, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-2, 0.5*ylims[2], expression(paste("Price per kilogram of fish, ", p[x])), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Segments for points on Offer curve
segments(0, 1, 6, 1, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(6, 0, 6, ylims[2], lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 0.5, 9, 0.5, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(9, 0, 9, ylims[2], lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 0.25, 10.5, 0.25, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(10.5, 0, 10.5, ylims[2], lty = 2, col = "gray" , lwd = segmentlinewidth)

#Annotate points on offer curve mrs = p for each of p = 1, 0.5, 0.25
#Where mrs = 2 - (1/6)*x
points(6, 1, pch = 16, col = "black", cex = 1.5)
points(9, 0.5, pch = 16, col = "black", cex = 1.5)
points(10.5, 0.25, pch = 16, col = "black", cex = 1.5)

text(6.2, 1.1, expression(paste(a*minute)), cex = labelsize)
text(9.2, 0.6, expression(paste(b*minute)), cex = labelsize)
text(10.7, 0.35, expression(paste(c*minute)), cex = labelsize)

#Label the offer curve
text(8, 1.8, expression("Demand Function"), cex = labelsize)
text(8, 1.62, expression(p == 2 - frac(1,6)*x), cex = labelsize)
Arrows(8, 1.55, 8, 0.75, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


dev.off()