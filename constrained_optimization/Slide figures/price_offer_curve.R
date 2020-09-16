#Graph Designer: Scott Cohn & Simon Halliday
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
require(pBrackets)
pdf(file = "constrained_optimization/Slide figures/offer_curve.pdf", width = 9, height = 8)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 2
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)


#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 7, 0.5, 0.5))

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
#mtext(expression(paste("Kilograms of fish, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-1.5, 0.5*ylims[2], expression(paste("Money left over, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the indifference curves
text(11.8, 1.3, expression(u[1]), cex = labelsize)
text(11.8, 5.1, expression(u[2]), cex = labelsize)
text(11.8, 7.6, expression(u[3]), cex = labelsize)

#Label the price lines
#text(8, 1.7, expression(paste(bc[1])))
text(8, 1.25, expression(paste(p[3] == 1)), cex = annotatesize)
#text(11.3, 4.1, expression(paste(bc[2])))
text(11.3, 3.7, expression(paste(p[2] == 0.5)), cex = annotatesize)
#text(11.3, 6.9, expression(paste(bc[3])))
text(11.1, 6.45, expression(paste(p[1] == 0.25)), cex = annotatesize)

#Label the offer curve
text(10.6, 9.7, expression("Offer Curve"), cex = labelsize)

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
segments(0, 4, 6, 4, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(6, 0, 6, 4, lty = 2, col = grays[20] , lwd = segmentlinewidth, xpd = TRUE)
segments(0, 5.5, 9, 5.5, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(9, 0, 9, 5.5, lty = 2, col = grays[20] , lwd = segmentlinewidth, xpd = TRUE)
segments(0, 7.375, 10.5, 7.375, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(10.5, 0, 10.5, 7.375, lty = 2, col = grays[20] , lwd = segmentlinewidth, xpd = TRUE)

#Annotate points on offer curve mrs = p for each of p = 1, 0.5, 0.25
#Where mrs = 2 - (1/6)*x
points(6, 4, pch = 16, col = "black", cex = 1.5)
points(9, 5.5, pch = 16, col = "black", cex = 1.5)
points(10.5, 7.375, pch = 16, col = "black", cex = 1.5)

text(6, 4.4, expression(paste(a)), cex = labelsize)
text(9, 5.9, expression(paste(b)), cex = labelsize)
text(10.5, 7.775, expression(paste(c)), cex = labelsize)



text(0.5*xlims[2], -1,  expression(paste("Kilograms of fish, ", x)), xpd = TRUE, cex = axislabelsize) 


dev.off()