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

bcA <- function(x, w = 20, p = 2) {
  w - p*x
}


xlims <- c(0, 25)
ylims <- c(0, 25)

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


ticksy <- c(0, 10, 13.3333 , 16, ylims[2])
ylabels <- c(NA, expression(paste(y[1])), expression(paste(y[2])), expression(paste(y[3])), NA)
ticksx <- c(0, 10, 13.3333, 16,  xlims[2])
xlabels <- c(NA, expression(paste(x[1])==10), expression(paste(x[2])==13.3), expression(paste(x[3])==16), NA)


axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#lines(xx1, bcA(xx1, w = 10, p = 1.5), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 20, p = 1), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 20, p = 0.5), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 20, p = 0.25), col = COLB[3], lwd = graphlinewidth)


#Label the axes
mtext(expression(paste("Quantity of knives, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-2, 0.5*ylims[2], expression(paste("Quantity of forks, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the indifference curves
text(10.5, 21, expression(u[1]), cex = labelsize)
text(13.8, 21, expression(u[2]), cex = labelsize)
text(16.5, 21, expression(u[3]), cex = labelsize)

#Label the price lines
text(17, 2, expression(paste(bc[1])))
text(17, 1, expression(paste(p[x] == 1)))
text(23.5, 7, expression(paste(bc[2])))
text(23.5, 6, expression(paste(p[x] == 0.5)))
text(23.5, 13.5, expression(paste(bc[3])))
text(23.5, 12.5, expression(paste(p[x] == 0.25)))

#Label the offer curve
text(22, 20, expression("Price"), cex = labelsize)
text(22, 19, expression("Offer"), cex = labelsize)
text(22, 18, expression("Curve"), cex = labelsize)

#Segments for points on Offer curve
segments(0, 10, 10, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(10, 0, 10, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 13.3333, 13.3333, 13.3333, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(13.3333, 0, 13.3333, 13.3333, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 16, 16, 16, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(16, 0, 16, 16, lty = 2, col = "gray" , lwd = segmentlinewidth)

# Add Leontief
segments(10, 10, 10, 22, lty = 1, col = COLA[3] , lwd = graphlinewidth)
segments(10, 10, 19, 10, lty = 1, col = COLA[3] , lwd = graphlinewidth)

segments(13.3333, 13.3333, 13.3333, 22, lty = 1, col = COLA[3] , lwd = graphlinewidth)
segments(13.3333, 13.3333, 22, 13.3333, lty = 1, col = COLA[3] , lwd = graphlinewidth)

segments(16, 16, 16, 22, lty = 1, col = COLA[3] , lwd = graphlinewidth)
segments(16, 16, 25, 16, lty = 1, col = COLA[3] , lwd = graphlinewidth)

#Add the offer curve (superimposed on the indifference curves tangent to the price lines)
segments(4, 4, 23, 23, lty = 1, col = COL[3], lwd = graphlinewidth)

#Annotate points on offer curve mrs = p for each of p = 1, 0.5, 0.25
#Where mrs = 2 - (1/6)*x
points(10, 10, pch = 16, col = "black", cex = 1.5)
points(13.3333, 13.3333, pch = 16, col = "black", cex = 1.5)
points(16, 16, pch = 16, col = "black", cex = 1.5)

text(9.6, 9.05, expression(paste(e)), cex = labelsize)
text(12.9333, 12.3833, expression(paste(f)), cex = labelsize)
text(15.6, 15.05, expression(paste(g)), cex = labelsize)

dev.off()
