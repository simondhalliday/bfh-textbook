require(shape)
pdf(file = "indmarketdemand/offer_curve_qql.pdf", width = 9, height = 7)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")
grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")


#Edited the margins to cater for the larger LHS labels
par(mar =  c(5.5, 5, 1, 1))

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
a <- c(13, 16.75, 19.2)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = ylims[1], to = ylims[2], by = 1)
# ylabels <- seq(from = ylims[1], to = ylims[2], by = 1)
# ticksx <- seq(from = xlims[1], to = xlims[2], by = 1)
# xlabels <- seq(from = xlims[1], to = xlims[2], by = 1)
ticksy <- c(0, 4, 5.5, 7.375 , 10, ylims[2])
ylabels <- c(NA, expression(paste(y[1])), expression(paste(y[2])), expression(paste(y[3])), expression(paste(m)), NA)
ticksx <- c(0, 6, 9, 10.5,  xlims[2])
xlabels <- c(NA, expression(paste(x[1])==6), expression(paste(x[2])==9), expression(paste(x[3])==10.5), NA)




axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#lines(xx1, bcA(xx1, w = 10, p = 1.5), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 10, p = 1), col = CBCols[2], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 10, p = 0.5), col = CBCols[2], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 10, p = 0.25), col = CBCols[2], lwd = graphlinewidth)


#Label the axes
text(0.5*xlims[2], -1.5, expression(paste("Kilograms of fish, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-1, 0.5*ylims[2], expression(paste("Money left over, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the indifference curves
text(11.8, 1.3, expression(u[1]), cex = labelsize)
text(11.8, 5.1, expression(u[2]), cex = labelsize)
text(11.8, 7.5, expression(u[3]), cex = labelsize)

#Label the price lines
text(8, 1.7, expression(paste(bc[1])), cex = annotatesize)
text(8, 1.2, expression(paste(p[x] == 10)), cex = annotatesize)
text(11.3, 4.1, expression(paste(bc[2])), cex = annotatesize)
text(11.3, 3.6, expression(paste(p[x] == 5)), cex = annotatesize)
text(11.3, 6.9, expression(paste(bc[3])), cex = annotatesize)
text(11.3, 6.4, expression(paste(p[x] == 2.5)), cex = annotatesize)

#Label the offer curve
text(11.1, 10.2, expression("Price-offer"), cex = labelsize)
text(11.1, 9.7, expression("curve"), cex = labelsize)

#Add the contour plot for the indifference curves
contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = CBCols[1],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE)

#Add the offer curve (superimposed on the indifference curves tangent to the price lines)
xx2 <- seq(2, xlims[2], length.out = npts)
lines(xx2, offerCurve(xx2, w = 10, rmax = 2, xmax = 12), col = CBCols[3], lwd = graphlinewidth)

#Segments for points on Offer curve
segments(0, 4, 6, 4, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(6, 0, 6, 4, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, 5.5, 9, 5.5, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(9, 0, 9, 5.5, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, 7.375, 10.5, 7.375, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(10.5, 0, 10.5, 7.375, lty = 2, col = grays[20] , lwd = segmentlinewidth)

#Annotate points on offer curve mrs = p for each of p = 1, 0.5, 0.25
#Where mrs = 2 - (1/6)*x
points(6, 4, pch = 16, col = "black", cex = 1.5)
points(9, 5.5, pch = 16, col = "black", cex = 1.5)
points(10.5, 7.375, pch = 16, col = "black", cex = 1.5)

text(6, 4.4, expression(paste(a)), cex = labelsize)
text(9, 5.9, expression(paste(b)), cex = labelsize)
text(10.5, 7.775, expression(paste(c)), cex = labelsize)

dev.off()
