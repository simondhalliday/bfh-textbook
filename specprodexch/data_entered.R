#Graph Designer: Simon Halliday + Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "specprodexch/data_entered.pdf", width = 8, height = 8)

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
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")
grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")


ffA <- function(x, yintA = 20, sA = 20/11) {
        yintA - sA*x
}

ffB <- function(x, yintB = 8, sB = 8/10) {
        yintB - sB*x
}

exchange <- function(x, slope = 1/1.25) {
        slope*x
}

priceA <- function(x, pintA = 20, psA = 1.45) {
        pintA - psA*x
}

priceB <- function(x, pintB = 14.5, psB = 1.45) {
        pintB - psB*x
}

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 6, 1, 1))

xlims <- c(0, 15)
ylims <- c(0, 21)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(3.25, 5.25, 7.25)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i"
)


ticksy <- c(ylims[1], 5.16, 7.11, 8, 14.5, 20, ylims[2])
ylabels <- c(NA, 5.16, 7.11, 8, 14.5, 20, NA)
ticksx <- c(xlims[1], 6.45, 8.88, 10, 11, 13.78, xlims[2])
xlabels <-  c(NA, 6.45, 8.88, 10, 11, 13.8, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Draw the graphs
lines(xx1, ffA(xx1), col = CBCols[1], lwd = graphlinewidth)
lines(xx1, ffB(xx1), col =CBCols[1], lwd = graphlinewidth)
lines(xx1, exchange(xx1), lty = 2, col = CBCols[3], lwd = graphlinewidth)
lines(xx1, priceA(xx1), col = CBCols[2], lwd = graphlinewidth)
lines(xx1, priceB(xx1), col = CBCols[2], lwd = graphlinewidth)


#Segments for new points
segments(0, 5.16, 6.45, 5.16, lty = 2, col = grays[20],  lwd = segmentlinewidth)
segments(6.45, 0, 6.45, 5.16, lty = 2, col = grays[20],  lwd = segmentlinewidth)
segments(0, 7.11, 8.88, 7.11, lty = 2, col = grays[20],  lwd = segmentlinewidth)
segments(8.88, 0, 8.88, 7.11, lty = 2, col = grays[20],  lwd = segmentlinewidth)


#Label Points
xpts <- c(5, 6.444444444, 7.638888889, 8.888888889)
ypts <- xpts/1.25
yadj1 <- 0.5
yadj2 <- -0.5

points(xpts, ypts, pch = 16, col = "black", cex = 1.5)
ptlabels <- c("g", "h", "i", "j")
text(xpts, ypts + 1.5* yadj1, ptlabels, cex = labelsize)

#Axis labels
mtext(expression(paste("Data entered ('000's), ", x)), side = 1, line = 3.5, cex = axislabelsize)
text(-2, 0.5*ylims[2], expression(paste("Graphs made, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Label the curves
text(13.8, 1.4, expression(paste("price")), cex = labelsize)
text(14, 0.7, expression(paste("line")), cex = labelsize)
text(10.6, 1.5, expression(ff[A]), cex = labelsize)

text(0.9, 12, expression(paste("price")), cex = labelsize)
text(1, 11.3, expression(paste("line")), cex = labelsize)
text(1, 6.7, expression(ff[B]), cex = labelsize)


Arrows(xpts[1]+1.8, ypts[1] - 1.8, 10 - 1.6, 0 + 0.9, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
Arrows(8.5, 0 + 2.5, xpts[2] + 0.5, ypts[2] - 0.25, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(10 + 0.4, 0.4, expression(s[B]), cex = labelsize)
points(10, 0,pch = 16, col = "black", cex = 1.5, xpd = TRUE)

Arrows(7.3, 6, 6.1, 8.2, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#Arrows(1.5, 16.8, 0 + 0.3, 20 - 1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
Arrows(0 + 0.3, 20, 1.5, 16.8 + 1.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(0 + 0.4, 20 + 0.4, expression(s[A]), cex = labelsize)
points(0, 20,pch = 16, col = "black", cex = 1.5, xpd = TRUE)


text(12.4, 13.5, expression(paste("Each graph needs")), cex = labelsize)
text(12.4, 12.7, expression(paste("1,250 keystrokes")), cex = labelsize)
text(12.4, 11.5, expression(paste(y == frac(x, 1.25) )), cex = labelsize)

dev.off()
