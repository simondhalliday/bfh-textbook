require(shape)
pdf(file = "indmarketdemand/budget_inferior_incomechange.pdf", width = 7, height = 7)

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
par(mar =  c(5, 5, 1, 1))

uA <- function(x, y, alpha = 0.8, beta = 0.4, a = 0.2, b = 5){
  log(a*(x)^alpha + b*(y)^beta)
}

uA2 <- function(x, y, a = 0.5){
  a*log(x) + y^2/2
}


bcA <- function(x, m = 2.25, px = 0.25, py = 1) {
  m/py - (px/py)*x
}

mOffer <- function(x, int, slope) {
  int - slope*x
}



xlims <- c(0, 3)
ylims <- c(0, 4)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
ulevels <- c(1.3, 2, 3)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs = "i", 
     yaxs = "i")

ticksy <- c(0, 1.52, 2, 2.52, ylims[2])
ylabels <- c(NA, expression(paste(y[1])), expression(paste(y[2])), expression(paste(y[3])), NA)
ticksx <- c(0, 0.7, 1, 1.3, xlims[2])
xlabels <- c(NA, expression(paste(x[1])), expression(paste(x[2])), expression(paste(x[3])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, bcA(xx1, m = 1.85), col = CBCols[2], lwd = graphlinewidth)
lines(xx1, bcA(xx1, m = 2.25), col = CBCols[2], lwd = graphlinewidth)
lines(xx1, bcA(xx1, m = 2.69), col = CBCols[2], lwd = graphlinewidth)
lines(xx1, mOffer(xx1, int = 3.71, slope = 1.7), col = CBCols[3], lwd = graphlinewidth)

#Label Axes
text(0.5*xlims[2], -0.4, expression(paste("Quantity of inferior good, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-0.4, 0.5*ylims[2], expression(paste("Quantity of normal good, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the budget curve functions for the HG, Aisha
text(2.8, 1.05, expression(bc[m1]), cex = labelsize)
text(2.8, 1.45, expression(bc[m2]), cex = labelsize)
text(2.8, 1.9, expression(bc[m3]), cex = labelsize)

text(2.4, 0.38, expression(paste("Income-offer")), cex = labelsize)
text(2.4, 0.23, expression(paste("curve")), cex = labelsize)


segments(0.7, 0, 0.7, 2.52, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, 2.52, 0.7, 2.52, lty = 2, col = grays[20], lwd = segmentlinewidth)
points(0.7, 2.52, pch = 16, col = "black", cex = 1.5)
text(0.7 + 0.1, 2.52 + 0.1, expression(h), cex = annotatesize)


segments(1, 0, 1, 2, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, 2, 1, 2, lty = 2, col = grays[20], lwd = segmentlinewidth)
points(1, 2, pch = 16, col = "black", cex = 1.5)
text(1 + 0.1, 2 + 0.1, expression(g), cex = annotatesize)

segments(1.3, 0, 1.3, 1.52, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, 1.52, 1.3, 1.52, lty = 2, col = grays[20], lwd = segmentlinewidth)
points(1.3, 1.52, pch = 16, col = "black", cex = 1.5)
text(1.3 + 0.1, 1.52 + 0.1, expression(f), cex = annotatesize)


dev.off()
