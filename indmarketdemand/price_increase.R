require(shape)
pdf(file = "indmarketdemand/price_increase.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(5, 6, 1, 1))

mrsA <- function(x, rmax = 20, xmax = 10) {
  rmax - (rmax/xmax)*x
}

uA <- function(x, y, rmax = 20, xmax = 10) {
  y + rmax*x - (1/2)(rmax/xmax)*x^2
}

Qs <- function(x, tax = 0){
  x + 5 + tax
}

Qs_tax<- function(x, tax = 3){
  x + 5 + tax
}

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

ticksy <- c(0, 10, 12, 20, ylims[2])
ylabels <- c(NA, expression(paste(p[b] == 10)), expression(paste(p[a] == 12)), expression(paste(bar(p) == 20)), NA)
ticksx <- c(0, 2, 4, 5, 10, xlims[2])
xlabels <- c(0, expression(paste(x["m"]) == 2),expression(paste(x[a]) == 4), expression(paste(x[b]) == 5), expression(paste(bar(x)==10)), NA)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)


#Lines for mrs graph
lines(xx1, mrsA(xx1, rmax = 20, xmax = 10), col = COLA[4], lwd = graphlinewidth)

lines(xx1, Qs(xx1), col = COLB[4], lwd = graphlinewidth)
lines(xx1, Qs_tax(xx1), col = COLB[5], lwd = graphlinewidth)

# Market Price
segments(0, 12, 4, 12, lty = 2, col = "gray" , lwd = segmentlinewidth) 
segments(0, 10, 5, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)

# Vert Seg from Q 
segments(5, 0, 5, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(4, 0, 4, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)

# Q*
segments(2, 0, 2, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)

#Label axes
mtext(expression(paste("Quantity, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-1.4, 0.5*ylims[2], expression(paste("Price, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 

# Tax
Arrows(6, 11.6, 6, 13.1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.2)
Arrows(6, 13.1, 6, 11.6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.2)
text(6.6, 12.7, expression(paste("Tax" == 3)), cex = labelsize)


#Label i
points(5, 10, pch = 16, col = "black", cex = 1.5)
text(5, 10.75, expression(b), cex = annotatesize)

#Label g
points(4, 12, pch = 16, col = "black", cex = 1.5)
text(4, 12.75, expression(a), cex = annotatesize)

#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
text(8.5, 19, expression(paste("Supply with tax, ", p(x) == 8 + x)), cex = labelsize)
text(7.75, 10.5, expression(paste("Supply, ", p(x) == 5 + x)), cex = labelsize)

#Label mrs function
text(8.05, 8, expression(paste("Demand, ", p(x) == 20 - 2*x)), cex = labelsize)
#Arrows(10, 7.5, 10, 5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

dev.off()