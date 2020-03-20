require(shape)
pdf(file = "indmarketdemand/mrs_price_cs.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 5, 1, 1))

mrsA <- function(x, rmax = 10, xmax = 20) {
  rmax - (rmax/xmax)*x
}

# uA <- function(x, y, rmax, rmax = 10, xmas = 20) {
#   y + rmax*x - (1/2)(rmax/xmax)*x^2
# }

xlims <- c(0, 22)
ylims <- c(0, 12)

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
ticksy <- c(0, 10, ylims[2])
ylabels <- c(NA, expression(paste(bar(r))), NA)
ticksx <- c(0, 10, 20, xlims[2])
xlabels <- c(NA, expression(paste(x,"*")), expression(paste(bar(x))), NA)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

xpoly <- c(0, 10, 0)
ypoly <- c(5, 5, 10)
polygon(x = xpoly, y = ypoly, col = COL[4], density=NULL, border = NA)

lines(xx1, mrsA(xx1, rmax = 10, xmax = 20), col = COLA[4], lwd = graphlinewidth)
#lines(xx1, brfB(xx1, alpha = 16, beta = 1/24), col = COLB[4], lwd = graphlinewidth)

segments(0, 5, xlims[2], 5, lty = 1, col = COL[2] , lwd = graphlinewidth)
segments(10, 0, 10, 5, lty = 2, col = "gray" , lwd = segmentlinewidth)

mtext(expression(paste("Quantity of the good, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-2, 6, expression(paste("Marginal rate of substitution, ", mrs(x,y))), xpd = TRUE, cex = axislabelsize, srt = 90) 

points(10, 5, pch = 16, col = "black", cex = 1.5)
text(10.5, 5.25, expression(i))

#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
text(20, 5.25, expression("Market price, p"), cex = labelsize)

#Label mrs function
text(18, 2.5, expression(paste(mrs(x,y) == bar(r) - bgroup("(",frac(bar(r), bar(x)),")")*x)), cex = labelsize)

#Label highest willingness to pay
text(5, 10, expression("Consumer Surplus"), cex = labelsize)
text(5, 9, expression(paste(CS==frac(1, 2)*bgroup("(",bar(r) - p,")")*x)), cex = labelsize)
Arrows(5, 8.5, 5, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

dev.off()
