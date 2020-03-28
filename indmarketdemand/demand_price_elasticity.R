require(shape)
pdf(file = "indmarketdemand/demand_price_elasticity.pdf", width = 9, height = 7)

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

par(mar =  c(6, 8, 1, 1))

mrsA <- function(x, rmax = 10, xmax = 20) {
  rmax - (rmax/xmax)*x
}


xlims <- c(0, 110)
ylims <- c(0, 22)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs = "i", 
     yaxs = "i")

ticksy <- c(0, 6, 10, 14, 20, ylims[2])
ylabels <- c(NA, expression(paste(p[g]) == 6), expression(paste(p[e] == 10)), expression(paste(p[f]) == 14), expression(paste(bar(p) == 20)), NA)
ticksx <- c(0, 30, 50, 70, 100, xlims[2])
xlabels <- c(NA, expression(paste(X[f] == 30)), expression(paste(X[e] == 50)), expression(paste(X[g] == 70)), expression(paste(bar(X) == 100)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)


lines(xx1, mrsA(xx1, rmax = 20, xmax = 100), col = COLA[4], lwd = graphlinewidth)

text(0.5*xlims[2], -3, expression(paste("Market quantity of the good, ", X)), xpd = TRUE, cex = axislabelsize) 
text(-19, 0.5*ylims[2], expression(paste("Price per unit of the good, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 


segments(0, 10, 50, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(50, 0, 50, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(50, 10, pch = 16, col = "black", cex = 1.5)
text(52.5, 10.5, expression(e), cex = labelsize)
text(70, 14, expression(paste(abs(eta[e]) == 5*bgroup("(",frac(10,50), ")"), phantom() == 1)), cex = labelsize)
Arrows(60, 12.8, 54.5, 11.2, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

segments(0, 14, 30, 14, lty = 2, "gray" , lwd = segmentlinewidth)
segments(30, 0, 30, 14, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(30, 14, pch = 16, col = "black", cex = 1.5)
text(32.5, 14.5, expression(f), cex = labelsize)
text(52, 18, expression(paste(abs(eta[f]) == 5*bgroup("(",frac(14,30), ")"), phantom() == 2.33)), cex = labelsize)
Arrows(40, 16.8, 34.5, 15.2, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


segments(0, 6, 70, 6, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(70, 0, 70, 6, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(70, 6, pch = 16, col = "black", cex = 1.5)
text(72.5, 6.5, expression(g), cex = labelsize)
text(92, 10, expression(paste(abs(eta[g]) == 5*bgroup("(",frac(6,70), ")"), phantom() == 0.43)), cex = labelsize)
Arrows(80, 8.8, 74.5, 7.2, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label mrs function
text(20, 20, expression(paste(p(X) == 20 - frac(1,5)*X)), cex = labelsize)


dev.off()
