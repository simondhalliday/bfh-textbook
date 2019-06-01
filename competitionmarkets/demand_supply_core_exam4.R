require(shape)
pdf(file = "competitionmarkets/demand_supply_core_exam4.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(5, 5, 1, 1))

Demand <- function(x, rmax = 20, xmax = 12, n = 10) {
  rmax - (rmax/(n*xmax))*x
}

mrsA <- function(x, rmax = 20, xmax = 10) {
  rmax - (rmax/xmax)*x
}

uA <- function(x, y, rmax, rmax = 20, xmax = 10) {
  y + rmax*x - (1/2)(rmax/xmax)*x^2
}


Supply <- function(x, c1 = 0.5, c2 = 0.0465){
  c1 + 2*c2*x
}

xlims <- c(0, 140)
ylims <- c(0, 24)

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
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
# ticksx <- seq(from = 0, to = xlims[2], by = 2)
# xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(0, 7.5, Supply(64.5, c1 = 1.5, c2 = 0.06), 20, ylims[2])
ylabels <- c(NA, expression(paste(p[2])), expression(paste(p[1])), NA, NA)
ticksx <- c(0, 64.5, 75, 120, xlims[2])
xlabels <- c(NA, expression(paste(x[1])), expression(paste(x[2])), NA, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = axislabelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = axislabelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Lines for mrs graph
lines(xx1, Demand(xx1), col = COLA[4], lwd = graphlinewidth)
#lines(xx1, Demand(xx1, rmax = 22, xmax = 14), col = COLA[5], lwd = graphlinewidth)
lines(xx1, Supply(xx1), col = COLB[5], lwd = graphlinewidth)
lines(xx1, Supply(xx1, c1 = 1.5, c2 = 0.06), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, mrsA(xx1, rmax = 20, xmax = 10), col = COLA[4], lwd = graphlinewidth)

segments(0, 7.5, 75, 7.5, lty = 2, "gray" , lwd = segmentlinewidth)
segments(75, 0, 75, 7.5, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(75, 7.5, pch = 16, col = "black", cex = 1.5)
text(75 - 2, Demand(75) - 0.75, expression(paste(2)), cex = axislabelsize) 

segments(0, Supply(64.5, c1 = 1.5, c2 = 0.06), 64.5, Supply(64.5, c1 = 1.5, c2 = 0.06), lty = 2, "gray" , lwd = segmentlinewidth)
segments(64.5, 0, 64.5, Supply(64.5, c1 = 1.5, c2 = 0.06), lty = 2, "gray" , lwd = segmentlinewidth)
text(64.5, Supply(64.5, c1 = 1.5, c2 = 0.06)+ 0.75, expression(paste(1)), cex = axislabelsize) 
points(64.5, Supply(64.5, c1 = 1.5, c2 = 0.06), pch = 16, col = "black", cex = 1.5)


#Label axes
mtext(expression(paste("Market output of ethically produced goods, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-12, 0.5*ylims[2], expression(paste("Price per unit of x, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 
# 


#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
text(126, 10.8, expression("Supply after"), cex = labelsize)
text(126, 18.2, expression("Supply before"), cex = labelsize)

#Label mrs function
text(28, 13.7, expression(paste("Demand")), cex = labelsize)

dev.off()
