require(shape)
pdf(file = "competitionmarkets/profit_rate_n.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(6, 6, 4, 4))

Profit <- function(n, pmax = 20, c1 = 10, s = 0.5) {
  (pmax - c1)^2 / (((n + 1)^2)*s)
}

mrsA <- function(x, rmax = 20, xmax = 10) {
  rmax - (rmax/xmax)*x
}

uA <- function(x, y, rmax = 20, xmax = 10) {
  y + rmax*x - (1/2)(rmax/xmax)*x^2
}

totaln <- function(pi, pmax = 20, c1 = 10, s = 0.5){
  (pmax - c1)/(sqrt(pi*s)) - 1
}

Supply <- function(x, c1 = 0, c2 = 0.05){
  c1 + 2*c2*x
}

xlims <- c(0, 20)
ylims <- c(0, 16)

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
ticksy <- c(0, 4, 7, ylims[2])
ylabels <- c(NA, expression(paste(rho[1])), expression(paste(rho[2])), NA)
ticksx <- c(0, 4.3, 6, 9.6, 13, xlims[2])
xlabels <- c(NA, expression(paste(n[1])), expression(paste(n[2])), expression(paste(n[3])), expression(paste(n[4])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Lines for mrs graph
lines(xx1, Profit(xx1, c1 = 10), col = COLA[4], lwd = graphlinewidth)
lines(xx1, Profit(xx1, c1 = 0.25), col = COLA[4], lty = 2, lwd = segmentlinewidth)

#Label axes
mtext(expression(paste("Number of firms, ", n)), side=1, line = 2.5, cex = axislabelsize)
text(-2, 0.5*ylims[2], expression(paste("Opportunity Cost of Capital and Profit, ", list(rho, pi) )), xpd = TRUE, cex = axislabelsize, srt = 90) 


Arrows(18, 6.8, 18, 4.5, col = "black", lty = 1, code = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(16, 5.75, expression(paste("Decrease in ", rho)), cex = labelsize)


text(19, 7.4, expression(paste(rho[2])), cex = labelsize)
text(19, 4.4, expression(paste(rho[1])), cex = labelsize)

Arrows(4, 9, 7.8, 9, col = "black", lty = 1, code = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(5.7, 10, expression(paste("Decrease in ", c[1])), cex = labelsize)

text(3.1, 15, expression(paste(pi[1])), cex = labelsize)
text(6.7, 15, expression(paste(pi[2])), cex = labelsize)


segments(0, 7, xlims[2], 7, lty = 1, col = COLB[4] , lwd = segmentlinewidth)
segments(0, 4, xlims[2], 4, lty = 2, col = COLB[3], lwd = segmentlinewidth)
segments(4.3, 0, 4.3, 7, lty = 2, col = "gray", lwd = segmentlinewidth)

 
# text(80, 12.5, expression(paste("Excess Supply at ", p^H)), cex = labelsize)
# 
# Arrows(62, 5.5, 82, 5.5, col = "black", lty = 1, code = 3, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# text(72, 4.75, expression(paste("Excess")), cex = labelsize)
# text(72, 3.75, expression(paste("Demand")), cex = labelsize)
# text(72, 2.75, expression(paste("at ", p^L)), cex = labelsize)
segments(6, 0, 6, 4, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(6, 4, pch = 16, col = "black", cex = 1.5)


segments(9.6, 0, 9.6, 7, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(9.6, 7, pch = 16, col = "black", cex = 1.5)

segments(13, 0, 13, 4, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(13, 4, pch = 16, col = "black", cex = 1.5)

#Label Demand
text(118, 3, expression(paste("Opportunity cost'")), cex = labelsize)
text(118, 2, expression(paste("of capital")), cex = labelsize)

dev.off()
