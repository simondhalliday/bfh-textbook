require(shape)
pdf(file = "competitionmarkets/demand_supply_excess_D.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(5, 6, 1, 1))

Demand <- function(x, rmax = 20, xmax = 12, n = 10) {
  rmax - (rmax/(n*xmax))*x
}

mrsA <- function(x, rmax = 20, xmax = 10) {
  rmax - (rmax/xmax)*x
}

uA <- function(x, y, rmax = 20, xmax = 10) {
  y + rmax*x - (1/2)(rmax/xmax)*x^2
}


#Supply <- function(x, c1 = 0, c2 = 0.05){
#  c1 + 2*c2*x
#}

# New Supply
Supply <- function(x, c1 = 0.5, c2 = 0.0465){
  c1 + 2*c2*x
}

xlims <- c(0, 130)
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
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
# ticksx <- seq(from = 0, to = xlims[2], by = 2)
# xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(0, 6, 7.5, Demand(x = 60), 20, ylims[2])
ylabels <- c(NA, expression(paste(p^L)), expression(paste(p,"*")), expression(paste(wtp(X^{SL}))), expression(paste(bar(p))), NA)
ticksx <- c(0,  60, 75, 84, 120, xlims[2])
xlabels <- c(NA, expression(paste(X^{SL})), expression(paste(X,"*")), expression(paste(X^{DL})), expression(paste(bar(p)/beta)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], 75, length.out = npts)
xx2 <- seq(75, xlims[2], length.out = npts)

#Lines for mrs graph
lines(xx1, Demand(xx1), col = COLA[4], lwd = graphlinewidth)
lines(xx2, Demand(xx2), col = COLA[4], lty = 2, lwd = segmentlinewidth)
lines(xx1, Supply(xx1), col = COLB[4], lwd = graphlinewidth)
lines(xx2, Supply(xx2), col = COLB[4], lty = 2, lwd = segmentlinewidth)

#Label axes
#mtext(expression(paste("Market quantity of output, ", X)), side=1, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], -2.5, expression(paste("Market quantity of output, ", X)), xpd = TRUE, cex = axislabelsize) 
text(-17, 0.5*ylims[2], expression(paste("Price per unit of x, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 


# segments(0, 7.5, 75, 7.5, lty = 2, "gray" , lwd = segmentlinewidth)
# segments(75, 0, 75, 7.5, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(75, 7.5, pch = 16, col = "black", cex = 1.5)
text(75, 8.25, expression(c), cex = labelsize)

segments(0, Supply(x = 60), 84, Demand(x = 84), lty = 2, "gray" , lwd = segmentlinewidth)
segments(60, 0, 60, Demand(x = 60), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, Demand(x = 60), 60, Demand(x = 60), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(84, 0, 84, Demand(x = 84), lty = 2, col = "gray" , lwd = segmentlinewidth)
points(60, Supply(x = 60), pch = 16, col = "black", cex = 1.5)
points(84, Demand(x = 84), pch = 16, col = "black", cex = 1.5)
points(60, Demand(x = 60), pch = 16, col = "black", cex = 1.5)
text(84, Demand(x = 84) + 0.75, expression(g), cex = labelsize)
text(60 - 2, Supply(x = 60) + 0.75, expression(e), cex = labelsize)
text(60 - 2, Demand(x = 60) - 0.75, expression(f), cex = labelsize)

Arrows(54, 6.5, 54, 9.5, col = "black", lty = 1, code = 3, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(36, 8, expression(paste("Buyers' rent at ", p^L)), cex = labelsize)


# 
# points(50, 10, pch = 16, col = "black", cex = 1.5)
# text(52, 10.5, expression(M))

#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
text(118, 13.75, expression("Sellers'"), cex = labelsize)
text(118, 12.75, expression("Supply"), cex = labelsize)
#text(7.3, 2.5, expression("Curve"))

# Arrows(56, 11.5, 108, 11.5, col = "black", lty = 1, code = 3, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# text(80, 12.5, expression(paste("Excess Supply at ", p^H)), cex = labelsize)

Arrows(62, 5.5, 82, 5.5, col = "black", lty = 1, code = 3, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(72, 4.75, expression(paste("Excess")), cex = labelsize)
text(72, 3.75, expression(paste("Demand")), cex = labelsize)
text(72, 2.75, expression(paste("at ", p^L)), cex = labelsize)


#Label Demand
text(118, 3, expression(paste("Buyers'")), cex = labelsize)
text(118, 2, expression(paste("Demand")), cex = labelsize)

#doesn't seem to be giving me a new pdf 
dev.off()
