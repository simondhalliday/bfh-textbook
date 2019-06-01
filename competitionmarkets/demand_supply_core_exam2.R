require(shape)
pdf(file = "competitionmarkets/demand_supply_core_exam2.pdf", width = 9, height = 7)

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
ticksy <- c(0, 7.5, Demand(86, rmax = 22, xmax = 14), 20, ylims[2])
ylabels <- c(NA, expression(paste(p[2])), expression(paste(p[1])), NA, NA)
ticksx <- c(0, 75, 86, 120, xlims[2])
xlabels <- c(NA, expression(paste(x[2])), expression(paste(x[1])), NA, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = axislabelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = axislabelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Lines for mrs graph
lines(xx1, Demand(xx1), col = COLA[5], lwd = graphlinewidth)
lines(xx1, Demand(xx1, rmax = 22, xmax = 14), col = COLA[4], lwd = graphlinewidth)
lines(xx1, Supply(xx1), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, mrsA(xx1, rmax = 20, xmax = 10), col = COLA[4], lwd = graphlinewidth)

segments(0, 7.5, 75, 7.5, lty = 2, "gray" , lwd = segmentlinewidth)
segments(75, 0, 75, 7.5, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(75, 7.5, pch = 16, col = "black", cex = 1.5)
text(75 - 2, Demand(75) - 0.75, expression(paste(2)), cex = axislabelsize) 

segments(0, Demand(86, rmax = 22, xmax = 14), 86, Demand(86, rmax = 22, xmax = 14), lty = 2, "gray" , lwd = segmentlinewidth)
segments(86, 0, 86, Demand(86, rmax = 22, xmax = 14), lty = 2, "gray" , lwd = segmentlinewidth)
text(86, Demand(86, rmax = 22, xmax = 14) + 0.75, expression(paste(1)), cex = axislabelsize) 

#Label axes
mtext(expression(paste("Market output of ethically produced goods, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-12, 0.5*ylims[2], expression(paste("Price per unit of x, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 
# 

points(86, Demand(86, rmax = 22, xmax = 14), pch = 16, col = "black", cex = 1.5)
# text(52, 10.5, expression(M))

#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
text(100, 10.8, expression("Supply"), cex = labelsize)

#Label mrs function
#text(94, 6.8, expression(paste("Demand: ", X(p) == 120 - 6*p)))
text(25, 13.7, expression(paste("Demand after")), cex = labelsize)
text(127, 4.2, expression(paste("Demand before")), cex = labelsize)
#text(94, 5.5, expression(paste("Inverse Demand: ", p(X) == 20 - frac(1,5)*X)))
#Arrows(10, 7.5, 10, 5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label satiation
#text(20, 3.5, expression(paste(x[max] == "Point")))
#text(20, 3, expression(paste("of Satiation")))
#Arrows(20, 2.5, 20, 0.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label highest willingness to pay
#text(5, 10, expression("Consumer Surplus"))
#text(5, 9, expression(paste(CS==frac(1, 2)*bgroup("(",r[max] - p,")")*x)))
#Arrows(5, 8.5, 5, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

dev.off()
