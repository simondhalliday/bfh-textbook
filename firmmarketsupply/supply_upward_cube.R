#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "firmmarketsupply/supply_upward_cube.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 7, 1, 1))

AvgRevenue <- function(x, rmax = 12, xmax = 12){
  rmax - (rmax/xmax)*x
}

MRevenue <- function(x, rmax = 12, xmax = 12){
  rmax - 2*(rmax/xmax)*x
}

Cost <- function(x, c0 = 6, c1 = 2, c2 = -0.5, c3 = 0.3){
  c0 + c1*x + c2*x^2 + c3*x^3 
}

AvgCost <- function(x, c0 = 6, c1 = 2, c2 = -1, c3 = 0.2){
  c0/x + c1 + c2*x + c3*x^2
}

AvgVarCost <- function(x, c0 = 6, c1 = 2, c2 = -1, c3 = 0.2){
  c1 + c2*x + c3*x^2
}

MCost <- function(x, c1 = 2, c2 = -1, c3 = 0.2){
  c1 + 2*c2*x + 3*c3*x^2
}

xlims <- c(0, 8)
ylims <- c(0, 5)

npts <- 501 

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs = "i", 
     yaxs = "i")


ticksy <- c(0, 2, MCost(x = 5/2), MCost(x = 3.63514), ylims[2])
ylabels <- c(NA, expression(paste(c[0])), expression(paste(p[min])),expression(paste(p[Break])), NA)
ticksx <- c(0, 5/2, 3.63514, xlims[2])
xlabels <- c(NA, expression(paste(x[min])), expression(paste(x[Break])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(5/2, 3.63514, length.out = npts)
xx4 <- seq(3.63514, xlims[2], length.out = npts)
xx3 <- seq(0, 5/2, length.out = npts)


lines(xx1, AvgCost(xx1), col = COLA[6], lwd = graphlinewidth)
lines(xx1, AvgVarCost(xx1), col = COLA[5], lwd = graphlinewidth)
lines(xx4, MCost(xx4), col = COLA[4], lwd = graphlinewidth)
lines(xx2, MCost(xx2), col = COLA[4], lwd = graphlinewidth, lty = 2)
lines(xx3, MCost(xx3), col = COL[2], lwd = graphlinewidth, lty = 2)

#Label the axes
mtext(expression(paste("Quantity, ", x)), side = 1, line = 3, cex = axislabelsize)
#mtext(expression(paste("Quantity of output, ", x)), side = 1, line = 2.5, cex = axislabelsize)
text(-1.2, 0.5*ylims[2], expression(paste("Price, Revenue and Costs, ", list(p, r, ac), " and ", mc)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label curves
text(6.1, 4.9, expression(paste(ac(x))), cex = labelsize)
text(7.5, 4.9, expression(paste(avc(x))), cex = labelsize)
text(3.5, 4.9, expression(paste("Firm's")), cex = labelsize)
text(3.5, 4.7, expression(paste("supply curve")), cex = labelsize)
text(3.5, 4.5, expression(paste(mc(x))), cex = labelsize)

#Draw segments for total costs
segments(0, MCost(x = 5/2), 3.63514, MCost(x = 5/2), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(5/2, 0, 5/2, MCost(x = 5/2), lty = 2, col = "gray" , lwd = segmentlinewidth)


segments(0, MCost(x = 3.63514), 3.63514, MCost(x = 3.63514), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(3.63514, 0, 3.63514, MCost(x = 3.63514), lty = 2, col = "gray" , lwd = segmentlinewidth)


#Label Points for comparison
points(3.63514, MCost(x = 3.63514), pch = 16, col = "black", cex = 1.5)
text(3.63514 + 0.15, MCost(x = 3.63514) - 0.15, expression(a), cex = labelsize)

points(5/2, MCost(x = 5/2), pch = 16, col = "black", cex = 1.5)
text(5/2 + 0.15, MCost(x = 5/2) - 0.15, expression(b), cex = labelsize)

#Arrow to for sr losses
text(1.5, 2.22, expression(paste(p[min] < p, phantom() < p[Break])), cex = labelsize)
text(1.45, 2, expression(paste("short-run losses")), cex = labelsize)
text(1.5, 1.78, expression(paste("firm supplies x")), cex = labelsize)
text(1.5, 1.6, expression(paste("in short run")), cex = labelsize)
text(1.5, 1.38, expression(paste("not long run")), cex = labelsize)

Arrows(2.5 , MCost(x = 5/2) + 0.2, 2.5, MCost(x = 3.63514) - 0.2, col = "black", code = 3, lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#Arrow to for sr shutdown
text(5.5, 0.7, expression(paste(p < p[min])), cex = labelsize)
text(5.5, 0.45, expression(paste("unable to cover fixed costs")), cex = labelsize)
text(5.5, 0.25, expression(paste("firm shuts down")), cex = labelsize)
Arrows(3.8, 0.1, 3.8, MCost(x = 5/2) - 0.1, col = "black", code = 3, lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


dev.off()
