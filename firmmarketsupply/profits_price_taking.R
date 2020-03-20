#Graph Designer: Simon Halliday + Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "firmmarketsupply/profits_price_taking.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 6, 1, 1))

AvgRevenue <- function(x, rmax = 12, xmax = 12){
  rmax - (rmax/xmax)*x
}

MRevenue <- function(x, rmax = 12, xmax = 12){
  rmax - 2*(rmax/xmax)*x
}

AvgCost <- function(x, c0 = 6, c1 = 2, c2 = 0.5){
  c0/x + c1 + c2*x
}

AvgVarCost <- function(x, c0 = 6, c1 = 2, c2 = 0.5){
  c1 + c2*x
}

MCost <- function(x, c1 = 2, c2 = 0.5){
  c1 + 2*c2*x
}

xlims <- c(0, 12)
ylims <- c(0, 12)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(13, 16.75, 19.25)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = ylims[1], to = ylims[2], by = 1)
# ylabels <- seq(from = ylims[1], to = ylims[2], by = 1)
# ticksx <- seq(from = xlims[1], to = xlims[2], by = 1)
# xlabels <- seq(from = xlims[1], to = xlims[2], by = 1)
ticksy <- c(0, AvgCost(x = 6), 8, ylims[2])
ylabels <- c(NA, expression(paste(ac, "(", x,"*)")), expression(paste(p)), NA)
ticksx <- c(0, 6, xlims[2])
xlabels <- c(NA, expression(paste(x,"*")), NA)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(2, 6, length.out = npts)

#Draw the polygon for profit
xpoly1 <- c(0, 6, 6, 0, 0)
ypoly1 <- c(AvgCost(x = 6), AvgCost(x = 6), MCost(x = 6), MCost(x = 6), AvgCost(x = 6))
polygon(x = xpoly1, y = ypoly1, col=COLA[1], density=NULL, border = NA)

#Draw the polygon for costs
xpoly2 <- c(0, 6, 6, 0, 0)
ypoly2 <- c(0, 0, AvgCost(x = 6), AvgCost(x = 6), AvgCost(x = 6))
polygon(x = xpoly2, y = ypoly2, col=COLB[1], density=NULL, border = NA)

#lines(xx1, bcA(xx1, w = 10, p = 1.5), col = COLB[3], lwd = graphlinewidth)
#lines(xx1, AvgRevenue(xx1, rmax = 12, xmax = 12), col = COLB[5], lwd = graphlinewidth)
#lines(xx1, MRevenue(xx1, rmax = 12, xmax = 12), col = COLB[4], lwd = graphlinewidth)
lines(xx1, AvgCost(xx1, c0 = 6, c1 = 2, c2 = 0.5), col = COLA[6], lwd = graphlinewidth)
#lines(xx1, AvgVarCost(xx1, c0 = 10, c1 = 2, c2 = 0.5), col = COLA[5], lwd = graphlinewidth)
lines(xx1, MCost(xx1, c1 = 2, c2 = 0.5), col = COLA[4], lwd = graphlinewidth)

#Label the axes
mtext(expression(paste("Quantity of output, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-1.5, 0.5*ylims[2], expression(paste("Price, Revenue and Costs, ", list(p, r, ac), " and ", mc)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label curves
text(10.5, 7.4, expression(paste(ac(x))), cex = labelsize)
text(10.5, 11.6, expression(paste(mc(x))), cex = labelsize)

text(9, 8.9, expression(paste("Market Price")), cex = labelsize)
text(9, 8.3, expression(paste(p == ar(x), phantom() == mr(x))), cex = labelsize)
#text(5, 0.8, expression(paste(mr(x))), cex = labelsize)

#Labels cost and profit areas
text(3, 0.5*AvgCost(x = 6), expression("Total Costs"), cex = labelsize)
text(3, (MCost(x = 6) - 0.5*(MCost(x = 6) - AvgCost(x = 6))), expression("Profit"), cex = labelsize)

#Draw segments for total costs
segments(0, MCost(x = 6), 6, MCost(x = 6), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(6, 0, 6, MCost(x = 6), lty = 2, col = "gray" , lwd = segmentlinewidth)

segments(0, AvgCost(x = 6), 6, AvgCost(x = 6), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 8, xlims[2], 8, lty = 1, col = COLB[4] , lwd = graphlinewidth)

#Label Points for comparison
points(6, AvgCost(x = 6), pch = 16, col = "black", cex = 1.5)
text(6.2, AvgCost(x = 6) + 0.4, expression(g), cex = labelsize)

points(6, MCost(x = 6), pch = 16, col = "black", cex = 1.5)
text(6, MCost(x = 6) + 0.4, expression(i), cex = labelsize)

#Arrow to mr = mc
text(6, 11.5, expression(paste("Profit Maximum at")), cex = labelsize)
text(6, 11, expression(paste(mr == mc)), cex = labelsize)
Arrows(6, 10.7, 6, 8.5 , col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)


dev.off()
