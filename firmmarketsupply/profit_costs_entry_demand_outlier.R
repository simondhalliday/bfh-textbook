#Graph Designer: Simon Halliday + Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
#pdf(file = "firmmarketsupply/profit_costs.pdf", width = 9, height = 7)
png(file = "firmmarketsupply/profit_costs_entry_demand_outlier.png", width = 7*120, height = 5*120)

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.8
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 5, 0.5, 0.5))

AvgRevenue <- function(x, rmax = 8.9, xmax = 8.9){
  rmax - (rmax/xmax)*x
}

MRevenue <- function(x, rmax = 8.9, xmax = 8.9){
  rmax - 2*(rmax/xmax)*x
}

AvgCost <- function(x, c0 = 6, c1 = 4){
  c0/x + c1
}

costs <- c(4)
eq1 <- uniroot(function(x)  MRevenue(x) - costs[1]  , c(1,1000), tol=1e-8)   
entQ <- c(as.numeric(eq1[1]))

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
     xaxs = "i", 
     yaxs = "i")


ticksy <- c(0,  ylims[2])
ylabels <- c(NA, NA)
ticksx <- c(0,  xlims[2])
xlabels <- c(NA, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(2, 6, length.out = npts)

#Draw the polygon for profit
# xpoly1 <- c(0, entQ[1], entQ[1], 0, 0)
# ypoly1 <- c(AvgCost(x = entQ[1]), AvgCost(x =entQ[1]), AvgRevenue(x = entQ[1]), AvgRevenue(x = entQ[1]), AvgCost(x = entQ[1]))
# polygon(x = xpoly1, y = ypoly1, col = COLA[1], density = NULL, border = NA)

#Draw the polygon for costs
# xpoly2 <- c(0, entQ[1], entQ[1], 0, 0)
# ypoly2 <- c(0, 0, AvgCost(x = entQ[1]), AvgCost(x = entQ[1]), AvgCost(x = entQ[1]))
# polygon(x = xpoly2, y = ypoly2, col = COLB[1], density = NULL, border = NA)

lines(xx1, AvgRevenue(xx1), col = COLB[5], lwd = graphlinewidth)
lines(xx1, AvgRevenue(xx1, rmax = 12, xmax = 12), lty = 2, col = COLB[5], lwd = segmentlinewidth)
lines(xx1, MRevenue(xx1), col = COLB[4], lwd = graphlinewidth)
lines(xx1, MRevenue(xx1, rmax = 12, xmax = 12), lty = 2, col = COLB[4], lwd = segmentlinewidth)
#lines(xx1, AvgCost(xx1), col = COLA[5], lwd = graphlinewidth)

#Label the axes
text(0.5*xlims[2], -1, expression(paste("Quantity (1000s), ", q)), xpd = TRUE, cex = axislabelsize) 
#mtext(expression(paste("Quantity, ", x)), side = 1, line = 2.5, cex = axislabelsize)
#mtext(expression(paste("Quantity of output, ", x)), side = 1, line = 2.5, cex = axislabelsize)
#text(-3, 0.5*ylims[2], expression(paste("Price, Revenue and Costs, ", list(p, r, ac), " and ", mc)), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(-0.7, 0.5*ylims[2], expression(paste("Price per unit, revenue, and costs ($) ")), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label curves
# text(10.5, 4.8, expression(paste(ac(q))), cex = labelsize)
# text(10.5, 3.6, expression(paste(mc == c)), cex = labelsize)

text(10.5, 1.2, expression(paste("old")), cex = labelsize)
text(10.5, 0.8, expression(paste(ar(q))), cex = labelsize)

text(8.61, 1.2, expression(paste("new")), cex = labelsize)
text(8.61, 0.8, expression(paste(ar(q))), cex = labelsize)


text(6.1, 1.2, expression(paste("old")), cex = labelsize)
text(6.1, 0.8, expression(paste(mr(q))), cex = labelsize)

text(4.54, 1.2, expression(paste("new")), cex = labelsize)
text(4.54, 0.8, expression(paste(mr(q))), cex = labelsize)

#Labels cost and profit areas
#text(1.2, 0.4*AvgCost(x = entQ[1]), expression("Total costs"), cex = annotatesize)
# text(1.5, 6.25, expression("Economic"), cex = annotatesize)
# text(1.5, 5.75, expression("profit"), cex = annotatesize)

#Draw segments for total costs
# segments(0, AvgRevenue(x = entQ[1]), entQ[1], AvgRevenue(x = entQ[1]), lty = 2, col = grays[20] , lwd = segmentlinewidth)
# segments(entQ[1], 0, entQ[1], AvgRevenue(x = entQ[1]), lty = 2, col = grays[20] , lwd = segmentlinewidth)

# segments(0, AvgCost(x = entQ[1]), entQ[1], AvgCost(x = entQ[1]), lty = 2, col = grays[20] , lwd = segmentlinewidth)
# segments(0, 4, xlims[2], 4, lty = 1, col = COL[1] , lwd = graphlinewidth)

#Label Points for comparison
# points(entQ[1], MRevenue(x = entQ[1]), pch = 16, col = "black", cex = 1.5)
# text(entQ[1] - 0.2, MRevenue(x = entQ[1]) - 0.4, expression(i), cex = labelsize)

# points(entQ[1], AvgCost(x =entQ[1]), pch = 16, col = "black", cex = 1.5)
# text(entQ[1] + 0.2, AvgCost(x = entQ[1]) + 0.4, expression(g), cex = labelsize)

# points(entQ[1], AvgRevenue(x = entQ[1]), pch = 16, col = "black", cex = 1.5)
# text(entQ[1] - 0.2, AvgRevenue(x = entQ[1]) - 0.3, expression(h), cex = labelsize)


#Arrow to mr = mc
# text(7.5, 8, expression(paste("Profit maximum at")), cex = annotatesize)
# text(7.5, 7.5, expression(paste(mr == mc)), cex = annotatesize)

#Shift of demand
text(5.1, 5.2, expression(paste("Entry shifts")), cex = annotatesize)
text(5.1, 4.7, expression(paste("demand")), cex = annotatesize)
Arrows(6.1, 5.8, 6.1, 3.1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)



dev.off()
