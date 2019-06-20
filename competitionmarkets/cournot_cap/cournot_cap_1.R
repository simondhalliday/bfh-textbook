#Graph Designer: Simon Halliday + Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "competitionmarkets/cournot_cap/cournot_cap_1.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 7, 4, 4))

AvgRevenue <- function(x, s = 0.5, pmax = 20, c1 = 2, n = 5, xbar = 6){
  pmax -s*(n-1)*xbar -s*x
}

MRevenue <- function(x,  s = 0.5, pmax = 20, c1 = 2, n = 5,  xbar = 6){
  pmax - s*(n-1)*xbar -2*s*x
}

AvgCost <- function(x, c0 = 2, c1 = 2){
  c0/x + c1
}

xlims <- c(0, 15)
ylims <- c(0, 8)

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

ticksy <- c(0, 2, AvgRevenue(x = 6), AvgRevenue(x = 4), ylims[2])
ylabels <- c(NA, expression(paste(c[1])), expression(paste(p^{oli})), expression(paste(p^{cap})), expression(paste(p[max] - gamma)))
ticksx <- c(0, 4, 6,  xlims[2])
xlabels <- c(NA, expression(paste(x^{cap})), expression(paste(x^{oli})), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(2, 6, length.out = npts)

#Draw the polygon for profit
# xpoly1 <- c(0, 4, 4, 0, 0)
# ypoly1 <- c(2, 2, AvgRevenue(x = 4), AvgRevenue(x = 4), 2)
# polygon(x = xpoly1, y = ypoly1, col=COLA[1], density=NULL, border = NA)

#Draw the polygon for costs
# xpoly2 <- c(0, 4, 4, 0, 0)
# ypoly2 <- c(0, 0, 2, 2, 2)
# polygon(x = xpoly2, y = ypoly2, col=COLB[1], density=NULL, border = NA)


lines(xx1, AvgRevenue(xx1), col = COLB[5], lwd = graphlinewidth)
lines(xx1, MRevenue(xx1), col = COLB[4], lwd = graphlinewidth)

#Label the axes
mtext(expression(paste("Quantity of output for a firm with a capacity constraint, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-2.1, 0.5*ylims[2], expression(paste("Price, Revenue and Costs, ", list(p, r, ac), " and ", mc)), xpd = TRUE, cex = axislabelsize, srt = 90) 

# text(8.6, 2.3, expression(paste(ac(x) == mc(x))), cex = labelsize)
text(9.2, 4.55, expression(paste(p(x) == p[max] - gamma - s*x)), cex = labelsize)
text(9.2, 1, expression(paste(mr(x) == p[max] - gamma - 2*s*x)), cex = labelsize)

#Labels cost and profit areas
# text(2, 1, expression("Total Costs"), cex = labelsize)
# text(2, 3.5, expression("Profit"), cex = labelsize)

#Draw segments for total costs
# segments(0, AvgRevenue(x = 4), 4, AvgRevenue(x = 4), lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(0, AvgRevenue(x = 6), 6, AvgRevenue(x = 6), lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(6, 0, 6, AvgRevenue(x = 6), lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(0, 2, xlims[2], 2, lty = 1, col = COL[1] , lwd = graphlinewidth)

#Segment for capacity constraint
# segments(4, 0, 4, ylims[2], lty = 1, col = COLC[7] , lwd = graphlinewidth)

#Label Points for comparison
# points(6, MRevenue(x = 6), pch = 16, col = "black", cex = 1.5)
# text(5.7, MRevenue(x = 6) - 0.2, expression(i), cex = labelsize)
# 
# points(6, AvgRevenue(x = 6), pch = 16, col = "black", cex = 1.5)
# text(6.3, AvgRevenue(x = 6) + 0.2, expression(h), cex = labelsize)
# 
# points(4, AvgRevenue(x = 4), pch = 16, col = "black", cex = 1.5)
# text(4.3, AvgRevenue(x = 4) + 0.2, expression(f), cex = labelsize)
# 
# points(4, 2, pch = 16, col = "black", cex = 1.5)
# text(3.7, 2 - 0.2, expression(g), cex = labelsize)

text(12, 7.8, expression(paste("Note:")), cex = labelsize)
text(12, 7.45, expression(paste(gamma == s*(n-1)*bar(x))), cex = labelsize)
segments(10.5, 7.2, 13.5, 7.2, lty = 1, col = "black" , lwd = 1)
segments(10.5, 8, 13.5, 8, lty = 1, col = "black" , lwd = 1)
segments(10.5, 7.2, 10.5, 8, lty = 1, col = "black" , lwd = 1)
segments(13.5, 7.2, 13.5, 8, lty = 1, col = "black" , lwd = 1)

# text(6.1, 7.8, expression(paste("Capacity Constraint")), cex = labelsize)
# text(6, 7.5, expression(paste("binds the firm;")), cex = labelsize)
# text(6.1, 7.2, expression(paste("it produces ", x^{cap} < x^{oli})), cex = labelsize)

dev.off()