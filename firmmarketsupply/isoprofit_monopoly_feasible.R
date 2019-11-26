#Graph Designer: Simon Halliday + Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "firmmarketsupply/isoprofit_monopoly_feasible.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 5, 1, 1))

AvgRevenue <- function(x, rmax = 12, xmax = 12){
  rmax - (rmax/xmax)*x
}

MRevenue <- function(x, rmax = 12, xmax = 12){
  rmax - 2*(rmax/xmax)*x
}

AvgCost <- function(x, c0 = 2, c1 = 4){
  c0/x + c1
}

# From CORE
IsoProf <- function(x, k = 0, c0 = 2, c1 = 4){
  AvgCost(x) + k/x
}

xlims <- c(0, 15)
ylims <- c(0, 15)

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
ticksy <- c(0,  12, ylims[2])
ylabels <- c(NA, expression(paste(bar(p))), NA)
ticksx <- c(0, 12, xlims[2])
xlabels <- c(NA,  expression(paste(frac(bar(p),beta))), NA)

#Draw the polygon for feasible
xpoly1 <- c(0, 0, 12, 0)
ypoly1 <- c(0, 12, 0, 0)
polygon(x = xpoly1, y = ypoly1, col=COLA[1], density=NULL, border = NA)

#Draw the polygon for infeasible
xpoly2 <- c(0, 0, xlims[2], xlims[2], 12, 0)
ypoly2 <- c(12, ylims[2], ylims[2], 0, 0, 12)
polygon(x = xpoly2, y = ypoly2, col=COLB[1], density=NULL, border = NA)


axis(1, at = ticksx, pos = 0, labels = FALSE)
text(x = c(0, 12, xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(xlims[1], xlims[2] - 2, length.out = npts)
xx3 <- seq(2, 6, length.out = npts)

lines(xx1, AvgRevenue(xx1, rmax = 12, xmax = 12), col = COLA[5], lwd = graphlinewidth)
#lines(xx1, MRevenue(xx1, rmax = 12, xmax = 12), col = COLB[4], lwd = graphlinewidth)
#lines(xx2, IsoProf(xx2, k = 14), col = COLA[4], lwd = graphlinewidth)

# Iso Prof --- Too high/too low
#lines(xx2, IsoProf(xx2, k = 8), col = COLA[4], lwd = graphlinewidth)
#lines(xx2, IsoProf(xx2, k = 20), col = COLA[4], lwd = graphlinewidth)

#Label the axes
text(0.5*(xlims[2]), -1.8, expression(paste("Output, ", x)), xpd = TRUE, cex = axislabelsize) 
#mtext(expression(paste("Quantity of output, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-1.1, 0.5*ylims[2], expression(paste("Price, Revenue and Costs, ", list(p, r, ac), " and ", mc)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label curves
# text(xlims[2] - 1, 7, expression(paste(Iso-Profit)), cex = labelsize)
# text(10.5, 3.5, expression(paste(ac(x) == mc(x))), cex = labelsize)
# text(10.5, 2.5, expression(paste(p(x))), cex = labelsize)
# text(5.8, 2.8, expression(paste(mr(x))), cex = labelsize)

# text(xlims[2] - 1, 6, expression(paste(pi[1])), cex = labelsize)
# text(xlims[2] - 1, 5.25, expression(paste(pi[2])), cex = labelsize)
# text(xlims[2] - 1, 4.5, expression(paste(pi[3])), cex = labelsize)

#Draw segments for total costs
# segments(0, AvgRevenue(x = 4), 4, AvgRevenue(x = 4), lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(4, 0, 4, AvgRevenue(x = 4), lty = 2, col = "gray" , lwd = segmentlinewidth)

#marginal cost
#segments(0, 4, xlims[2], 4, lty = 1, col = COLA[5] , lwd = graphlinewidth)

#Label Points for comparison
# points(4, MRevenue(x = 4), pch = 16, col = "black", cex = 1.5)
# text(3.8, MRevenue(x = 4) - 0.4, expression(i), cex = labelsize)
# 
# 
# points(4, AvgRevenue(x = 4), pch = 16, col = "black", cex = 1.5)
text(2, AvgRevenue(x = 4) + 0.4, expression("Demand Curve"), cex = labelsize)

text(2, 2, expression("Feasible"), cex = labelsize)
text(12, 12, expression("Infeasible"), cex = labelsize)




dev.off()