#Graph Designer: Simon Halliday + Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
require(pBrackets)
pdf(file = "firmmarketsupply/profit_revenues_total_stacked.pdf", width = 9, height = 12)


# Economic Profit ---------------------------------------------------------

#Set parameters for graphics
pointsize <- 1.8
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
par(mar =  c(4.75, 7, .2, 1), mfrow = c(2,1))

Revenue <- function(x, rmax = 12, xmax = 10){
  (rmax - (rmax/xmax)*x)*x
}

MRevenue <- function(x, rmax = 12, xmax = 10){
  rmax - 2*(rmax/xmax)*x
}

mrline <- function(x, constant = 0.3181472, slope = 0.125){
  constant + slope*x
}

TotalCost <- function(x, c0 = 2, c1 = 4){
  c0 + c1*x
}

AvgRevenue <- function(x, rmax = 12, xmax = 10){
  rmax - (rmax/xmax)*x
}

AvgCost <- function(x, c0 = 2, c1 = 4){
  c0/x + c1
}

xlims <- c(0, 10.5)
ylims <- c(0, 40)

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


ticksy <- c(0, TotalCost(x = 4, c0 = 0, c1 = 4), Revenue(x = 4), ylims[2])
ylabels <- c(expression(paste(c[0])), expression(paste(c(x^m))), expression(paste(r(x^m))), NA)
ticksx <- c(0, 4, xlims[2])
xlabels <- c(NA, expression(paste(x^m)), NA)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(3, 5, length.out = npts)

lines(xx1, Revenue(xx1, rmax = 12, xmax = 10), col = COLB[5], lwd = graphlinewidth)
lines(xx1, TotalCost(xx1, c0 = 0, c1 = 4), col = COLA[5], lwd = graphlinewidth)
lines(xx3, mrline(xx3, constant = Revenue(x = 4) - 4*MRevenue(x = 4), slope = MRevenue(x = 4)), col = "gray", lty = 2, lwd = graphlinewidth)

#Label the axes
#mtext(expression(paste("Quantity of output, ", x)), side = 1, line = 2.5, cex = axislabelsize)
text(-1.4, 0.5*ylims[2], expression(paste("Revenue and costs ($), ", r, " and ", c)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label curves
text(7.3, 36, expression(paste("Total costs, ", c(x))), cex = annotatesize)
text(8.3, 21, expression(paste(r(x))), cex = annotatesize)

#Draw segments for total costs
segments(0, Revenue(x = 4), 4, Revenue(x = 4), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(4, -20, 4, TotalCost(x = 4, c0 = 0, c1 = 4), lty = 2, col = "gray" , lwd = segmentlinewidth, xpd = TRUE)
segments(0, TotalCost(x = 4, c0 = 0, c1 = 4), 4, TotalCost(x = 4, c0 = 0, c1 = 4), lty = 2, col = "gray" , lwd = segmentlinewidth)

#Label Points for comparison
points(4, Revenue(x = 4), pch = 16, col = "black", cex = 1.5)
text(4.2, Revenue(x = 4) + 1.5, expression(a), cex = annotatesize)

points(4, TotalCost(x = 4, c0 = 0, c1 = 4), pch = 16, col = "black", cex = 1.5)
text(4.2, TotalCost(x = 4, c0 = 0, c1 = 4) - 0.5, expression(b), cex = annotatesize)

Arrows(4, Revenue(x = 4) - 1.5, 4, TotalCost(x = 4, c0 = 0, c1 = 4) + 1.5, code = 3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(5.0, 1.4*TotalCost(x = 4, c0 = 0, c1 = 4) - 0.75, expression("Total \n economic \n profits"), cex = annotatesize)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)



# Monopolist Profit ------------------------------------------------------------


xlims <- c(0, 10.5)
ylims <- c(0, 10)

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


#Draw the polygon for profit
xpoly1 <- c(0, 4, 4, 0, 0)
ypoly1 <- c(3.35, 3.35, AvgRevenue(x = 4) - 1.2, AvgRevenue(x = 4) - 1.2, 3.35)
polygon(x = xpoly1, y = ypoly1, col = COLA[1], density = NULL, border = NA)

#Draw the polygon for costs
xpoly2 <- c(0, 4, 4, 0, 0)
ypoly2 <- c(0, 0, 3.35, 3.35, 3.35)
polygon(x = xpoly2, y = ypoly2, col = COLB[1], density = NULL, border = NA)


ticksy <- c(0, MRevenue(x = 4) + 0.95, AvgRevenue(x = 4) - 1.2, ylims[2])
ylabels <- c(NA, expression(paste(c)), expression(paste(p^{m})), expression(paste(bar(p))))
ticksx <- c(0, 4, 6, xlims[2] - 0.5, xlims[2])
xlabels <- c(NA, expression(paste(x^{m})), expression(paste(frac(bar(p),2*beta))), expression(paste(frac(bar(p),beta))), NA)

axis(1, at = ticksx, pos = 0, labels = FALSE)
text(x = c(0, 4, 6, xlims[2] - 0.5, xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE, cex = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(2, 6, length.out = npts)


lines(xx1, AvgRevenue(xx1, rmax = 10, xmax = 10), col = COLB[5], lwd = graphlinewidth)
lines(xx1, MRevenue(xx1, rmax = 10, xmax = 12), col = COLB[4], lwd = graphlinewidth)

#Label the axes
text(0.45*(xlims[2]), -1.5, expression(paste("Output, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-1.4, 0.44*ylims[2], expression(paste("Price, revenue and costs, ", list(p, r, ac), " and ", mc)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label curves
text(8.5, MRevenue(x = 4) + 1.5, expression(paste(ac(x) == mc(x),phantom() == c)), cex = annotatesize)
text(8.5, 2.8, expression(paste(p(x) == bar(p) - beta*x)), cex = annotatesize)
text(5.6, 2.8, expression(paste(mr(x) == bar(p) - 2*beta*x)), cex = annotatesize)

#Labels cost and profit areas
text(2, 0.5*(AvgCost(x = 4) - 1.2), expression("Total costs"), cex = annotatesize)
text(2, 4.75, expression("Economic"), cex = annotatesize)
text(2, 4.25, expression("profit"), cex = annotatesize)

#Draw segments for total costs
segments(0, AvgRevenue(x = 4) - 1.2, 4, AvgRevenue(x = 4) - 1.2, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(4, 0, 4, AvgRevenue(x = 4) + 40, lty = 2, col = "gray" , lwd = segmentlinewidth, xpd = TRUE)
segments(0, MRevenue(x = 4) + 0.95, xlims[2], MRevenue(x = 4) + 0.95, lty = 1, col = COL[1] , lwd = graphlinewidth)

#Label Points for comparison
points(4, MRevenue(x = 4) + 0.95, pch = 16, col = "black", cex = 1.5)
text(3.8, MRevenue(x = 4) +  0.55, expression(i), cex = annotatesize)


points(4, AvgRevenue(x = 4) - 1.2, pch = 16, col = "black", cex = 1.5)
text(4.2, AvgRevenue(x = 4) - 0.8, expression(h), cex = annotatesize)

#Arrow to mr = mc
text(7.5, 9.5, expression(paste("Profit maximum at")), cex = annotatesize)
text(7.5, 9, expression(paste(mr == mc)), cex = annotatesize)
Arrows(6.1, 9.2, 4.2, 4.0, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)



dev.off()
