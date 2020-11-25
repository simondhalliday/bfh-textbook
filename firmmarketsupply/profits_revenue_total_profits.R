#Graph Designer: Simon Halliday
#Authors: based on Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
require(pBrackets)
#pdf(file = "firmmarketsupply/profit_revenues_total_profit.pdf", width = 9, height = 12)
png(file = "firmmarketsupply/profit_revenues_total_profit.png", width = 6*120, height = 8*120)

#Set parameters for graphics
axislabelsize <- 2
labelsize <- 1.8
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
layout(matrix(c(1,1,2,2), 2,  byrow = TRUE),
       widths=c(3), heights=c(2,1))
par(mar = c(4,6,1,1))
Revenue <- function(x, rmax = 12, xmax = 12){
  (rmax - (rmax/xmax)*x)*x
}

MRevenue <- function(x, rmax = 12, xmax = 12){
  rmax - 2*(rmax/xmax)*x
}

mrline <- function(x, constant = 0.3181472, slope = 0.125){
  constant + slope*x
}

TotalCost <- function(x, c0 = 2, c1 = 4){
  c0 + c1*x
}

Profits <- function(x, rmax = 12, xmax = 12, c0 = 2, c1 = 4){
  (rmax - (rmax/xmax)*x)*x - (c0 + c1*x)
}

xlims <- c(0, 10)
ylims <- c(0, 40)

npts <- 501 

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

#ticksy <- seq(from = ylims[1], to = ylims[2], by = 2)
ticksy <- c(0,2,18,32, ylims[2])
#ylabels <- seq(from = ylims[1], to = ylims[2], by = 2)
ylabels <- c(0,2,18,32, NA)
# ticksx <- seq(from = xlims[1], to = xlims[2], by = 1)
# xlabels <- seq(from = xlims[1], to = xlims[2], by = 1)
#ticksy <- c(0, TotalCost(x = 0), TotalCost(x = 4), Revenue(x = 4), ylims[2])
#ylabels <- c(NA, expression(paste(c[0])), expression(paste(tc(x[1]))), expression(paste(r(x[1]))), NA)
ticksx <- seq(xlims[1], xlims[2], 1) #c(0, 4, xlims[2])
xlabels <- seq(xlims[1], xlims[2], 1)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(3, 5, length.out = npts)

#Draw the polygon for profit
# xpoly1 <- c(0, 4, 4, 0, 0)
# ypoly1 <- c(AvgCost(x = 4), AvgCost(x = 4), AvgRevenue(x = 4), AvgRevenue(x = 4), AvgCost(x = 4))
# polygon(x = xpoly1, y = ypoly1, col=COLA[1], density=NULL, border = NA)

#Draw the polygon for costs
# xpoly2 <- c(0, 4, 4, 0, 0)
# ypoly2 <- c(0, 0, AvgCost(x = 4), AvgCost(x = 4), AvgCost(x = 4))
# polygon(x = xpoly2, y = ypoly2, col=COLB[1], density=NULL, border = NA)

#lines(xx1, bcA(xx1, w = 10, p = 1.5), col = COLB[3], lwd = graphlinewidth)
lines(xx1, Revenue(xx1, rmax = 12, xmax = 12), col = COLB[5], lwd = graphlinewidth)
lines(xx1, TotalCost(xx1, c0 = 2, c1 = 4), col = COLA[5], lwd = graphlinewidth)
#lines(xx1, Profits(xx1), col = COLA[5], lwd = graphlinewidth)
#lines(xx3, mrline(xx3, constant = Revenue(x = 4) - 4*MRevenue(x = 4), slope = MRevenue(x = 4)), col = "gray", lty = 2, lwd = graphlinewidth)

#Label the axes
text(0.5*xlims[2], -2.8, expression(paste("Quantity (1000s), ", x)), xpd = TRUE, cex = axislabelsize) 
text(-0.8, 0.5*ylims[2], expression(paste("Revenue and Costs ($1000s)")), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label curves
text(8.5, 39.2, expression(paste("Total")), cex = labelsize)
text(8.5, 38, expression(paste("costs")), cex = labelsize)
text(8.5, 27, expression(paste("Total")), cex = labelsize)
text(8.5, 25.8, expression(paste("revenues")), cex = labelsize)


#Labels cost and profit areas
# text(2, 0.5*AvgCost(x = 4), expression("Total Costs"), cex = labelsize)
# text(2, 6, expression("Profit"), cex = labelsize)

#Draw segments for total costs
segments(0, Revenue(x = 4), 4, Revenue(x = 4), lty = 2, col = grays[22] , lwd = segmentlinewidth)
segments(4, 0, 4, Revenue(x = 4), lty = 2, col = grays[22] , lwd = segmentlinewidth)
segments(0, TotalCost(x = 4), 4, TotalCost(x = 4), lty = 2, col = grays[22] , lwd = segmentlinewidth)

#Label Points for comparison
points(4, Revenue(x = 4), pch = 16, col = "black", cex = 1.5)
text(4.2, Revenue(x = 4) - 0.5, expression(a), cex = labelsize)

points(4, TotalCost(x = 4), pch = 16, col = "black", cex = 1.5)
text(4.2, TotalCost(x = 4) - 0.5, expression(b), cex = labelsize)

Arrows(4, Revenue(x = 4) - 1, 4, TotalCost(x = 4) + 1, code = 3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(4.75, 0.8*Revenue(x = 4) + 0.7, expression("Maximum"), cex = labelsize)
text(4.75, 0.8*Revenue(x = 4) - 0.7, expression("profits"), cex = labelsize)

#brackets(4, 32, 4, 18, h = 0.5, ticks = 0.5, curvature = 0.25, type = 1, col = 1, lwd = 1, lty = 1, xpd = FALSE)
# points(4, AvgRevenue(x = 4), pch = 16, col = "black", cex = 1.5)
# text(4.2, AvgRevenue(x = 4) + 0.4, expression(h), cex = labelsize)

xlims <- c(0, 10)
ylims <- c(-2, 18)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs = "i", 
     yaxs = "i")
ticksy <- c(Profits(x = 0), 0, 14, ylims[2])
#ticksy <- c(Profits(x = 0), 0, TotalCost(x = 0), TotalCost(x = 4), Revenue(x = 4), ylims[2])
#ylabels <- c(expression(paste(-c[0])), NA, expression(paste(c[0])), expression(paste(tc(x[1]))), expression(paste(r(x[1]))), NA)
ylabels <- c(Profits(x = 0), 0, 14, ylims[2])
ticksx <- seq(xlims[1], xlims[2], 1) #c(0, 4, xlims[2])
xlabels <- c(NA, seq(1, xlims[2], 1))

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

text(0.5*xlims[2], -3.6, expression(paste("Quantity (1000s), ", x)), xpd = TRUE, cex = axislabelsize) 
text(-0.8, 0.5*ylims[2], expression(paste("Profits ($1000s)")), xpd = TRUE, cex = axislabelsize, srt = 90) 

text(6.7, 10, expression(paste("Profits")), cex = labelsize)
#text(8.5, 25.8, expression(paste("revenues")), cex = labelsize)

lines(xx1, Profits(xx1), col = COLA[5], lwd = graphlinewidth)

segments(2, Profits(x = 4), 6, Profits(x = 4), lty = 2, col = grays[22] , lwd = segmentlinewidth)
points(4, Profits(x = 4), pch = 16, col = "black", cex = 1.5)
text(4, Profits(x = 4) + 1, expression(m), cex = labelsize)


Arrows(4, 0.7, 4, Profits(x = 4) - 0.9, code = 3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(4.75, 7 + 0.7, expression("Maximum"), cex = labelsize)
text(4.75, 7 - 0.7, expression("profits"), cex = labelsize)



dev.off()
