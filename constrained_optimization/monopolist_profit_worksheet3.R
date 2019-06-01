#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "competitionmarkets/monopolist_profit_worksheet3.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 1, 1))

AvgRevenue <- function(x, rmax = 12, xmax = 12){
  rmax - (rmax/xmax)*x
}

MRevenue <- function(x, rmax = 12, xmax = 12){
  rmax - 2*(rmax/xmax)*x
}

AvgCost <- function(x, c0 = 1, c1 = 0.75, F = 10){
  F/x + c0 + 0.5*c1*x
}

MCost <- function(x, c0 = 1, c1 = 0.75){
  c0 + c1*x
}


TotCost <- function(x, c0 = 1, c1 = 0.75, F = 10){
  F + c0*x + (0.5)*c1*x^2
}

IsoProfit <- function(x, c0 = 1, c1 = 0.75, F = 20){
  F/x + c0 + 0.5*c1*x
}

xlims <- c(0, 12)
ylims <- c(0, 12)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(25, 60, 75)

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
ticksy <- c(0, 1, 4, AvgRevenue(x = 4), 12, ylims[2])
ylabels <- c(NA, 1, 4, 8, 12, NA)
ticksx <- c(0, 4, 6.3, xlims[2])
xlabels <- c(NA, NA, NA, NA)

axis(1, at = ticksx, pos = 0, labels = FALSE)
text(x = c(0, 4, 6, xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(2, 6, length.out = npts)
# 
# #Draw the polygon for profit
# xpoly1 <- c(0, 4, 4, 0, 0)
# ypoly1 <- c(4, 4, AvgRevenue(x = 4), AvgRevenue(x = 4), 4)
# polygon(x = xpoly1, y = ypoly1, col=COLA[1], density=NULL, border = NA)
# 
#Draw the polygon for DWL
xpoly2 <- c(4, 6.3, 4, 4)
ypoly2 <- c(8, AvgRevenue(6.3), 4, 8)
polygon(x = xpoly2, y = ypoly2, col=COL[4], density=NULL, border = NA)
# 
#Draw the polygon for CS
xpoly3 <- c(0, 0, 4, 0)
ypoly3 <- c(8, 12, 8, 8)
polygon(x = xpoly3, y = ypoly3, col=COLB[1], density=NULL, border = NA)
# 
# #Draw the polygon for PS
xpoly4 <- c(0, 4, 4, 0, 0)
ypoly4 <- c(8, 8, 4, 1, 8)
polygon(x = xpoly4, y = ypoly4, col=COLA[1], density=NULL, border = NA)



#lines(xx1, bcA(xx1, w = 10, p = 1.5), col = COLB[3], lwd = graphlinewidth)
lines(xx1, AvgRevenue(xx1, rmax = 12, xmax = 12), col = COLB[5], lwd = graphlinewidth)
#lines(xx1, MRevenue(xx1, rmax = 12, xmax = 12), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, AvgCost(xx1), col = COLA[5], lwd = graphlinewidth)
lines(xx1, MCost(xx1), col = COLA[5], lwd = graphlinewidth)
lines(xx1, IsoProfit(xx1, F = 22), col = "red", lwd = graphlinewidth)

#Label the axes
text(0.5*(xlims[2]), -1, expression(paste("Output, ", x)), xpd = TRUE, cex = axislabelsize) 
#mtext(expression(paste("Quantity of output, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-0.9, 0.5*ylims[2], expression(paste("Price per unit in $, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label curves
# text(10.5, 4.5, expression(paste(ac(x) == mc(x),phantom() == c[1])), cex = labelsize)
# text(10.5, 2.8, expression(paste(p(x) == bar(p) - beta*x)), cex = labelsize)
# text(5.8, 2.8, expression(paste(mr(x) == bar(p) - 2*beta*x)), cex = labelsize)

#Labels cost and profit areas
# text(2, 0.5*AvgCost(x = 4), expression("Total Costs"), cex = labelsize)
# text(2, 6, expression("Profit"), cex = labelsize)

#Draw segments for total costs
segments(0, AvgRevenue(x = 4), 4, AvgRevenue(x = 4), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(4, 0, 4, AvgRevenue(x = 4), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 4, 4, 4, lty = 2, col = "gray" , lwd = segmentlinewidth)

segments(6.3, 0, 6.3, AvgRevenue(6.3), lty = 2, col = "gray" , lwd = segmentlinewidth)

#Label Points for comparison
points(4, MRevenue(x = 4), pch = 16, col = "black", cex = 1.5)
text(4+ 0.2, MRevenue(x = 4) - 0.3, expression(i), cex = labelsize)


points(4, AvgRevenue(x = 4), pch = 16, col = "black", cex = 1.5)
text(4.2, AvgRevenue(x = 4) + 0.3, expression(h), cex = labelsize)

points(6.3, AvgRevenue(x = 6.3), pch = 16, col = "black", cex = 1.5)
text(6.3 + 0.1, AvgRevenue(x = 6.3) - 0.4, expression(j), cex = labelsize)


#Arrow to mr = mc
# text(7.5, 9.5, expression(paste("Profit Maximum at")), cex = labelsize)
# text(7.5, 9, expression(paste(mr == mc)), cex = labelsize)
# Arrows(6.1, 9.2, 4.2, 4.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)



dev.off()
