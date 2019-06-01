#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "competitionmarkets/monopolist_medicine_core.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.8

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 1, 1))

AvgRevenue <- function(x, intercept = 90, slope = 10){
  intercept - slope*x
}

MRevenue <- function(x, intercept = 90, slope = 10){
  intercept - 2*(slope)*x
}

TotRevenue <- function(x, intercept = 90, slope = 10){
  (intercept - slope*x)*x
}

AvgCost <- function(x, c0 = 50, c1 = 10){
  c0/x + c1
}

xlims <- c(0, 10)
ylims <- c(0, 100)

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
ticksy <- c(0, 10, AvgRevenue(x = 10), ylims[2])
ylabels <- c(NA, NA, NA, NA)
ticksx <- c(0, 4, 8, xlims[2])
xlabels <- c(NA, NA, NA, NA)

axis(1, at = ticksx, pos = 0, labels = FALSE)
text(x = c(0, 4, 6, xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(2, 6, length.out = npts)

#Draw the polygon for profit
xpoly1 <- c(0, 4, 4, 0, 0)
ypoly1 <- c(10, 10, AvgRevenue(x = 4), AvgRevenue(x = 4), 10)
polygon(x = xpoly1, y = ypoly1, col=COLA[1], density=NULL, border = NA)

#Draw the polygon for costs
# xpoly2 <- c(0, 4, 4, 0, 0)
# ypoly2 <- c(0, 0, 10, 10, 10)
# polygon(x = xpoly2, y = ypoly2, col=COLB[1], density=NULL, border = NA)

#Draw the polygon for CS
xpoly3 <- c(0, 0, 4, 0)
ypoly3 <- c(50, 90, 50, 50)
polygon(x = xpoly3, y = ypoly3, col=COLB[1], density=NULL, border = NA)

# #Draw the polygon for PS
xpoly4 <- c(4, 4, 8, 4)
ypoly4 <- c(10, 50, 10, 10)
polygon(x = xpoly4, y = ypoly4, col=COL[4], density=NULL, border = NA)


#lines(xx1, bcA(xx1, w = 10, p = 1.5), col = COLB[3], lwd = graphlinewidth)
lines(xx1, AvgRevenue(xx1), col = COLB[5], lwd = graphlinewidth)
#lines(xx1, MRevenue(xx1), col = COLB[4], lwd = graphlinewidth)
lines(xx1, AvgCost(xx1, c0 = 160, c1 = 10), col = COLA[5], lwd = graphlinewidth)

#Label the axes
text(0.5*(xlims[2]), -6, expression(paste("Output, ", Q)), xpd = TRUE, cex = axislabelsize) 
#mtext(expression(paste("Quantity of output, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-0.5, 0.5*ylims[2], expression(paste("Price per unit in $, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label curves
# text(10.5, 4.5, expression(paste(ac(x) == mc(x),phantom() == c[1])), cex = labelsize)
# text(10.5, 2.8, expression(paste(p(x) == bar(p) - beta*x)), cex = labelsize)
# text(5.8, 2.8, expression(paste(mr(x) == bar(p) - 2*beta*x)), cex = labelsize)

#Labels cost and profit areas
# text(2, 0.5*AvgCost(x = 4), expression("Total Costs"), cex = labelsize)
# text(2, 6, expression("Profit"), cex = labelsize)

#Draw segments for total costs
segments(8, 0, 8, AvgRevenue(x = 8), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(4, 0, 4, AvgRevenue(x = 4), lty = 2, col = "gray" , lwd = segmentlinewidth)

#Marginal costs
segments(0, 10, xlims[2], 10, lty = 1, col = COL[1] , lwd = graphlinewidth)

#Label Points for comparison
points(4, MRevenue(x = 4), pch = 16, col = "black", cex = 1.5)
text(4 + 0.2, MRevenue(x = 4) + 2, expression(i), cex = labelsize)


points(4, AvgRevenue(x = 4), pch = 16, col = "black", cex = 1.5)
text(4.2, AvgRevenue(x = 4) + 0.4, expression(h), cex = labelsize)

points(8, AvgRevenue(x = 8) + 0.4, pch = 16, col = "black", cex = 1.5)
text(8 + 0.2, AvgRevenue(x = 8) + 2, expression(j), cex = labelsize)


#Arrow to mr = mc
# text(7.5, 9.5, expression(paste("Profit Maximum at")), cex = labelsize)
# text(7.5, 9, expression(paste(mr == mc)), cex = labelsize)
# Arrows(6.1, 9.2, 4.2, 4.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)



dev.off()
