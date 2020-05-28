#Graph Designer: Simon Halliday + Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "public_mechanism/markets_competition_monopolist_external.pdf", width = 9, height = 7)

# Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 6, 1, 1))

AvgRevenue <- function(x, rmax = 12, xmax = 12){
  rmax - (rmax/xmax)*x
}
  
MRevenue <- function(x, rmax = 12, xmax = 12){
  rmax - 2*(rmax/xmax)*x
}

AvgCost <- function(x, c0 = 2, c1 = 4){
  c0/x + c1
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
     xaxs = "i", 
     yaxs = "i")

ticksy <- c(0, 3, 5, AvgRevenue(x = 4.5), AvgRevenue(3.5),  ylims[2])
ylabels <- c(NA, expression(paste(p^{CP})), expression(paste(p^{CS})), expression(paste(p^{MP})), expression(paste(p^{MS})), NA)
ticksx <- c(0, 3.5, 4.5, 7, 9, xlims[2])
xlabels <- c(NA, expression(paste(X^{MS})), expression(paste(X^{MP})), expression(paste(X^{CS})), expression(paste(X^{CP})), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(2, 6, length.out = npts)

#Draw the polygon for A
# xpolya <- c(7, 9, 7, 7)
# ypolya <- c(3, 3, 5, 3)
# polygon(x = xpolya, y = ypolya, col=COL[4], density=NULL, border = NA)

xpolya <- c(9, 9, 7, 9)
ypolya <- c(3, 5, 5, 3)
polygon(x = xpolya, y = ypolya, col=COL[4], density=NULL, border = NA)


#Draw the polygon for B
xpolyb <- c(4.5, 7, 4.5, 4.5)
ypolyb <- c(5, 5, AvgRevenue(x = 4.5), 5)
polygon(x = xpolyb, y = ypolyb, col=COLB[1], density=NULL, border = NA)

#Draw the polygon for area C
xpolyc <- c(3.5, 4.5, 4.5, 3.5, 3.5)
ypolyc <- c(5, 5, AvgRevenue(x = 4.5), AvgRevenue(x = 3.5), MRevenue(3.5))
polygon(x = xpolyc, y = ypolyc, col = COLA[1], density = NULL, border = NA)


#lines(xx1, bcA(xx1, w = 10, p = 1.5), col = COLB[3], lwd = graphlinewidth)
lines(xx1, AvgRevenue(xx1, rmax = 12, xmax = 12), col = COLB[5], lwd = graphlinewidth)
lines(xx1, MRevenue(xx1, rmax = 12, xmax = 12), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, AvgCost(xx1, c0 = 2, c1 = 4), col = COLA[5], lwd = graphlinewidth)

#Label the axes
mtext(expression(paste("Quantity of output, ", X)), side=1, line = 2.5, cex = axislabelsize)
text(-1.5, 0.5*ylims[2], expression(paste("Price, Revenue and Costs, ", list(p, r, ac), " and ", mc)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label curves
#text(10.5, 4.5, expression(paste(ac(x) == mc(x))), cex = labelsize)
#text(10.5, 3, expression(paste(p(x) == p[max] - s*x)), cex = labelsize)
text(10.5, 0.5, expression(paste("Demand")), cex = labelsize)

text(11, 5.75, expression(paste("Social")), cex = labelsize)
text(11, 5.25, expression(paste("mc")), cex = labelsize)

text(11, 2.7, expression(paste("Private")), cex = labelsize)
text(11, 2.2, expression(paste("mc")), cex = labelsize)

#Labels cost and profit areas
#text(2, 0.5*AvgCost(x = 4), expression("Total Costs"), cex = labelsize)
#text(2, 6, expression("Profit"), cex = labelsize)

#Draw segments for total costs
segments(0, AvgRevenue(x = 3.5), 3.5, AvgRevenue(x = 3.5), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, AvgRevenue(x = 4.5), 4.5, AvgRevenue(x = 4.5), lty = 2, col = grays[20] , lwd = segmentlinewidth)

segments(3.5, 0, 3.5, AvgRevenue(x = 3.5), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(4.5, 0, 4.5, AvgRevenue(x = 4.5), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(7, 0, 7, AvgRevenue(x = 7), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(9, 0, 9, 5, lty = 2, col = grays[20] , lwd = segmentlinewidth)

#Price lines
segments(0, 3, xlims[2], 3, lty = 1, col = COL[1] , lwd = graphlinewidth)
segments(0, 5, xlims[2], 5, lty = 1, col = COL[3] , lwd = graphlinewidth)


#Label Points for comparison
#points(4, MRevenue(x = 4), pch = 16, col = "black", cex = 1.5)
#text(3.8, MRevenue(x = 4) - 0.4, expression(i), cex = labelsize)


#points(4, AvgRevenue(x = 4), pch = 16, col = "black", cex = 1.5)
text(8.25, 4.25, expression(A), cex = labelsize)
text(5.5, 6, expression(B), cex = labelsize)
text(4, 6, expression(C), cex = labelsize)


#Arrow to mr = mc
#text(7.5, 9.5, expression(paste("Profit Maximum at")), cex = labelsize)
#text(7.5, 9, expression(paste(mr == mc)), cex = labelsize)
#Arrows(6.1, 9.2, 4.2, 4.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)



dev.off()
