#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "competitionmarkets/monopolist_deadweightloss_v2.pdf", width = 9, height = 7)

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
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 7, 1, 1))

AvgRevenue <- function(x, rmax = 20, xmax = 40){
  rmax - (rmax/xmax)*x
}

MRevenue <- function(x, rmax = 20, xmax = 40){
  rmax - 2*(rmax/xmax)*x
}

AvgCost <- function(x, c0 = 2, c1 = 4){
  c0/x + c1
}

xlims <- c(0, 44)
ylims <- c(0, 21)

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
ticksy <- c(0, 2, AvgRevenue(x = 18), 20, ylims[2])
#ylabels <- c(NA, expression(paste(p,"*", phantom())), expression(paste(p^{m})), expression(paste(bar(p))))
ylabels <- c(NA, expression(paste(p^c==c, phantom()== 2)), expression(paste(p^{m}==11)), expression(paste(bar(p)==20)), NA)
ticksx <- c(0, 18, 36, xlims[2])
#xlabels <- c(NA, expression(paste(x^{m})), expression(paste(x^{c})), NA)
xlabels <- c(NA, expression(paste(x^{m} == 18)), expression(paste(x^{c}==36)), NA)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(2, 6, length.out = npts)

#Draw the polygon for profit
xpoly1 <- c(0, 18, 18, 0, 0)
ypoly1 <- c(2, 2, AvgRevenue(x = 18), AvgRevenue(x = 18), 18)
polygon(x = xpoly1, y = ypoly1, col=COLA[1], density=NULL, border = NA)

#Draw the polygon for costs
xpoly2 <- c(0, 18, 18, 0, 0)
ypoly2 <- c(0, 0, 2, 2, 2)
polygon(x = xpoly2, y = ypoly2, col=COLB[1], density=NULL, border = NA)

#Draw the polygon for deadweight loss
xpoly3 <- c(18, 18, 36, 18)
ypoly3 <- c(2, AvgRevenue(x = 18), 2, 2)
polygon(x = xpoly3, y = ypoly3, col=COL[4], density=NULL, border = NA)

#Draw the polygon for consumer surplus
xpoly4 <- c(0, 18, 0, 0)
ypoly4 <- c(AvgRevenue(18), AvgRevenue(18), 20, AvgRevenue(18))
polygon(x = xpoly4, y = ypoly4, col=COLC[3], density=NULL, border = NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#lines(xx1, bcA(xx1, w = 10, p = 1.5), col = COLB[3], lwd = graphlinewidth)
lines(xx1, AvgRevenue(xx1, rmax = 20, xmax = 40), col = COLB[5], lwd = graphlinewidth)
lines(xx1, MRevenue(xx1, rmax = 20, xmax = 40), col = CBCols[2], lwd = graphlinewidth)
#lines(xx1, AvgCost(xx1, c0 = 2, c1 = 4), col = COLA[5], lwd = graphlinewidth)

#Label the axes
mtext(expression(paste("Quantity of output, ", X)), side=1, line = 3, cex = axislabelsize)
text(-7, 0.5*ylims[2], expression(paste("Price, revenue and costs, ($)")), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label curves
text(40.5, 2.5, expression(paste(ac == mc, phantom() == c)), cex = labelsize, xpd = TRUE)
text(40.5, 0.9, expression(paste(p(x))), cex = labelsize)
text(21.5, 0.9, expression(paste(mr(x))), cex = labelsize)


#Draw segments for total costs
segments(0, AvgRevenue(x = 18), 18, AvgRevenue(x = 18), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(18, 0, 18, AvgRevenue(x = 18), lty = 2, col = grays[20] , lwd = segmentlinewidth)

#Marginal costs
segments(0, 2, xlims[2], 2, lty = 1, col = CBCols[1] , lwd = graphlinewidth)

#Segment for many firms comparison
segments(36, 0, 36, 2, lty = 2, col = grays[20] , lwd = segmentlinewidth)

#Label Points for comparison
points(18, MRevenue(x = 18), pch = 16, col = "black", cex = 1.5)
text(18 -0.5, MRevenue(x = 18) - 0.5, expression(i), cex = labelsize)

points(18, AvgRevenue(x = 18), pch = 16, col = "black", cex = 1.5)
text(18 + 0.5, AvgRevenue(x = 18) + 0.5, expression(h), cex = labelsize)

#Label Points for comparison
points(36, AvgRevenue(x = 36), pch = 16, col = "black", cex = 1.5)
text(36 -0.5, AvgRevenue(x = 36) - 0.5, expression(j), cex = labelsize)

# Arrow to Consumer surplus
text(16, 16, expression(paste("Consumer surplus")), cex = labelsize)
Arrows(14.4, 15.5, 10, 13, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Labels cost and profit areas
text(9, 1, expression("Total costs"), cex = labelsize)
text(9, 5, expression("Economic"), cex = labelsize)
text(9, 4, expression("profit"), cex = labelsize)
text(24, 5, expression(paste("Deadweight")), cex = labelsize)
text(24, 4, expression(paste("loss")), cex = labelsize)

#text(9, 7, expression(paste(dwl(x) == frac(1,2)*(p^{m} - c)*(x^{c} - x^{m}))), cex = labelsize)
#Arrows(7.4, 6.5, 5.4, 5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)



dev.off()
