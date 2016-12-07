#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "competitionmarkets/cournot_efficiency_many.pdf", width = 9, height = 7)

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

AvgRevenue <- function(x, s = 0.5, pmax = 20, c1 = 2, n = 17,  xbar = 2){
  pmax -s*(n-1)*xbar -s*x
}

MRevenue <- function(x,  s = 0.5, pmax = 20, c1 = 2, n = 17,  xbar = 2){
  pmax - s*(n-1)*xbar -2*s*x
}

AvgCost <- function(x, c0 = 2, c1 = 2){
  c0/x + c1
}

xN <- function(n, s = 0.5, pmax = 20, c1 = 2){
  (pmax - c1)/((n+1)*s)
}
#Using xN defined above, a firm has a unit output when there are 35 firms. 
#So I will re-draw this with n = 35


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

# ticksy <- seq(from = ylims[1], to = ylims[2], by = 1)
# ylabels <- seq(from = ylims[1], to = ylims[2], by = 1)
# ticksx <- seq(from = xlims[1], to = xlims[2], by = 1)
# xlabels <- seq(from = xlims[1], to = xlims[2], by = 1)
ticksy <- c(0, 2, AvgRevenue(x = 6), ylims[2])
ylabels <- c(NA, expression(paste(p,"*", phantom()== c[1])), expression(paste(p^{oli})), expression(paste(p[max] - gamma)))
ticksx <- c(0, 6, 12, xlims[2])
xlabels <- c(NA, expression(paste(x^{oli})), expression(paste(x^{many})), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(2, 6, length.out = npts)

#Draw the polygon for profit
xpoly1 <- c(0, 6, 6, 0, 0)
ypoly1 <- c(2, 2, AvgRevenue(x = 6), AvgRevenue(x = 6), 2)
polygon(x = xpoly1, y = ypoly1, col=COLA[1], density=NULL, border = NA)

#Draw the polygon for costs
xpoly2 <- c(0, 6, 6, 0, 0)
ypoly2 <- c(0, 0, 2, 2, 2)
polygon(x = xpoly2, y = ypoly2, col=COLB[1], density=NULL, border = NA)

#Draw the polygon for deadweight loss
xpoly3 <- c(6, 6, 12, 6)
ypoly3 <- c(2, AvgRevenue(x = 6), 2, 2)
polygon(x = xpoly3, y = ypoly3, col=COL[4], density=NULL, border = NA)

#Draw the polygon for consumer surplus
xpoly4 <- c(0, 6, 0, 0)
ypoly4 <- c(AvgRevenue(x = 6), AvgRevenue(x = 6), AvgRevenue(x = 0), AvgRevenue(x = 6))
polygon(x = xpoly4, y = ypoly4, col=COLC[3], density=NULL, border = NA)


#lines(xx1, bcA(xx1, w = 10, p = 1.5), col = COLB[3], lwd = graphlinewidth)
lines(xx1, AvgRevenue(xx1), col = COLB[5], lwd = graphlinewidth)
lines(xx1, MRevenue(xx1), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, AvgCost(xx1, c0 = 2, c1 = 4), col = COLA[5], lwd = graphlinewidth)

#Label the axes
mtext(expression(paste("Quantity of output, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-2.1, 0.5*ylims[2], expression(paste("Price, Revenue and Costs, ", list(p, r, ac), " and ", mc)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label curves
text(13.5, 2.3, expression(paste(ac(x) == mc(x))), cex = labelsize)
text(13.1, 1, expression(paste(p(x))), cex = labelsize)
text(7.8, 1, expression(paste(mr(x))), cex = labelsize)

#Labels cost and profit areas
text(3, 0.5*AvgCost(x = 4), expression("Total Costs"), cex = labelsize)
text(3, 3.5, expression("Profit"), cex = labelsize)
# text(5.25, 5.5, expression("Deadweight"), cex = labelsize)
# text(5.25, 5, expression("Loss"), cex = labelsize)

#Draw segments for total costs
segments(0, AvgRevenue(x = 6), 6, AvgRevenue(x = 6), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(6, 0, 6, AvgRevenue(x = 6), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 2, xlims[2], 2, lty = 1, col = COL[1] , lwd = graphlinewidth)

#Segment for many firms comparison
segments(12, 0, 12, 2, lty = 2, col = "gray" , lwd = segmentlinewidth)

#Label Points for comparison
points(6, MRevenue(x = 6), pch = 16, col = "black", cex = 1.5)
text(5.7, MRevenue(x = 6) - 0.2, expression(i), cex = labelsize)


points(6, AvgRevenue(x = 6), pch = 16, col = "black", cex = 1.5)
text(6.3, AvgRevenue(x = 6) + 0.2, expression(h), cex = labelsize)


# Arrow to Consumer surplus
text(7.3, 7.5, expression(paste("Consumer Surplus")), cex = labelsize)
#text(6, 11, expression(paste(cs(x) == frac(1,2)*(p[max] - p^{mon})*x^{mon} )), cex = labelsize)
Arrows(5.4, 7.5, 2.9, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

# Arrow to Deadweight loss
text(11.6, 5.1, expression(paste("Deadweight Loss")), cex = labelsize)
#text(9, 7, expression(paste(dwl(x) == frac(1,2)*(p^{mon} - c[1])*(x^{many} - x^{mon}))), cex = labelsize)
Arrows(9.9, 5.1, 7.4, 3.6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(12, 7.8, expression(paste("Note:")), cex = labelsize)
text(12, 7.45, expression(paste(gamma == s*(n-1)*bar(x))), cex = labelsize)
segments(10.5, 7.2, 13.5, 7.2, lty = 1, col = "black" , lwd = 1)
segments(10.5, 8, 13.5, 8, lty = 1, col = "black" , lwd = 1)
segments(10.5, 7.2, 10.5, 8, lty = 1, col = "black" , lwd = 1)
segments(13.5, 7.2, 13.5, 8, lty = 1, col = "black" , lwd = 1)


dev.off()
