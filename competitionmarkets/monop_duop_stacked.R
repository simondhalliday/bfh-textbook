#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
library(pBrackets)
pdf(file = "competitionmarkets/monop_duop_stacked.pdf", width = 9, height = 12)

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
grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 8, 0.2, 0.2), mfrow = c(2, 1))

AvgRevenue <- function(x, rmax = 12, xmax = 12){
  rmax - (rmax/xmax)*x
}

MRevenue <- function(x, rmax = 12, xmax = 12){
  rmax - 2*(rmax/xmax)*x
}

AvgCost <- function(x, c0 = 2, c1 = 4){
  c0/x + c1
}

xlims <- c(0, 12.5)
ylims <- c(0, 12.5)

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
ticksy <- c(0, 4, AvgRevenue(x = 4), 12, ylims[2])
ylabels <- c(NA, expression(paste(c)), expression(paste(p^{m})), expression(paste(bar(p))), NA)
ticksx <- c(0, 4, 6, 12, xlims[2])
xlabels <- c(NA, expression(paste(x^{Am})), expression(paste(frac(bar(p),2*beta))), expression(paste(frac(bar(p),beta))), NA)

segments(4, -20, 4, AvgRevenue(x = 4), lty = 2, col = grays[20] , lwd = segmentlinewidth, xpd = TRUE)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(2, 6, length.out = npts)

#Draw the polygon for profit
xpoly1 <- c(0, 4, 4, 0, 0)
ypoly1 <- c(4, 4, AvgRevenue(x = 4), AvgRevenue(x = 4), 4)
polygon(x = xpoly1, y = ypoly1, col=COLA[1], density=NULL, border = NA)

#Draw the polygon for costs
xpoly2 <- c(0, 4, 4, 0, 0)
ypoly2 <- c(0, 0, 4, 4, 4)
polygon(x = xpoly2, y = ypoly2, col=COLB[1], density=NULL, border = NA)

#lines(xx1, bcA(xx1, w = 10, p = 1.5), col = COLB[3], lwd = graphlinewidth)
lines(xx1, AvgRevenue(xx1, rmax = 12, xmax = 12), col = COLB[5], lwd = graphlinewidth)
lines(xx1, MRevenue(xx1, rmax = 12, xmax = 12), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, AvgCost(xx1, c0 = 2, c1 = 4), col = COLA[5], lwd = graphlinewidth)

#Label the axes
#text(0.5*(xlims[2]), -2.5, expression(paste("Quantity, ", x)), xpd = TRUE, cex = axislabelsize) 
#text(0.5*(xlims[2]), -2.5, expression(paste("Output, ", x)), xpd = TRUE, cex = axislabelsize) 
#mtext(expression(paste("Quantity of output, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-2.2, 0.5*ylims[2], expression(paste("Price, Revenue and Costs, ($)")), xpd = TRUE, srt = 90, cex = axislabelsize) 

#Label curves
text(10.8, 4.5, expression(paste(ac(x) == mc(x),phantom() == c)), cex = labelsize, xpd=TRUE)
text(11.2, 2.4, expression(paste(p(x) == bar(p) - beta*x)), cex = labelsize)
text(5.5, 2.4, expression(paste(mr(x))), cex = labelsize)

#Labels cost and profit areas
text(2, 0.5*AvgCost(x = 4), expression("Total costs"), cex = labelsize)
text(2, 6, expression("Economic"), cex = labelsize)
text(2, 5.4, expression("profit"), cex = labelsize)

#Draw segments for total costs
segments(0, AvgRevenue(x = 4), 4, AvgRevenue(x = 4), lty = 2, col = grays[20] , lwd = segmentlinewidth)
#segments(4, 0, 4, AvgRevenue(x = 4), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, 4, xlims[2], 4, lty = 1, col = CBCols[1] , lwd = graphlinewidth)

#Label Points for comparison
points(4, MRevenue(x = 4), pch = 16, col = "black", cex = 1.5)
text(3.8, MRevenue(x = 4) - 0.4, expression(i), cex = labelsize)


points(4, AvgRevenue(x = 4), pch = 16, col = "black", cex = 1.5)
text(4.2, AvgRevenue(x = 4) + 0.4, expression(h), cex = labelsize)


#Arrow to mr = mc
text(7.5, 9.6, expression(paste("Profit maximum at")), cex = labelsize)
text(7.5, 9, expression(paste(mr == mc)), cex = labelsize)
Arrows(6.1, 9.2, 4.2, 4.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

axis(1, at = ticksx, pos = 0, labels = FALSE, cex.axis=labelsize)
text(x = c(0, 4, 6, 12, xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE, cex = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis=labelsize)



AvgRevenue <- function(xA,  xB = 0, pbar = 12, beta = 1){
  pbar - (beta)*(xA + xB)
}

#p = pbar - beta(xA + xB)
#=>r(xA) = (pbar - beta(xA + xB))xA 
#=>r(xA) = pbar*xA - beta(xA)^2 - beta(xA*xB)
#=>mr(xA) = pbar - 2beta*x^A - beta*xB
#mc = 5 = 12 - x^A - 4

MRevenue <- function(xA, xB = 0, pbar = 12, beta = 1){
  pbar - 2*beta*(xA) - beta*xB
}

MRevResid <- function(xA, xB = 0, pbar = 12, beta = 1){
  pbar - 2*beta*(xA) + beta*xB
}

AvgCost <- function(x, c0 = 2, c1 = 4){
  c0/xA + c1
}

xlims <- c(0, 12.5)
ylims <- c(0, 12.5)

#Code three relevant points for x, 
#1. A's output, xhigh, when A is a monopolist. 
#2. A's output, xlow, when A and B are in a duopoly and B produces the NE output 
#3. the x intercept for A's mr curve for xB > 0

xlow <- function(c, beta = 1, pbar = 12){
  (pbar - c)/(3*beta)
}

xhigh <- function(c, beta = 1, pbar = 12){
  (pbar - c)/(2*beta)
}

mrxInt <- function(xB, pbar = 12, beta = 1){
  (pbar - beta*xB)/(2*beta)
}

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
     xaxs="i", 
     yaxs="i")

#Draw the polygon for profit
xpoly1 <- c(8/3, 2*8/3, 2*8/3, 8/3, 8/3)
ypoly1 <- c(4, 4, AvgRevenue(xA = 8/3, xB = 8/3), AvgRevenue(xA = 8/3, xB = 8/3), 4)
polygon(x = xpoly1, y = ypoly1, col=COLA[1], density=NULL, border = NA)

#Draw the polygon for costs
xpoly2 <- c(8/3, 2*8/3, 2*8/3, 8/3, 8/3)
ypoly2 <- c(0, 0, 4, 4, 4)
polygon(x = xpoly2, y = ypoly2, col=COLB[1], density=NULL, border = NA)

#Segment for xM from top figure
segments(4, 0, 4, ylims[2], lty = 2, col = grays[20] , lwd = segmentlinewidth, xpd = TRUE)


#Labels cost and profit areas
text(3.7, 2, expression("Total costs"), cex = labelsize)
text(3.7, 5.3, expression("Economic"), cex = labelsize)
text(3.7, 4.7, expression("profit"), cex = labelsize)


# ticksy <- seq(from = ylims[1], to = ylims[2], by = 1)
# ylabels <- seq(from = ylims[1], to = ylims[2], by = 1)
# ticksx <- seq(from = xlims[1], to = xlims[2], by = 1)
# xlabels <- seq(from = xlims[1], to = xlims[2], by = 1)
ticksy <- c(0, 4, AvgRevenue(xA = 8/3, xB = 8/3), MRevenue(xA = 0, xB = 2.667), AvgRevenue(xA = 0), ylims[2])
ylabels <- c(NA, expression(paste(c)), expression(paste(p^N)), expression(paste(bar(p) - beta*x^B )), expression(paste(bar(p))), NA)
ticksx <- c(0, 8/3, 2*8/3, mrxInt(xB = 2.667) + 8/3,  12, xlims[2])
xlabels2 <- c(NA, NA, expression(paste(frac(bar(p),beta))), NA )
#xlabels2 <- c(NA, expression(paste(x^{AN})), expression(paste(frac(bar(p) - beta*x^B,2*beta ))), expression(paste(frac(bar(p),beta))), NA )
ticksy2 <- c(ylims[1], ylims[2])

axis(1, at = ticksx, pos = 0, labels = FALSE, cex.axis = labelsize)
text(x = c(0, mrxInt(xB = 2.667) + 8/3,  12, xlims[2]), par("usr")[3] - 0.4, labels = xlabels2, srt = 0, pos = 1, xpd = TRUE, cex = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize, col = grays[20])
axis(2, at = ticksy2, pos = 8/3, labels = NA, las = 1, cex.axis = labelsize)


npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(8/3, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 8/3, length.out = npts)
xx4 <- seq(8/3, xlims[2], length.out = npts)

#lines(xx1, bcA(xx1, w = 10, p = 1.5), col = COLB[3], lwd = graphlinewidth)
#lines(xx1, AvgRevenue(xx1, beta = 1), col = COLB[5], lwd = graphlinewidth)
#lines(xx1, MRevenue(xx1, beta = 1), col = COLB[4], lwd = graphlinewidth)
lines(xx3, AvgRevenue(xx3, xB = 0, beta = 1), col = COLB[5], lwd = segmentlinewidth, lty = 2)
lines(xx4, AvgRevenue(xx4, xB = 0, beta = 1), col = COLB[5], lwd = graphlinewidth)
#lines(xx1, AvgRevenue(xx1, xB = 2.666667, beta = 1), col = COLB[5], lwd = graphlinewidth)
#lines(xx1, MRevenue(xx1, xB = 2.666667, beta = 1), col = COLB[4], lwd = graphlinewidth)
lines(xx2, MRevResid(xx2, xB = 2.666667, beta = 1), col = COLB[4], lwd = graphlinewidth)


#lines(xx1, AvgCost(xx1, c0 = 2, c1 = 4), col = COLA[5], lwd = graphlinewidth)

#Label the axes
text(0.5*(xlims[2]), -2.6, expression(paste("Quantity of output, ", X)), xpd = TRUE, cex = axislabelsize) 
#mtext(expression(paste("Quantity  of output, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-2.2, 0.5*ylims[2], expression(paste("Price, Revenue and Costs, ($)")), xpd = TRUE, srt = 90, cex = axislabelsize) 
#text(-0.8, 0.5*ylims[2], expression(paste("Price, Revenue and Costs, ($), ", list(p, r, ac), " and ", mc)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label curves
text(10.8, 4.5, expression(paste(ac(x) == mc(x),phantom() == c)), cex = annotatesize)
text(10.8, 2.8, expression(paste(p(x) == bar(p) - beta*x)), cex = annotatesize)

text(8.9, 0.7, expression(paste(mr^A, " when ", x^B == x^{BN} )), cex = annotatesize)

#text(xlims[2] - 1, ylims[2] - 1, expression(paste(mr^A == bar(p) - 2*beta*x^A - beta*x^B)), cex = labelsize)

# text(4, 5.8, expression(paste(mr[2]^A, " when ", x^B == 0)), cex = annotatesize)
# text(4.6, 5.8, expression(paste(p^A, " when ", x^B > 0 )), cex = annotatesize)
# text(4.6, 8.8, expression(paste(p^A, " when ", x^B == 0 )), cex = annotatesize)


#Draw segments for quantities
#segments(xlow(c = 4), 0, xlow(c = 4), 4, lty = 2, col = grays[20] , lwd = segmentlinewidth)
#segments(xhigh(c = 4), 0, xhigh(c = 4), 4, lty = 2, col = grays[20] , lwd = segmentlinewidth)

#Segment for pb - 2beta xb
# segments(8/3, 0, 8/3, AvgRevenue(xA = 8/3), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, AvgRevenue(xA = 8/3), 8/3, AvgRevenue(xA = 8/3), lty = 2, col = grays[20] , lwd = segmentlinewidth)

#With xX and xB
segments(2*8/3, 0, 2*8/3, AvgRevenue(xA = 8/3, xB = 8/3), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, AvgRevenue(xA = 8/3, xB = 8/3), 2*8/3, AvgRevenue(xA = 8/3, xB = 8/3), lty = 2, col = grays[20] , lwd = segmentlinewidth)

#Marginal cost
segments(0, 4, 8/3, 4, lty = 2, col = CBCols[1] , lwd = segmentlinewidth)
segments(8/3, 4, xlims[2], 4, lty = 1, col = CBCols[1] , lwd = graphlinewidth)




#Arrow to mr = mc
# text(7.5, 9.5, expression(paste("Profit Maximum at")), cex = labelsize)
# text(7.5, 9, expression(paste(mr == mc)), cex = labelsize)
# Arrows(6.1, 9.2, 4.2, 4.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


brackets(8/3-0.1, -0.1, 0.1, -0.1,  h = 0.5,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = segmentlinewidth, lty = 1, xpd = TRUE)
text(0.5*8/3, -1, expression(paste(x^{BN} )), cex = annotatesize, xpd = TRUE)


brackets(2*8/3 - 0.1, -0.1, 8/3 + 0.1, -0.1,  h = 0.5,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = segmentlinewidth, lty = 1, xpd = TRUE)
text(8/3 + 0.5*8/3, -1, expression(paste(x^{AN} )), cex = annotatesize, xpd = TRUE)


brackets(8/3 + 0.1, AvgRevenue(xA = 0, xB = 8/3), 12 - 0.1, AvgRevenue(xA = 0, xB = 8/3),  h = 0.5,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = segmentlinewidth, lty = 1, xpd = TRUE)

text(10/2 + 8/3, 11, expression(paste("Firm A's residual demand")), cex = annotatesize, xpd = TRUE)
text(10/2 + 8/3, 10.2, expression(paste("given Firm B producing ", x^{BN})), cex = annotatesize, xpd = TRUE)

#Arrow to mr = mc
text(8.5, 7.1, expression(paste("Profit maximum at")), cex = labelsize)
text(8.5, 6.5, expression(paste(mr == mc)), cex = labelsize)
Arrows(7.5, 6.8, 2*8/3 + 0.15, 4 + 0.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label Points for comparison
#points(4, MRevenue(x = 4), pch = 16, col = "black", cex = 1.5)
#text(3.8, MRevenue(x = 4) - 0.4, expression(i), cex = labelsize)


points(2*8/3, AvgRevenue(xA = 8/3, xB = 8/3),
       pch = 16, col = "black", cex = 1.5)
text(2*8/3 + 0.2, AvgRevenue(xA = 8/3, xB = 8/3)+ 0.3,
     expression(f), cex = labelsize)

points(2*8/3, MRevenue(xA = 8/3, xB = 8/3),
       pch = 16, col = "black", cex = 1.5)
text(2*8/3 - 0.2, MRevenue(xA = 8/3, xB = 8/3) - 0.3,
     expression(g), cex = labelsize)



dev.off()
