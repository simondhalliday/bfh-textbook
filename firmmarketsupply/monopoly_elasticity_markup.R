#Graph Designer: Simon Halliday
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
library(pBrackets)
pdf(file = "firmmarketsupply/monopoly_elasticity_markup.pdf", width = 9, height = 7)

#Set parameters for graphics
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
par(mar =  c(6, 8, 1, 1))

AvgRevenue <- function(x, pbar = 12, xbar = 12){
  pbar - (pbar/xbar)*x
}

MRevenue <- function(x, pbar = 12, xbar = 12){
  pbar - 2*(pbar/xbar)*x
}

AvgCost <- function(x, c0 = 0, c1 = 4){
  c0/x + c1
}

# From CORE
IsoProf <- function(x, k = 0, c0 = 0, c1 = 4){
  AvgCost(x) + k/x
}

Elasticity <- function(x, p = 2, pbar = 12, xbar = 12){
  (xbar/pbar)*(p/x)
}


xlims <- c(0, 12.5)
ylims <- c(0, 19)

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


ticksy <- c(0, 4, AvgRevenue(x = 4), 12, 14, ylims[2])
ylabels <- c(NA, expression(paste(c == 4)), expression(paste(p[h]^{m} == 8)), expression(paste(bar(p)[h]^m == 12)), expression(paste(p[g]^{m} == 14)), NA)
ticksx <- c(0, 2.5, 4, 6, 12, xlims[2])
xlabels <- c(NA,  expression(paste(x[g]^{m} == 2.5)), expression(paste(x[h]^{m} == 4)), expression(paste(frac(bar(p)[g]^m,beta[g]^m) == 6)), expression(paste(frac(bar(p)[h]^m,beta[h]^m) == 12)), NA)

axis(1, at = ticksx, pos = 0, labels = FALSE)
text(x = c(0, 2.5, 4, 6, 12, xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE, cex = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(xlims[1], xlims[2] - 1.5, length.out = npts)
xx3 <- seq(2, 6, length.out = npts)

lines(xx1, AvgRevenue(xx1, pbar = 12, xbar = 12), col = COLA[5], lwd = graphlinewidth)
#lines(xx1, AvgRevenue(xx1, pbar = 18, xbar = 7.2), col = COLA[5], lwd = graphlinewidth)
lines(xx1, AvgRevenue(xx1, pbar = 24, xbar = 6), col = COLA[5], lwd = graphlinewidth)
#lines(xx1, MRevenue(xx1, pbar = 12, xbar = 12), col = COLA[4], lwd = graphlinewidth)
#lines(xx1, MRevenue(xx1, pbar = 18, xbar = 7.2), col = COLA[4], lwd = graphlinewidth)
#lines(xx1, MRevenue(xx1, pbar = 24, xbar = 6), col = COLA[4], lwd = graphlinewidth)
lines(xx2, IsoProf(xx2, k = 16), col = COLB[4], lwd = graphlinewidth)
#lines(xx2, IsoProf(xx2, k = 19.6), col = COLB[4], lwd = graphlinewidth)
lines(xx2, IsoProf(xx2, k = 25), col = COLB[4], lwd = graphlinewidth)


# Iso Prof --- Too high/too low
#lines(xx2, IsoProf(xx2, k = 8), col = COLB[4], lwd = graphlinewidth)
#lines(xx2, IsoProf(xx2, k = 20), col = COLB[4], lwd = graphlinewidth)

#Label the axes
text(0.5*(xlims[2]), -3.5, expression(paste("Quantity, ", x)), xpd = TRUE, cex = axislabelsize) 
#text(0.5*(xlims[2]), -1.8, expression(paste("Quantity of output, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-2.3, 0.5*ylims[2], expression(paste("Price and Costs ($)")), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Costs
segments(0, 4, xlims[2] - 2, 4, lty = 1, col = COLB[3] , lwd = graphlinewidth)

#Label curves
#text(xlims[2] - 1, 7, expression(paste("Isoprofit \n curves")), cex = labelsize)

text(11.5, 4, expression(paste(mc == c, phantom() == pi[0])), cex = annotatesize)
text(11.7, 2.2, expression(paste("Initial")), cex = annotatesize)
text(11.7, 1.4, expression(paste("demand")), cex = annotatesize)

text(6.8, 2.4, expression(paste("Less elastic")), cex = annotatesize)
text(6.8, 1.4, expression(paste("demand ")), cex = annotatesize)

#text(4, 14, expression(paste("Less elastic")), cex = annotatesize)
#text(4, 13, expression(paste("demand ")), cex = annotatesize)


#text(4.5, 18.5, expression(paste("Less elastic demand")), cex = annotatesize, xpd = TRUE)
#Arrows(2.5, 18.5, 1.6, 18.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#text(6.7, 2.2, expression(paste(mr(x) == bar(p) - 2*beta*x)), cex = labelsize)

# brackets(x1 = 4 - 0.2, y1 = 4 + 0.1, 
#          x2 = 4 - 0.2, y2 = AvgRevenue(x = 4) - 0.1,  
#          ticks = 0.5, curvature = 0.5, type = 1, 
#          col = "black", lwd = 2, lty = 1, xpd = TRUE)
# text(2, 6.3, expression(paste("Markup")), xpd = TRUE, cex = annotatesize)
# text(2, 5.7, expression(paste(phantom() == p - c)), xpd = TRUE, cex = annotatesize)

# text(xlims[2] - 1, 5.8, expression(paste(pi[3])), cex = labelsize)
# text(xlims[2] - 1, 5.25, expression(paste(pi[2])), cex = labelsize)
# text(xlims[2] - 1, 4.7, expression(paste(pi[1])), cex = labelsize)
# text(xlims[2] - 1, 4, expression(paste(pi[0])), cex = labelsize)

#Draw segments for total costs
segments(0, AvgRevenue(x = 4), 4, AvgRevenue(x = 4), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(4, 0, 4, AvgRevenue(x = 4), lty = 2, col = grays[20] , lwd = segmentlinewidth)


# segments(0, AvgRevenue(x = 2.8, pbar = 18, xbar = 7.2), 2.8, AvgRevenue(x = 2.8, pbar = 18, xbar = 7.2), lty = 2, col = grays[20] , lwd = segmentlinewidth)
# segments(2.8, 0, 2.8, AvgRevenue(x = 2.8, pbar = 18, xbar = 7.2), lty = 2, col = grays[20] , lwd = segmentlinewidth)


segments(0, AvgRevenue(x = 2.5, pbar = 24, xbar = 6), 2.5, AvgRevenue(x = 2.5, pbar = 24, xbar = 6), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(2.5, 0, 2.5, AvgRevenue(x = 2.5, pbar = 24, xbar = 6), lty = 2, col = grays[20] , lwd = segmentlinewidth)



#Label Points for comparison
# points(4, MRevenue(x = 4), pch = 16, col = "black", cex = 1.5)
# text(3.8, MRevenue(x = 4) - 0.4, expression(i), cex = labelsize)


points(4, AvgRevenue(x = 4), pch = 16, col = "black", cex = 1.5)
text(4.2, AvgRevenue(x = 4) + 0.4, expression(h), cex = labelsize)

# points(2.8, AvgRevenue(x = 2.8, pbar = 18, xbar = 7.2), pch = 16, col = "black", cex = 1.5)
# text(2.8 + 0.2, AvgRevenue(x = 2.8, pbar = 18, xbar = 7.2) + 0.4, expression(g), cex = labelsize)

points(2.5, AvgRevenue(x = 2.5, pbar = 24, xbar = 6), pch = 16, col = "black", cex = 1.5)
text(2.5 + 0.2, AvgRevenue(x = 2.5, pbar = 24, xbar = 6) + 0.4, expression(g), cex = labelsize)

text(xlims[2] - 0.8, 6.5, expression(paste(pi[g]^m == 25)), cex = labelsize)
text(xlims[2] - 0.8, 5.25, expression(paste(pi[h]^m) == 16), cex = labelsize)
#text(xlims[2] - 1, 4.7, expression(paste(pi[1])), cex = labelsize)
#text(xlims[2] - 1, 4, expression(paste(pi[0])), cex = labelsize)

#(xbar/pbar)*(p/x)
#text(5.1, 17, expression(paste(abs(eta[g]) == frac(6,24)*bgroup("(",frac(14,2.5), ")"), phantom() == 1.4)), cex = labelsize)
text(4.3, 17, expression(paste(abs(eta[g]) == 1.4)), cex = labelsize)
Arrows(3.5, 16.8, 2.95, 15.2, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#text(6.3, 10.7, expression(paste(abs(eta[h]) == frac(12,12)*bgroup("(",frac(8,4), ")"), phantom() == 2)), cex = labelsize)
text(5.6, 10.7, expression(paste(abs(eta[h]) ==  2)), cex = labelsize)
Arrows(5, 10.5, 4.45, 8.9, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

dev.off()