#Graph Designer: Simon Halliday + Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "firmmarketsupply/isoprofit_monopoly_A.pdf", width = 9, height = 7)

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
par(mar =  c(6, 6, 1, 1))

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
     xaxs = "i", 
     yaxs = "i")


ticksy <- c(0, 4, AvgRevenue(x = 4), 12, ylims[2])
ylabels <- c(NA, expression(paste(c)), expression(paste(p^{m})), expression(paste(bar(p))), NA)
ticksx <- c(0, 4, 6, 12, xlims[2])
xlabels <- c(NA, expression(paste(x^{m})), expression(paste(frac(bar(p),2*beta))), expression(paste(frac(bar(p),beta))), NA)

axis(1, at = ticksx, pos = 0, labels = FALSE)
text(x = c(0, 4, 6, 12, xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE, cex = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(xlims[1], xlims[2] - 1.5, length.out = npts)
xx3 <- seq(2, 6, length.out = npts)

lines(xx1, AvgRevenue(xx1, rmax = 12, xmax = 12), col = COLA[5], lwd = graphlinewidth)
lines(xx1, MRevenue(xx1, rmax = 12, xmax = 12), col = COLA[4], lwd = graphlinewidth)
lines(xx2, IsoProf(xx2, k = 14), col = COLB[4], lwd = graphlinewidth)

# Iso Prof --- Too high/too low
lines(xx2, IsoProf(xx2, k = 8), col = COLB[4], lwd = graphlinewidth)
lines(xx2, IsoProf(xx2, k = 20), col = COLB[4], lwd = graphlinewidth)

#Label the axes
text(0.5*(xlims[2]), -2.5, expression(paste("Quantity, ", x)), xpd = TRUE, cex = axislabelsize) 
#text(0.5*(xlims[2]), -1.8, expression(paste("Quantity of output, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-1.4, 0.5*ylims[2], expression(paste("Price, revenue, and costs, ", list(p, r, ac), " and ", mc)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label curves
text(xlims[2] - 1, 7, expression(paste("Isoprofit \n curves")), cex = labelsize)
text(10.6, 3.5, expression(paste(ac(x) == mc(x))), cex = labelsize)
text(11.7, 2.2, expression(paste(p(x) == bar(p) - beta*x)), cex = labelsize)
text(6.7, 2.2, expression(paste(mr(x) == bar(p) - 2*beta*x)), cex = labelsize)

text(xlims[2] - 1, 5.8, expression(paste(pi[3])), cex = labelsize)
text(xlims[2] - 1, 5.25, expression(paste(pi[2])), cex = labelsize)
text(xlims[2] - 1, 4.7, expression(paste(pi[1])), cex = labelsize)
text(xlims[2] - 1, 4, expression(paste(pi[0])), cex = labelsize)

#Draw segments for total costs
segments(0, AvgRevenue(x = 4), 4, AvgRevenue(x = 4), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(4, 0, 4, AvgRevenue(x = 4), lty = 2, col = grays[20] , lwd = segmentlinewidth)

segments(0, 4, xlims[2] - 1.5, 4, lty = 1, col = COLB[3] , lwd = graphlinewidth)

#Label Points for comparison
points(4, MRevenue(x = 4), pch = 16, col = "black", cex = 1.5)
text(3.8, MRevenue(x = 4) - 0.4, expression(i), cex = labelsize)


points(4, AvgRevenue(x = 4), pch = 16, col = "black", cex = 1.5)
text(4.2, AvgRevenue(x = 4) + 0.4, expression(h), cex = labelsize)




dev.off()