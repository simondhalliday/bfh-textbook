#Graph Designer: Simon Halliday + Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "firmmarketsupply/isoprofit_monopoly_profits.pdf", width = 9, height = 7)

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

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 6, 1, 1))

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


ticksy <- c(0, AvgRevenue(x = 4), AvgRevenue(x = 6.45), 12, ylims[2])
ylabels <- c(NA, expression(paste(p^{m})), expression(paste(p^{bar(m)})), expression(paste(bar(p))), NA)
ticksx <- c(0, 4, 6.45, 12, xlims[2])
xlabels <- c(NA, expression(paste(x^{m})), expression(paste(x^bar(m))), expression(paste(frac(bar(p),beta))), NA)

#Draw the polygon for feasible
xpoly1 <- c(0, 0, 12, 0)
ypoly1 <- c(0, 12, 0, 0)
polygon(x = xpoly1, y = ypoly1, col = COLA[1], density = NULL, border = NA)


axis(1, at = ticksx, pos = 0, labels = FALSE)
text(x = c(0, 4, 6.45, 12, xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE, cex = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(xlims[1], xlims[2] - 2, length.out = npts)
xx3 <- seq(2, 6, length.out = npts)

lines(xx1, AvgRevenue(xx1, rmax = 12, xmax = 12), col = COLA[5], lwd = graphlinewidth)
lines(xx2, IsoProf(xx2, k = 14), col = COLB[4], lwd = graphlinewidth)

# Iso Prof --- Too high/too low
lines(xx2, IsoProf(xx2, k = 8), col = COLB[4], lwd = graphlinewidth)
lines(xx2, IsoProf(xx2, k = 20), col = COLB[4], lwd = graphlinewidth)

#Label the axes
text(0.5*(xlims[2]), -1.8, expression(paste("Quantity, ", x)), xpd = TRUE, cex = axislabelsize) 
#text(0.5*(xlims[2]), -1.8, expression(paste("Output, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-1.5, 0.5*ylims[2], expression(paste("Price, Revenue and Costs, ", list(p, r, ac), " and ", mc)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label curves
text(xlims[2] - 1, 7, expression(paste("Isoprofit \n curves")), cex = labelsize)

text(xlims[2] - 1, 6, expression(paste(pi[3])), cex = labelsize)
text(xlims[2] - 1, 5.25, expression(paste(pi[2])), cex = labelsize)
text(xlims[2] - 1, 4.5, expression(paste(pi[1])), cex = labelsize)

#Draw segments for total costs
segments(0, AvgRevenue(x = 4), 4, AvgRevenue(x = 4), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(4, 0, 4, AvgRevenue(x = 4), lty = 2, col = "gray" , lwd = segmentlinewidth)

segments(0, AvgRevenue(x = 6.45), 6.45, AvgRevenue(x = 6.45), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(6.45, 0, 6.45, AvgRevenue(x = 6.45), lty = 2, col = "gray" , lwd = segmentlinewidth)

points(4, AvgRevenue(x = 4), pch = 16, col = "black", cex = 1.5)
text(4.2, AvgRevenue(x = 4) + 0.5, expression("h"), cex = labelsize)

points(6.45, AvgRevenue(x = 6.45), pch = 16, col = "black", cex = 1.5)
text(6.65, AvgRevenue(x = 6.45) + 0.4, expression("h'"), cex = labelsize)

dev.off()