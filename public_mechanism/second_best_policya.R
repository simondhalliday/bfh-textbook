#Graph Designer: Simon Halliday + Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "public_mechanism/second_best_policya.pdf", width = 9, height = 7)

# Set parameters for graphics
axislabelsize <- 2
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5
fadelevel <- 0.2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")


#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 7, 1, 1))

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

costs <- c(3, 6.5)
monopoly <- c((12 - costs[1])/2, (12 - costs[2])/2)
comp <- c((12 - costs[1]), (12 - costs[2]))

ticksy <- c(0, costs[1], costs[2], AvgRevenue(monopoly[1]),  AvgRevenue(monopoly[2]), ylims[2])
ylabels <- c(NA, expression(paste(p == mpc)), expression(paste(p == msc)), expression(paste(p^{MP})),  expression(paste(p^{MS})), NA)
ticksx <- c(0, monopoly[2], monopoly[1], comp[2], comp[1], xlims[2])
xlabels <- c(NA,  expression(paste(X^{MS})), expression(paste(X^{MP})), expression(paste(X^{CS})), expression(paste(X^{CP})), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(2, 6, length.out = npts)

#Draw the polygon for D
xpolya <- c(comp[1], comp[1], comp[2], comp[1])
ypolya <- c(costs[1], costs[2], costs[2], costs[1])
polygon(x = xpolya, y = ypolya, col=COL[4],
      density=NULL, border = NA)


#Monopoly CS no MSC: 
xpolyCSM <- c(comp[2], 0, 0, comp[2])
ypolyCSM <- c(AvgRevenue(x = comp[2]), 12, AvgRevenue(x = comp[2]), AvgRevenue(x = comp[2]))
polygon(x = xpolyCSM, y = ypolyCSM, col=COLA[1], density=NULL, border = NA)

#Monopoly profit no MSC, above MSC: 
# xpolyMpi <- c(monopoly[1], monopoly[1], 0, 0, monopoly[1])
# ypolyMpi <- c(costs[2], AvgRevenue(x = monopoly[1]), AvgRevenue(x = monopoly[1]), costs[2], costs[2])
# polygon(x = xpolyMpi, y = ypolyMpi, col=COLB[1], density=NULL, border = NA)

#Monopoly profit no MSC, BELOW MSC: 

xpolyMpiSC <- c(monopoly[1], monopoly[1], 0, 0, monopoly[1])
ypolyMpiSC <- c(costs[1], costs[2], costs[2], costs[1], costs[1])
polygon(x = xpolyMpiSC, y = ypolyMpiSC, col=rgb(204/255, 121/255, 167/255, fadelevel), 
        density=NULL, border = NA)


#Draw the polygon for A
# xpolyb <- c(comp[2], monopoly[1], monopoly[1], comp[2])
# ypolyb <- c(costs[2], AvgRevenue(x = monopoly[1]), costs[2], costs[2])
# polygon(x = xpolyb, y = ypolyb, col=COL[4], density=NULL, border = NA)

#Draw the polygon for area C
# xpolyc <- c(monopoly[1], monopoly[1], monopoly[2], monopoly[2], monopoly[1])
# ypolyc <- c(costs[2], AvgRevenue(x = monopoly[1]), AvgRevenue(x = monopoly[2]), MRevenue(monopoly[2]), MRevenue(monopoly[2]))
# polygon(x = xpolyc, y = ypolyc, col = COLA[1], density = NULL, border = NA)

#Draw the polygon for D
xpolyd <- c(comp[2], comp[2], monopoly[1], monopoly[1], comp[2] )
ypolyd <- c(costs[1], AvgRevenue(x = comp[2]), AvgRevenue(x = comp[2]), costs[1], costs[1])
polygon(x = xpolyd, y = ypolyd, col=COL[2], density=NULL, border = NA)

#Area E
xpolye <- c(comp[1], comp[2], comp[2], comp[1])
ypolye <- c(costs[1], AvgRevenue(x = comp[2]), costs[1], costs[1])
polygon(x = xpolye, y = ypolye, col=COL[2], density=NULL, border = NA)

#Total costs only covering private costs 
xpolya <- c(comp[1], comp[1], 0, 0, comp[1])
ypolya <- c(0, costs[1], costs[1], 0, 0)
polygon(x = xpolya, y = ypolya, col=rgb(86/255, 180/255, 233/255, fadelevel),
        density=NULL, border = NA)

#lines(xx1, bcA(xx1, w = 10, p = 1.5), col = COLB[3], lwd = graphlinewidth)
lines(xx1, AvgRevenue(xx1, rmax = 12, xmax = 12), col = COLB[5], lwd = graphlinewidth)
lines(xx1, MRevenue(xx1, rmax = 12, xmax = 12), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, AvgCost(xx1, c0 = 2, c1 = 4), col = COLA[5], lwd = graphlinewidth)

#Label the axes
#mtext(expression(paste("Quantity of output, ", X)), side=1, line = 2.5, cex = axislabelsize)

text(0.5*xlims[2], -1.2, expression(paste("Quantity of output, ", X)), xpd = TRUE, cex = axislabelsize) 
text(-2, 0.5*ylims[2], expression(paste("Price, revenue, and costs, $")), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label curves
#text(10.5, 4.5, expression(paste(ac(x) == mc(x))), cex = labelsize)
#text(10.5, 3, expression(paste(p(x) == p[max] - s*x)), cex = labelsize)
text(10.5, 0.5, expression(paste("Demand")), cex = labelsize)

text(6.6, 1, expression(paste("Marginal")), cex = labelsize)
text(6.6, 0.5, expression(paste("revenue")), cex = labelsize)

text(11.2, 6.2, expression(paste("Marginal")), cex = labelsize)
text(11.2, 5.7, expression(paste("social cost")), cex = labelsize, xpd = TRUE)
text(11.2, 5.2, expression(paste("(msc)")), cex = labelsize)

text(11.2, 2.7, expression(paste("Marginal")), cex = labelsize)
text(11.2, 2.2, expression(paste("private cost")), cex = labelsize, xpd = TRUE)
text(11.2, 1.7, expression(paste("(mpc)")), cex = labelsize)

#Labels cost and profit areas
#text(2, 0.5*AvgCost(x = 4), expression("Total Costs"), cex = labelsize)
#text(2, 6, expression("Profit"), cex = labelsize)

#Draw segments for total costs
segments(0, AvgRevenue(x = monopoly[2]), monopoly[2], AvgRevenue(x = monopoly[2]), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, AvgRevenue(x = 4.5), 4.5, AvgRevenue(x = 4.5), lty = 2, col = grays[20] , lwd = segmentlinewidth)

segments(monopoly[2], 0, monopoly[2], AvgRevenue(x = monopoly[2]), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(4.5, 0, 4.5, AvgRevenue(x = 4.5), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(comp[2], 0, comp[2], AvgRevenue(x = comp[2]), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(9, 0, 9, costs[2], lty = 2, col = grays[20] , lwd = segmentlinewidth)

#Price lines
segments(0, 3, xlims[2], 3, lty = 1, col = CBCols[1] , lwd = graphlinewidth)
segments(0, 6.5, xlims[2], 6.5, lty = 1, col = CBCols[3] , lwd = graphlinewidth)


#Label Points for comparison
points(monopoly[2], MRevenue(x = monopoly[2]), pch = 16, col = "black", cex = 1.5)
text(monopoly[2] -0.2, MRevenue(x = monopoly[2]) - 0.4, expression(c), cex = labelsize)

points(4.5, MRevenue(x = 4.5), pch = 16, col = "black", cex = 1.5)
text(4.5 -0.2, MRevenue(x = 4.5) - 0.4, expression(b), cex = labelsize)

points(comp[2], AvgRevenue(x = comp[2]), pch = 16, col = "black", cex = 1.5)
text(comp[2] + 0.2, AvgRevenue(x = comp[2]) + 0.4, expression(e), cex = labelsize)

points(9, AvgRevenue(x = 9), pch = 16, col = "black", cex = 1.5)
text(9 -0.2, AvgRevenue(x = 9) - 0.4, expression(a), cex = labelsize)


#points(4, AvgRevenue(x = 4), pch = 16, col = "black", cex = 1.5)
text(7.75, 5.5, expression(F), cex = labelsize)
#text(4.75, 6.75, expression(A), cex = labelsize)
#text(3.5, 7, expression(C), cex = labelsize)
 text(5, 5, expression(D), cex = labelsize)
 text(6.5, 4.25, expression(E), cex = labelsize)

text(1, 8.25, expression(paste(CS[MSC]^{CP})), cex = labelsize)
#text(1, 7, expression(paste(pi[msc]^{MP})), cex = labelsize)
text(1, 4.75, expression(paste(CS[mpc]^{CP})), cex = labelsize)

#Arrow to mr = mc
#text(7.5, 9.5, expression(paste("Profit Maximum at")), cex = labelsize)
#text(7.5, 9, expression(paste(mr == mc)), cex = labelsize)
#Arrows(6.1, 9.2, 4.2, 4.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(2, 2.25, expression(paste("Competition policy:")), cex = labelsize)
text(2, 1.75, expression(paste("competitive firm")), cex = labelsize)
text(2, 1.25, expression(paste("only incurs")), cex = labelsize)
text(2, 0.75, expression(paste("private costs")), cex = labelsize)



dev.off()
