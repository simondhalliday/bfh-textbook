#Graph Designer: Simon Halliday
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
#pdf(file = "firmmarketsupply/perf_comp_costs.pdf", width = 9, height = 7)
png(file = "firmmarketsupply/perf_comp_costs_cube_outlier.png", width = 6*120, height = 4.5*120)

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
CBCols <- c("#009E73","#0072B2","#E69F00")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 8, 1, 1))

AvgRevenue <- function(x, rmax = 12, xmax = 12){
  rmax - (rmax/xmax)*x
}

MRevenue <- function(x, rmax = 12, xmax = 12){
  rmax - 2*(rmax/xmax)*x
}

TotCost <- function(x, c0 = 20, c1 = 6, c2 = -1, c3 = 0.1){
  c0 + c1*x + c2*(x^2) + c3*(x^3)
}

AvgCost <- function(x, c0 = 20, c1 = 6, c2 = -1, c3 = 0.1){
  #  TotCost(x) / x
  c0 / x + c1 + c2*x + c3*(x^2)
}

MCost <- function(x, c1 = 6, c2 = -1, c3 = 0.1){
  # deriv(TotCost(x), x)
  c1 + 2*c2*x + 3*c3*(x^2)
}

eq2 <- uniroot(function(x)  MCost(x) - AvgCost(x) , c(.01,40), tol=1e-8)   
eq2[1]

prices <- c(as.numeric(eq2[1]) - 2.5, as.numeric(eq2[1]), as.numeric(eq2[1]) + 2)

eq1 <- uniroot(function(x)  MCost(x) - prices[1]  , c(1,1000), tol=1e-8)   
eq1[1]


eq3 <- uniroot(function(x)  MCost(x) - prices[3]  , c(.01,1000), tol=1e-8)   
eq3[1]

eqmQ <- as.numeric(c(eq1[1], eq2[1], eq3[1]))
eqmQ[2]

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
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = ylims[1], to = ylims[2], by = 1)
# ylabels <- seq(from = ylims[1], to = ylims[2], by = 1)
# ticksx <- seq(from = xlims[1], to = xlims[2], by = 1)
# xlabels <- seq(from = xlims[1], to = xlims[2], by = 1)
ticksy <- c(0, AvgCost(eqmQ[2]), NA, ylims[2])
ylabels <- c(NA, expression(paste(p[c] == 6.75)), NA, NA)
ticksx <- c(0, eqmQ[2], NA, xlims[2])
xlabels <- c(NA, expression(paste(q[c] == 7)), NA, NA)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(2, 6, length.out = npts)

# Draw the polygon for profit
xpoly1 <- c(0, 0, eqmQ[2], eqmQ[2], 0)
ypoly1 <- c(0, min(AvgCost(xx1)), min(AvgCost(xx1)), 0, 0)
polygon(x = xpoly1, y = ypoly1, col=COLB[1],density=NULL, border = NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis=labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1,cex.axis=labelsize)

#Label the costs
text(3, 0.7*AvgCost(x = eqmQ[2]), expression("Total costs"), cex = labelsize)

#Draw the polygon for costs

#xpoly2 <- c(0, 4, 4, 0, 0)
#ypoly2 <- c(0, 0, 4, 0, 0)
#polygon(x = xpoly2, y = ypoly2, col=COLB[1], density=NULL, border = NA)

# lines(xx1, bcA(xx1, w = 10, p = 1.5), col = COLB[3], lwd = graphlinewidth)
# lines(xx1, AvgRevenue(xx1, rmax = 12, xmax = 12), col = COLB[5], lwd = graphlinewidth)
# lines(xx1, MRevenue(xx1, rmax = 12, xmax = 12), col = COLB[4], lwd = graphlinewidth)
lines(xx1, AvgCost(xx1), col = COLA[5], lwd = graphlinewidth)
lines(xx1, MCost(xx1), col = CBCols[1], lwd = graphlinewidth)

# Label the axes

# text(0.5*(xlims[2]), -1.8, expression(paste("Output, ", x)), xpd = TRUE, cex = axislabelsize) 
text(0.5*(xlims[2]), -1.5, expression(paste("Quantity (1000s), ", q[i])), xpd = TRUE, cex = axislabelsize) 
#text(0.5*(xlims[2]), -2.5, expression(paste("Output, ", x)), xpd = TRUE, cex = axislabelsize) 
#mtext(expression(paste("Quantity of output, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-2, 0.5*ylims[2], expression(paste("Price per unit, revenues, and costs ($)")), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label curves

text(10, AvgCost(eqmQ[2]) - 0.5, expression(paste("ar = mr = p")), cex = labelsize)
text(11, 9.75, expression(paste(ac(q[i]))), cex = labelsize)
text(8.1, 11.5, expression(paste(mc(q[i]))), cex = labelsize)

#Labels cost and profit areas (To be added)


#Draw segments for total costs

# segments(0, AvgCost(eqmQ[2]), eqmQ[2], AvgCost(eqmQ[2]),
#          lty = 2, col = grays[20] , lwd = segmentlinewidth
# )
segments(eqmQ[2], 0, eqmQ[2], AvgCost(eqmQ[2]),
         lty = 2, col = grays[20] , lwd = segmentlinewidth
)
segments(0, AvgCost(eqmQ[2]), xlims[2], AvgCost(eqmQ[2]),
         lty = 1, col = CBCols[2] , lwd = graphlinewidth
)

# Label Points for comparison

points(eqmQ[2], AvgCost(eqmQ[2]), pch = 16, col = "black", cex = 1.5)
# text(3.8, MRevenue(x = 4) - 0.4, expression(i), cex = labelsize)


# points(4, AvgRevenue(x = 4), pch = 16, col = "black", cex = 1.5)
# text(4.2, AvgRevenue(x = 4) + 0.4, expression(h), cex = labelsize)

# Arrows (Labels to be added)

text(eqmQ[2] - 1, 8.9, expression(paste("Profit maximum at")), cex = labelsize)
text(eqmQ[2] - 1, 8.3, expression(paste(mr == mc)), cex = labelsize)
shape::Arrows(eqmQ[2], 8.6, eqmQ[2], AvgCost(eqmQ[2]) + 0.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)



dev.off()