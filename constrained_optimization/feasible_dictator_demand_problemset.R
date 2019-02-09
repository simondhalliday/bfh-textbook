#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Microeconomics: Competition, Conflict and Coordination

library(shape)
pdf(file = "constrained_optimization/feasible_dictator_demand_problemset.pdf", width = 8, height = 8)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 4, 4))

ppf <- function(x, slope = 1, bary = 20) {
  bary - slope * x
}

demand <- function(x, lambda = 0.2, endowment = 20){
 (endowment*lambda)/x
}

uFn <- function(x, y, alpha = 0.8){
  ((x)^alpha)*(y^(1-alpha))
}

indiffA <- function(x, ubar = uFn(8,2) - 1, alpha = 0.3) {
  (ubar/ ((x)^alpha))^(1/ (1 - alpha))
}

ylims <- c(0, 4)
xlims <- c(0, 10)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(uFn(16, 2), uFn(16, 4), uFn(16, 8)) #alpha = 0.8

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i"
)


# ticksy <- seq(from = 0, to = ylims[2], by = 1)
# ylabels <- seq(from = 0, to = ylims[2], by = 1)
ticksx <- seq(from = 0, to = xlims[2], by = 1)
xlabels <- seq(from = 0, to = xlims[2], by = 1)
ticksy <- seq(from = 0, to = ylims[2], by = 0.5)
ylabels <- seq(from = 0, to = ylims[2], by = 0.5)
# ticksx <- c(xlims[1], 5.25, 8.944272, xlims[2])
# xlabels <- c(NA, expression(paste(x,"*")), expression(paste(bar(x))), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Draw the polygon for shading the feasible set
# xpoly1 <- seq(from = xlims[1], 10, length.out = 500)
# ypoly1 <- ppf(xpoly1)
# polygon(x = c(0, xpoly1, 0),
#         y = c(0, ypoly1, 0),
#         col=COLA[1], density=NULL, border = NA)

#Draw the graphs
lines(xx1, demand(xx1), col = COLA[5], lwd = graphlinewidth)
#lines(xx1, ppf(xx1, bary = 40, slope = 2), col = COLA[5], lwd = graphlinewidth)
#lines(xx1, ppf(xx1, bary = 10, slope = 0.5), col = COLA[5], lwd = graphlinewidth)

#Label the feasible frontier
# text(16, 3, expression("Feasible Frontier"), cex = labelsize)
# Arrows(16, 3.1, 16, 3.9, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

# contour(x, y, 
#         outer(x, y, uFn),
#         drawlabels = FALSE,
#         col = COLB[3],
#         lwd = graphlinewidth,
#         levels = a, 
#         xaxs="i", 
#         yaxs="i", 
#         add = TRUE)

#Axis labels
mtext(expression(paste("B's payoff (dollars), ", pi^B)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.9, 0.5*ylims[2], expression(paste("Price of transfer, ", p[pi^B])), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Add mrs = mrt at i
# text(16, 16.5, expression(paste(mrs == mrt)), cex = labelsize)
# Arrows(16, 16, 16, 9.1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the indifference curves
text(8.1, 38, expression(u[1]^A), cex = labelsize)
text(9.5, 38, expression(u[2]^A), cex = labelsize)
text(11.3, 38, expression(u[3]^A), cex = labelsize)

#Label the feasible frontiers
text(3, 2.6, expression(paste("Demand for altruistic")), cex = labelsize)
text(3, 2.4, expression(paste("transfers to B")), cex = labelsize)

#Annotate max u point on feasible frontier
#text(5.2, ppf(5) + 0.2, expression(paste(i)), cex = labelsize)

segments(0, 2, 2, 2, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(2, 0, 2, 2, lty = 2, col = "gray", lwd = segmentlinewidth)

segments(0, 1, 4, 1,  lty = 2, col = "gray", lwd = segmentlinewidth)
segments(4, 0, 4, 1,  lty = 2, col = "gray", lwd = segmentlinewidth)

segments(0, 0.5, 8, 0.5, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(8, 0, 8, 0.5, lty = 2, col = "gray", lwd = segmentlinewidth)
#segments(0, ppf(x = 16, bary = 40, slope = 2), 16, ppf(x = 16, bary = 40, slope = 2), lty = 2, col = "gray", lwd = segmentlinewidth)

points(2, 2, pch = 16, col = "black", cex = 1.5)
points(4, 1, pch = 16, col = "black", cex = 1.5)
points(8, 0.5, pch = 16, col = "black", cex = 1.5)



# text(2.3+0.2, ppf(2.3) + 0.2, expression(paste(a)), cex = labelsize)
# segments(2.3, 0, 2.3, ppf(x = 2.3), lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, ppf(x = 2.3), 2.3, ppf(x = 2.3), lty = 2, col = "gray", lwd = segmentlinewidth)
# points(2.3, ppf(x = 2.3), pch = 16, col = "black", cex = 1.5)


# text(7.7+0.2, ppf(7.7) + 0.2, expression(paste(b)), cex = labelsize)
# segments(7.7, 0, 7.7, ppf(x = 7.7), lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, ppf(x = 7.7), 7.7, ppf(x = 7.7), lty = 2, col = "gray", lwd = segmentlinewidth)
# points(7.7, ppf(x = 7.7), pch = 16, col = "black", cex = 1.5)



dev.off()
