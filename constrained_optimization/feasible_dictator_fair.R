#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Microeconomics: Competition, Conflict and Coordination

library(shape)
pdf(file = "constrained_optimization/feasible_dictator_fair.pdf", width = 8, height = 8)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 4, 4))

ppf <- function(x, slope = 1, bary = 10) {
  bary - slope * x
}

uFn <- function(x, y, alpha = 0.5){
  ((x)^alpha)*(y^(1-alpha))
}

indiffA <- function(x, ubar = uFn(8,2) - 1, alpha = 0.3) {
  (ubar/ ((x)^alpha))^(1/ (1 - alpha))
}

ylims <- c(0, 11)
xlims <- c(0, 11)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(uFn(5, 5) - 0.8, uFn(5, 5), uFn(5, 5) + 1.4) #alpha = 0.4

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
ticksy <- seq(from = 0, to = ylims[2], by = 1)
ylabels <- seq(from = 0, to = ylims[2], by = 1)
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
xpoly1 <- seq(from = xlims[1], 10, length.out = 500)
ypoly1 <- ppf(xpoly1)
polygon(x = c(0, xpoly1, 0),
        y = c(0, ypoly1, 0),
        col=COLA[1], density=NULL, border = NA)

#Draw the graphs
lines(xx1, ppf(xx1), col = COLA[5], lwd = graphlinewidth)

#Label the feasible frontier
text(16, 3, expression("Feasible Frontier"), cex = labelsize)
Arrows(16, 3.1, 16, 3.9, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

contour(x, y, 
        outer(x, y, uFn),
        drawlabels = FALSE,
        col = COLB[3],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE)

#Axis labels
mtext(expression(paste("A's payoff (dollars), ", pi^A)), side = 1, line = 2.5, cex = axislabelsize)
text(-1, 0.5*ylims[2], expression(paste("B's payoff (dollars), ", pi^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Add mrs = mrt at i
text(5, 6.5, expression(paste(mrs == mrt)), cex = labelsize)
Arrows(5, 6.3, 5, 5.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the indifference curves
text(10.5, 1.4, expression(u[1]^A), cex = labelsize)
text(10.5, 2.1, expression(u[2]^A), cex = labelsize)
text(10.5, 3.6, expression(u[3]^A), cex = labelsize)

#Annotate max u point on feasible frontier
text(5.2, ppf(5) + 0.2, expression(paste(i)), cex = labelsize)
segments(5, 0, 5, ppf(x = 5), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, ppf(x = 5), 5, ppf(x = 5), lty = 2, col = "gray", lwd = segmentlinewidth)
points(5, ppf(x = 5), pch = 16, col = "black", cex = 1.5)

text(2.3+0.2, ppf(2.3) + 0.2, expression(paste(a)), cex = labelsize)
segments(2.3, 0, 2.3, ppf(x = 2.3), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, ppf(x = 2.3), 2.3, ppf(x = 2.3), lty = 2, col = "gray", lwd = segmentlinewidth)
points(2.3, ppf(x = 2.3), pch = 16, col = "black", cex = 1.5)


text(7.7+0.2, ppf(7.7) + 0.2, expression(paste(b)), cex = labelsize)
segments(7.7, 0, 7.7, ppf(x = 7.7), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, ppf(x = 7.7), 7.7, ppf(x = 7.7), lty = 2, col = "gray", lwd = segmentlinewidth)
points(7.7, ppf(x = 7.7), pch = 16, col = "black", cex = 1.5)



dev.off()
