#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Microeconomics: Competition, Conflict and Coordination

library(shape)
pdf(file = "constrained_optimization/feasible_dictator_fair07.pdf", width = 8, height = 8)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 2
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")
Grays <- gray.colors(25, start = 1, end = 0)

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 1, 1))

ppf <- function(x, slope = 1, bary = 10) {
  bary - slope * x
}

uFn <- function(x, y, alpha = 0.7){
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
a <- c(uFn(7, 3) - 0.8, uFn(7, 3), uFn(7, 3) + 1.4) #alpha = 0.4

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

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

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
text(0.5*xlims[2] - 0.1, -0.9, expression(paste("C's payoff (dollars), ", pi^C)), xpd = TRUE, cex = axislabelsize) 
text(-0.9, 0.5*ylims[2], expression(paste("D's payoff (dollars), ", pi^D)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Add mrs = mrt at i
text(7, 4.5, expression(paste(mrs == mrt)), cex = labelsize)
Arrows(7, 4.3, 7, 3.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the indifference curves
text(10.5, 0.35, expression(u[1]^C), cex = labelsize)
text(10.5, 1.5, expression(u[2]^C), cex = labelsize)
text(10.5, 2.9, expression(u[3]^C), cex = labelsize)

#Annotate max u point on feasible frontier
text(7.3, ppf(7) + 0.2, expression(paste(b*minute)), cex = labelsize)
segments(7, 0, 7, ppf(x = 7), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, ppf(x = 7), 7, ppf(x = 7), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(7, ppf(x = 7), pch = 16, col = "black", cex = 1.5)

text(4.2 + 0.3, ppf(4.2) + 0.2, expression(paste(a*minute)), cex = labelsize)
segments(4.2, 0, 4.2, ppf(x = 4.2), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, ppf(x = 4.2), 4.2, ppf(x = 4.2), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(4.2, ppf(x = 4.2), pch = 16, col = "black", cex = 1.5)


text(9.03 + 0.2, ppf(9.03) + 0.2, expression(paste(c*minute)), cex = labelsize)
segments(9.03, 0, 9.03, ppf(x = 9.03), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, ppf(x = 9.03), 9.03, ppf(x = 9.03), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(9.03, ppf(x = 9.03), pch = 16, col = "black", cex = 1.5)


# text(2, ppf(2) - 1, expression(paste("Feasible")), cex = labelsize)
# text(2, ppf(2) - 1.5, expression(paste("frontier")), cex = labelsize)


dev.off()
