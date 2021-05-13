#Graph Designer: Simon Halliday
#Authors: Bowles and Halliday
#Title: Microeconomics: Competition, Conflict and Coordination

library(shape)
pdf(file = "constrained_optimization/feasible_production_learning.pdf", width = 8, height = 8)

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
grays <- gray.colors(25, start = 1, end = 0)


#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 8, 1, 1))

prodFn <- function(x, slope = 1/64) {
  4 - slope *(16 - x)^2
}

ppf <- function(x, slope = 1/64, bary = 4) {
  bary - slope *x^2
}

uFn <- function(x, y, alpha = 0.4){
  (x^alpha)*(y^(1-alpha))
}

indiffA <- function(x, ubar = uFn(8,3) - 1, alpha = 0.4) {
  (ubar/ (x^alpha))^(1/ (1 - alpha))
}

ylims <- c(0, 4.2)
xlims <- c(0, 16)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(uFn(8, 3) - 1, uFn(8, 3), uFn(8, 3) + 1.4) #alpha = 0.4

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


ticksx <- seq(from = 0, to = xlims[2], by = 2)
xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(ylims[1], 1, prodFn(4), 3, 3.75, 4, ylims[2])
ylabels <- c(NA, 1, expression(paste(y[e*minute] == 1.75)), expression(paste(y[i*minute] == 3)), expression(paste(y[g*minute] == 3.75)), 4, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], 16, length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Draw the polygon for shading the feasible set
xpoly1 <- seq(from = xlims[1], to = 16, length.out = 500)
ypoly1 <- prodFn(xpoly1)
polygon(x = c(xpoly1, rev(xpoly1)[1]), y = c(ypoly1, ypoly1[1]), 
        col=COLA[1], density=NULL, border = NA)

polygon(x = c(16, 18, 18, 16), y = c(4, 4, 0, 0), 
         col=COLA[1], density=NULL, border = NA)

#Draw the graphs
lines(xx1, prodFn(xx1), col = COLA[5], lwd = graphlinewidth)
segments(16, 4, 18, 4, lty = 1, col = COLA[5], lwd = graphlinewidth)

#Label the feasible frontier
text(6.3, 3.6, expression("Studying to Learning"), cex = labelsize)
text(6.3, 3.4, expression("production function"), cex = labelsize)
text(6.3, 3.2, expression(paste(y == f(h))), cex = labelsize)
#Arrows(12.5, 0.53, 14.5, 0.53, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#Axis labels
#mtext(expression(paste("Studying (hours), ", h)), side = 1, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], -0.38, expression(paste("Studying (hours), ", h)), xpd = TRUE, cex = axislabelsize) 
text(-3.5, 0.5*ylims[2], expression(paste("Learning, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Add mrs = mrt at i
# text(8.6, 3.6, expression(paste(mrs(x,y) == mrt(x,y))), cex = labelsize)
# Arrows(8, 3.5, 8, 3.15, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the indifference curves
# text(17, 1, expression(u[1]^A), cex = labelsize)
# text(17, 1.65, expression(u[2]^A), cex = labelsize)
# text(17, 2.7, expression(u[3]^A), cex = labelsize)

#Annotate max u point on feasibility frontier
text(8 + 0.4, prodFn(8) - 0.1, expression(paste(i*minute)), cex = labelsize)
segments(8, 0, 8, prodFn(x = 8), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, prodFn(x = 8), 8, prodFn(x = 8), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(8, prodFn(x = 8), pch = 16, col = "black", cex = 1.5)
# 
text(4 + 0.4, prodFn(4) - 0.1, expression(paste(e*minute)), cex = labelsize)
segments(4, 0, 4, prodFn(x = 4), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, prodFn(x = 4), 4, prodFn(x = 4), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(4, prodFn(x = 4), pch = 16, col = "black", cex = 1.5)
# 
# 
text(12 + 0.4, prodFn(12) - 0.1, expression(paste(g*minute)), cex = labelsize)
segments(12, 0, 12, prodFn(12), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, prodFn(12), 12, prodFn(x = 12), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(12, prodFn(12), pch = 16, col = "black", cex = 1.5)



dev.off()
