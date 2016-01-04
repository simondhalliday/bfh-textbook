#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "bfh-textbook/constrained_optimization/indiff_map.pdf", width = 8, height = 6)

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

#Change this to make it log of l 

# ppf <- function(l, k = 0.5) {
#   k * (1/l)
# }

ppf <- function(x, k = 0.1, maxy = 8, alpha = 2) {
  maxy - k*x^alpha 
}

uFn <- function(x, y, alpha = 0.5){
  (x^alpha)*(y^(1-alpha))
}

xlims <- c(0, 10)
ylims <- c(0, 10)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(3, 5, 7)

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


ticksy <- seq(from = 0, to = ylims[2], by = 1)
ylabels <- seq(from = 0, to = ylims[2], by = 1)
ticksx <- seq(from = 0, to = xlims[2], by = 1)
xlabels <- seq(from = 0, to = xlims[2], by = 1)
# ticksy <- c(ylims[1], 5.25, 8, ylims[2])
# ylabels <- c(NA, expression(paste(y,"*")), expression(paste(bar(y))), NA)
# ticksx <- c(xlims[1], 5.25, 8.944272, xlims[2])
# xlabels <- c(NA, expression(paste(x,"*")), expression(paste(bar(x))), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)

#Draw the polygon for shading the feasible set
#xpoly1 <- seq(from = xlims[1], to = xlims[2], length.out = 500)
#ypoly1 <- ppf(xpoly1, k = 0.1, maxy = 8, alpha = 2)
#polygon(x = c(xpoly1, rev(xpoly1[1])), y = c(ypoly1, rev(ypoly1)[1]), col=COLA[1], density=NULL, border = NA)

#Draw the graphs
#lines(xx1, ppf(xx1, k = 0.1, maxy = 8, alpha = 2), col = COLA[5], lwd = graphlinewidth)
#lines(xx2, fishProd(xx2, k = 2), col = COLB[3], lwd = graphlinewidth)
#lines(xx3, feasibleLabor(xx3, time = 10), col = COL[3], lwd = graphlinewidth)
#lines(xx4, manufactureProd(xx4, k = 0.1, alpha = 2), col = COLB[4], lwd = graphlinewidth)

#Label the feasible frontier
# text(3.2, 1, expression("Feasible Frontier"), cex = labelsize)
# text(3.2, 0.5, expression("(production possibilities frontier)"), cex = labelsize)
# Arrows(4.35, 0.95, 8.1, 0.95, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

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
mtext(expression(paste("Quantity of apples, ", x)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.8, 5, expression(paste("Quantity of oranges, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Annotate max u point on feasibility frontier
text(5.4, 5.4, expression(paste(i)), cex = labelsize)
segments(0, 5.25, 5.25, 5.25, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(5.25, 0, 5.25, 5.25, lty = 2, col = "gray", lwd = segmentlinewidth)
points(5.25, 5.25, pch = 16, col = "black", cex = 1.5)

#Add mrs = mrt at i
# text(5.25, 7.25, expression(paste(mrs(x,y) == mrt(x,y))), cex = labelsize)
# Arrows(5.25, 7, 5.25, 5.75, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the indifference curves
text(9.5, 1.35, expression(u[1]^A), cex = labelsize)
text(9.5, 3.15, expression(u[2]^A), cex = labelsize)
text(9.5, 5.85, expression(u[3]^A), cex = labelsize)

dev.off()
