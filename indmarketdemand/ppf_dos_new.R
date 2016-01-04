#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "bfh-textbook/indmarketdemand/ppf_dos.pdf", width = 8, height = 8)

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

ppf2 <- function(x, yint = 64){
  sqrt(yint - (x)^2)
}

uFn <- function(x, y, alpha = 0.5){
  (x^alpha)*(y^(1-alpha))
}

bcA <- function(x, w = 8, p = 2) {
  w - p*x
}


xlims <- c(0, 10)
ylims <- c(0, 10)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(3.85, 5.65, 6.4)

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
# ticksx <- seq(from = 0, to = xlims[2], by = 1)
# xlabels <- seq(from = 0, to = xlims[2], by = 1)
ticksy <- c(ylims[1], bcA(x = 4.5, w = 18, p = 2), 5.65, ppf2(x = 7.15), 8, ylims[2])
ylabels <- c(NA, expression(paste(y[sc])), expression(paste(y[d])), expression(paste(y[sp])), expression(paste(bar(y))), NA)
ticksx <- c(xlims[1], 4.5, 5.65, 7.15, 8, xlims[2])
xlabels <- c(NA, expression(paste(x[sc])), expression(paste(x[d])), expression(paste(x[sp])), expression(paste(bar(x))), NA)


axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, 8, length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Draw the polygon for shading the feasible set
xpoly1 <- seq(from = xlims[1], to = 8, length.out = 500)
ypoly1 <- ppf2(xpoly1, yint = 64)
polygon(x = c(xpoly1, rev(xpoly1[1])), y = c(ypoly1, rev(ypoly1)[1]), col=COLA[1], density=NULL, border = NA)


#Draw the graphs
#lines(xx1, ppf(xx1, k = 0.1, maxy = 8, alpha = 2), col = COLA[5], lwd = graphlinewidth)
lines(xx2, ppf2(xx2, yint = 64), col = COLA[5], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 18, p = 2), col = COL[3], lwd = graphlinewidth)

#Label the feasible frontier
text(2.2, 1, expression("Feasible Frontier"), cex = labelsize)
text(2.2, 0.5, expression("(production possibilities frontier)"), cex = labelsize)
Arrows(3.35, 0.95, 7.7, 0.95, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the exchange constraint
text(8.5, 6.5, expression("Exchange"), cex = labelsize)
text(8.5, 6.2, expression("Budget Constraint"), cex = labelsize)
Arrows(8.5, 6, 8.5, 1.25, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


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
text(-0.9, 5, expression(paste("Quantity of oranges, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Annotate max u point on feasibility frontier
text(5.8, 5.8, expression(paste(d)), cex = labelsize)
segments(0, 5.65, 5.65, 5.65, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(5.65, 0, 5.65, 5.65, lty = 2, col = "gray", lwd = segmentlinewidth)
points(5.65, 5.65, pch = 16, col = "black", cex = 1.5)

#Point of tangency between exchange budget and ppf
text(7.35, ppf2(x = 7.15) +.2, expression(s), cex = labelsize)
segments(0, ppf2(x = 7.15), 7.15, ppf2(x = 7.15), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(7.15, 0, 7.15, ppf2(x = 7.15), lty = 2, col = "gray", lwd = segmentlinewidth)
points(7.15, ppf2(x = 7.15), pch = 16, col = "black", cex = 1.5)

#Point of tangency between exchange budget and indiff curve 3
text(4.7, bcA(x = 4.5, w = 18, p = 2) +.2, expression(e), cex = labelsize)
segments(0, bcA(x = 4.5, w = 18, p = 2), 4.5, bcA(x = 4.5, w = 18, p = 2), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(4.5, 0, 4.5, bcA(x = 4.5, w = 18, p = 2), lty = 2, col = "gray", lwd = segmentlinewidth)
points(4.5, bcA(x = 4.5, w = 18, p = 2), pch = 16, col = "black", cex = 1.5)


#Add mrs = mrt at i
text(6.4, 8.25, expression(paste(mrs(x,y) == mrt(x,y))), cex = labelsize)
Arrows(5.65, 8, 5.65, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the indifference curves
text(9.5, 1.25, expression(u[1]^A), cex = labelsize)
text(9.5, 3.05, expression(u[2]^A), cex = labelsize)
text(9.5, 4, expression(u[3]^A), cex = labelsize)

dev.off()
