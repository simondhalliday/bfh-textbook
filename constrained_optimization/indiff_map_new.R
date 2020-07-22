#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "constrained_optimization/indiff_map_new.pdf", width = 8, height = 6)

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

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 1, 1))

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
a <- c(2, 4, 6)

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
# ticksy <- c(ylims[1], 5, 8, ylims[2])
# ylabels <- c(NA, expression(paste(y,"*")), expression(paste(bar(y))), NA)
# ticksx <- c(xlims[1], 5, 8.944272, xlims[2])
# xlabels <- c(NA, expression(paste(x,"*")), expression(paste(bar(x))), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

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
#mtext(expression(paste("Kilograms of coffee, ", x)), side = 1, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], -1.2, expression(paste("Kilograms of coffee, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-0.8, 5, expression(paste("Gigabytes of data, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Segments before points
segments(0, 4, 4, 4, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(4, 0, 4, 4, lty = 2, col = "gray", lwd = segmentlinewidth)

segments(0, 8, 2, 8, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(2, 0, 2, 8, lty = 2, col = "gray", lwd = segmentlinewidth)

segments(0, 2, 8, 2, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(8, 0, 8, 2, lty = 2, col = "gray", lwd = segmentlinewidth)

segments(0, 5, 2, 5, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(2, 0, 2, 5, lty = 2, col = "gray", lwd = segmentlinewidth)

segments(0, 9, 2, 9, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(2, 0, 2, 9, lty = 2, col = "gray", lwd = segmentlinewidth)


#Annotate points (4,4),(2,8),(8,2) on feasibility frontier
text(4.2, 4.2, expression(paste(b)), cex = labelsize)
points(4, 4, pch = 16, col = "black", cex = 1.5)

text(2.2, 8.2, expression(paste(a)), cex = labelsize)
points(2, 8, pch = 16, col = "black", cex = 1.5)

text(8.2, 2.2, expression(paste(c)), cex = labelsize)
points(8, 2, pch = 16, col = "black", cex = 1.5)


#Annotate points between the indifference curves along x = 2
text(2.2, 5.2, expression(paste(d)), cex = labelsize)
points(2, 5, pch = 16, col = "black", cex = 1.5)

text(2.2, 9.2, expression(paste(e)), cex = labelsize)
points(2, 9, pch = 16, col = "black", cex = 1.5)





#Add mrs = mrt at i
# text(5.25, 7.25, expression(paste(mrs(x,y) == mrt(x,y))), cex = labelsize)
# Arrows(5.25, 7, 5.25, 5.75, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the indifference curves
text(9.7, .8, expression(u[3]^A), cex = labelsize)
text(9.7, 2.1, expression(u[4]^A), cex = labelsize)
text(9.7, 4.1, expression(u[5]^A), cex = labelsize)

dev.off()
