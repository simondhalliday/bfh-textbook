#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "/Users/rileyboeth/Library/Mobile Documents/com~apple~CloudDocs/bfh textbook - backup/constrained_optimization/figure3_11.pdf", width = 8, height = 8)

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

#Change to feasibility frontier equation

ppf <- function(x, y) {
  100-((x^2)/2)
}

#change alpha to .4

uFn <- function(x, y, alpha = 0.4){
  (x^alpha)*(y^(1-alpha))
}

#Add limits on axes and levels of utility for each indifference curve

ylims <- c(0, 130)
xlims <- c(0, 20)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

a <- c(27, 29.045, 31) #alpha = 0.4

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

#changed x and y limits and ticks 
 
# ticksy <- seq(from = 0, to = ylims[2], by = 1)
# ylabels <- seq(from = 0, to = ylims[2], by = 1)
ticksx <- seq(from = 0, to = xlims[2], by = 1)
xlabels <- seq(from = 0, to = xlims[2], by = 1)
ticksy <- seq(from = 0, to = ylims[2], by = 10)
ylabels <- seq(from = 0, to = ylims[2], by = 10)
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
#xpoly1 <- seq(from = xlims[1], to = 16, length.out = 500)
#ypoly1 <- ppf(xpoly1, k = 1.058868, maxx = 17)
#polygon(x = c(xpoly1, xpoly1[1]), y = c(ypoly1, rev(ypoly1)[1]), col=COLA[1], density=NULL, border = NA)

#Draw the graphs
lines(xx1, ppf(xx1, y), col = COLA[5], lwd = graphlinewidth)
#lines(xx2, fishProd(xx2, k = 2), col = COLB[3], lwd = graphlinewidth)
#lines(xx3, feasibleLabor(xx3, time = 10), col = COL[3], lwd = graphlinewidth)
#lines(xx4, manufactureProd(xx4, k = 0.1, alpha = 2), col = COLB[4], lwd = graphlinewidth)

#Label the feasible frontier
text(15, 15, expression("Initial"), cex = labelsize)
text(15, 11, expression("Feasiblility"), cex = labelsize)
text(15, 7.5, expression("Frontier"), cex = labelsize)
Arrows(12.5, 0.53, 14.9, 0.53, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

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
mtext(expression(paste("Environmental Quality, ", x)), side = 1, line = 2.5, cex = axislabelsize)
text(-1.8, 0.5*ylims[2], expression(paste("Goods,Millions, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the indifference curves generally
text(7,120, expression("Policy-Maker's"), cex = labelsize)
text(7,116, expression("Indifference Curves"), cex = labelsize)

#Label the indifference curves and place labels properly
text(19, 36, expression(u[1]), cex = labelsize)
text(19, 40.5, expression(u[2]), cex = labelsize)
text(19, 45, expression(u[3]), cex = labelsize)

#Annotate max u point on feasibility frontier
text(7.48, ppf(7)+1, expression(paste("e*")), cex = labelsize)
segments(7, 0, 7, ppf(x = 7), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, ppf(x = 7), 7, ppf(x = 7), lty = 2, col = "gray", lwd = segmentlinewidth)
points(7, ppf(x = 7), pch = 16, col = "black", cex = 1.5)




dev.off()