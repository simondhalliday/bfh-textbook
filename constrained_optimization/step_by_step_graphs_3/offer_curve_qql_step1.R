#Graph Designer: Simon Halliday & Madeleine Wettach '20
#Authors: Bowles, Foley and Halliday
#Title: Microeconomics: Competition, Conflict and Coordination

require(shape)
pdf(file = "constrained_optimization/step_by_step_graphs_3/offer_curve_qql_step1.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 5, 4, 4))

mrsA <- function(x, rmax = 10, xmax = 20) {
  rmax - (rmax/xmax)*x
}


uA <- function(x, y, rmax = 2, xmax = 12) {
  y + rmax*x - (1/2)*(rmax/xmax)*x^2
}

indiffA1 <- function(x, uA = 10, rmax = 2.5, xmax = 10) {
  uA - rmax*x + (1/2)*(rmax/xmax)*(x^2)
}

offerCurve <- function(x, w, rmax = 2, xmax = 12) {
  w - rmax*x + (rmax/xmax)*x^2
}

bcA <- function(x, w = 20, p = 2) {
  w - p*x
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
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
# ticksx <- seq(from = 0, to = xlims[2], by = 2)
# xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(0, 4, 5.5, 7.375 , ylims[2])
ylabels <- c(NA, expression(paste(y[1])), expression(paste(y[2])), expression(paste(y[3])), NA)
ticksx <- c(0, 6, 9, 10.5,  xlims[2])
xlabels <- c(NA, expression(paste(x[1])), expression(paste(x[3])), expression(paste(x[3])), NA)




axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#lines(xx1, bcA(xx1, w = 10, p = 1.5), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 10, p = 1), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 10, p = 0.5), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 10, p = 0.25), col = COLB[3], lwd = graphlinewidth)


#Label the axes
mtext(expression(paste("Quantity of fish, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-1.2, 0.5 * ylims[2], expression(paste("Quanity of money, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the indifference curves
text(11.8, 1.3, expression(u[1]))
text(11.8, 5.1, expression(u[2]))
text(11.8, 7.6, expression(u[3]))

#Label the price lines
text(8, 0.9, expression(paste(pl[1], ", ", p[x] == 0.25)))
text(11.3, 3.75, expression(paste(pl[2], ", ", p[x] == 0.5)))
text(11.3, 6.8, expression(paste(pl[3], ", ", p[x] == 1)))

contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLA[3],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE)

#Annotate points on offer curve mrs = p for each of p = 1, 0.5, 0.25
#Where mrs = 2 - (1/6)*x
points(6, 4, pch = 16, col = "black", cex = 1.5)
points(9, 5.5, pch = 16, col = "black", cex = 1.5)
points(10.5, 7.375, pch = 16, col = "black", cex = 1.5)

dev.off()

