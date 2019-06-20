library(shape)
pdf(file = "indmarketdemand/demandcd.pdf", width = 8, height = 8)

axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2


COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 7, 4, 4))

uA <- function(x, y, alpha = 0.5) {
  x^(alpha)*y^(1 - alpha)
}

bcA <- function(x, w = 20, p = 2) {
  w - p*x
}


xlims <- c(0, 14)
ylims <- c(0, 14)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(sqrt(2)*sqrt(6), 2*sqrt(6), 6)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

ticksy <- seq(from = 0, to = ylims[2], by = 2)
ylabels <- seq(from = 0, to = ylims[2], by = 2)
ticksx <- seq(from = 0, to = xlims[2], by = 2)
xlabels <- seq(from = 0, to = xlims[2], by = 2)
# ticksy <- c(0, 6, 9, 12, ylims[2])
# ylabels <- c(NA, expression(paste(y==m[1]/p[y])), expression(paste(y == m[2]/p[y])), expression(paste(y == m[3]/p[y])), NA)
# ticksx <- c(0, 6, 9, 12, xlims[2])
# xlabels <- c(NA, expression(paste(x == m[1]/p[x])), expression(paste(x == m[2]/p[x])), expression(paste(x == m[3]/p[x])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, bcA(xx1, w = 12, p = 1), col = COLA[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 12, p = 1.5), col = COLA[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 12, p = 3), col = COLA[3], lwd = graphlinewidth)
#lines(xx1, indiffA1(xx1, uA = 20, rmax = 2.5, xmax = 10), col = COLB[4], lwd = graphlinewidth)


#Label Axes
mtext(expression(paste("Kilograms of coffee, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-2.3, 0.5*ylims[2], expression(paste("Gigabytes of data, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

segments(0, 6, 6, 6, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(2, 0, 2, 6, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(4, 0, 4, 6, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(6, 0, 6, 6, lty = 2, col = "gray" , lwd = segmentlinewidth)

#Label the budget curve functions for the HG, Aisha
text(4.3, .25, expression(bc[1]), cex = labelsize)
text(8.3, .25, expression(bc[2]), cex = labelsize)
text(12.3, .25, expression(bc[3]), cex = labelsize)

#adding iso-welfare functions:

#Label the iso-welfare functions for the HG, Aisha
text(12.2, 1.2, expression(u[1]))
text(12.2, 2.2, expression(u[2]))
text(12.2, 3.15, expression(u[3]))
#text(6.6, 8.3, expression(u[4]^A))

contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLB[3],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE)


points(2, 6, pch = 16, col = "black", cex = 1.5)
text(2.1, 6.5, expression(i[1]))
points(4, 6, pch = 16, col = "black", cex = 1.5)
text(4.1, 6.5, expression(i[2]))
points(6, 6, pch = 16, col = "black", cex = 1.5)
text(6.1, 6.5, expression(i[3]))

Arrows(8.5, 6, 6.5, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(9.7, 6.05, expression(mrs==mrt))


dev.off()
