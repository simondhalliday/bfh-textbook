require(shape)
pdf(file = "indmarketdemand/effort_bad.pdf", width = 7, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 5, 2, 2))

mrsA <- function(x, rmax = 10, xmax = 20) {
  rmax - (rmax/xmax)*x
}


uA <- function(x, y, slope = -1) {
  y + slope*x^2
}

indiffA1 <- function(x, uA = 10, slope = -1) {
  uA + slope*x^2
}


bcA <- function(x, m = 180, p = 2) {
  m - p*x
}


xlims <- c(0, 12)
ylims <- c(0, 220)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(60, 90, 120)

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
ticksy <- c(0, a[1], a[2], a[3], ylims[2])
ylabels <- c(NA, expression(paste(y[1] == u[1])), expression(paste(y[2] == u[2])), expression(paste(y[3] == u[3])), NA)
ticksx <- c(0, xlims[2])
xlabels <- c(NA, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#lines(xx1, bcA(xx1, m = 140, p = 7), col = COLB[3], lwd = graphlinewidth)
#lines(xx1, indiffA1(xx1, uA = 20, rmax = 2.5, xmax = 10), col = COLB[4], lwd = graphlinewidth)



mtext(expression(paste("Level of effort, ", e)), side=1, line = 2.5, cex = axislabelsize)
text(-1.8, 0.5*ylims[2], expression(paste("Quantity of money, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
#text(7.3, 3, expression("Pareto Efficient"))
#text(7.3, 2.5, expression("Curve"))
#Arrows(7.3, 3.5, 7.3, 6.1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the iso-welfare functions for the HG, Aisha
text(8.4, 200, expression(u[3]))
text(10, 200, expression(u[2]))
text(11.4, 200, expression(u[1]))
#text(6.6, 8.3, expression(u[4]^A))

#Label the indifference curves for the HG, Betty
# text(7.6, 17, expression(u[1]^B))
# text(6.75, 17, expression(u[2]^B))
# text(6, 17, expression(u[3]^B))
#text(3.4, 6.9, expression(v[4]^B))

#Label mrs function
#text(10, 8, expression(paste(mrs(x,y) == r[max] - frac(r[max], x[max])*x)))
#Arrows(10, 7.5, 10, 5.6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label satiation
#text(20, 4, expression(paste(x[max] == "Point")))
#text(20, 3, expression(paste("of Satiation")))
#Arrows(20, 2.5, 20, 0.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label highest willingness to pay
# text(5, 10.5, expression(paste(r[max] == "Maximum")))
# text(5, 9.5, expression(paste("Willingness to Pay")))
# Arrows(3, 10, 0.5, 10, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLA[3],
        #xlab = expression(paste("A's Apples, ", x)),
        #ylab = expression(paste("A's Oranges, ", y)),
        #cex.lab = axislabelsize,
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE)

# segments(0, 98, 6, 98, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(6, 0, 6, 98, lty = 2, col = "gray" , lwd = segmentlinewidth)
# points(6, 98, pch = 16, col = "black", cex = 1.5)
# 
# text(6.1, 6.5, expression(i))

# 
# points(1.1, 10.9, pch = 16, col = "black", cex = 1.5)
# points(10.9, 1.1, pch = 16, col = "black", cex = 1.5)
# text(1.2, 11.4, expression(a))
# text(11, 1.6, expression(b))

#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
# text(2, 3, expression(paste("Budget constraint, ", bc[1])))
# Arrows(3.5, 3, 8.5, 3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#text(10, 13, expression(paste("Budget constraint, ", bc[1])))
# Arrows(10, 12.7, 10, 2.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(3, 200, expression(paste(u == y - e^2)))
text(3, 185, expression(paste(mrs(e,y) == frac(u[e],u[y]), phantom() == -2*e)))

#Frame around u function
segments(1, 170, 1, 210, lty = 1, col = "black", lwd = segmentlinewidth)
segments(5, 170, 5, 210, lty = 1, col = "black", lwd = segmentlinewidth)
segments(1, 170, 5, 170, lty = 1, col = "black", lwd = segmentlinewidth)
segments(1, 210, 5, 210, lty = 1, col = "black", lwd = graphlinewidth)


dev.off()
