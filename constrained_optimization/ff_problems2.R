#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Problems for BFH:CCC and CORE: The Economy

library(shape)
pdf(file = "constrained_optimization/ff_problems2.pdf", width = 8, height = 8)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 4, 4))

#Change this to make it log of l 

# ppf <- function(l, k = 0.5) {
#   k * (1/l)
# }

ppf <- function(x) {
  (144 - 0.25*x^2)^(0.5)
}

uB <- function(x, y, rmax = 0.8, xmax = 24) {
  y + rmax*x - (1/2)*(rmax/xmax)*x^2
}

b <- c(13, 17.7, 30) 

uA <- function(x, y, rmax = 1.4, xmax = 24) {
  y + rmax*x - (1/2)*(rmax/xmax)*x^2
}

a <- c(uA(x = 24, y = 3.4), uA(x = 16, y = 6.6), uA(x = 16, y = 9)) 

ylims <- c(0, 12.8)
xlims <- c(0, 24)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 



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




#ticksy <- seq(from = 0, to = ylims[2], by = 1)
#ylabels <- seq(from = 0, to = ylims[2], by = 1)
# ticksx <- seq(from = 0, to = xlims[2], by = 2)
# xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(ylims[1], 5.25, 6.6, 9, 9.9, 12, ylims[2])
ylabels <- c(ylims[1], 5.25, 6.5, 9, 10, 12, NA)
ticksx <- c(xlims[1], 4, 8, 12, 13.5, 16, 20, xlims[2])
xlabels <- c(xlims[1], 4, 8, 12, 13.5, 16, 20, xlims[2])

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 501
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)

#Draw the polygon for shading the feasible set
#xpoly1 <- seq(from = xlims[1], to = 16, length.out = 500)
#ypoly1 <- ppf(xpoly1, k = 1.058868, maxx = 17)
#polygon(x = c(xpoly1, xpoly1[1]), y = c(ypoly1, rev(ypoly1)[1]), col=COLA[1], density=NULL, border = NA)

#Draw the graphs
lines(xx1, ppf(xx1), col = COLA[5], lwd = graphlinewidth)
#lines(xx2, fishProd(xx2, k = 2), col = COLB[3], lwd = graphlinewidth)
#lines(xx3, feasibleLabor(xx3, time = 10), col = COL[3], lwd = graphlinewidth)
#lines(xx4, manufactureProd(xx4, k = 0.1, alpha = 2), col = COLB[4], lwd = graphlinewidth)

#Label the feasible frontier
# text(10.5, 0.53, expression("Feasible Frontier"), cex = labelsize)
# Arrows(12.5, 0.53, 14.9, 0.53, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

# contour(x, y,
#         outer(x, y, uA),
#         drawlabels = FALSE,
#         col = COLB[3],
#         lwd = graphlinewidth,
#         levels = a,
#         xaxs="i",
#         yaxs="i",
#         add = TRUE
#         )

contour(x, y, 
        outer(x, y, uB),
        drawlabels = FALSE,
        col = COL[3],
        lwd = graphlinewidth,
        lty = 2, 
        levels = b, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE)

#Axis labels
mtext(expression(paste("Anna's hours of leisure, ", x)), side = 1, line = 2.5, cex = axislabelsize)
text(-2.35, 0.5*ylims[2], expression(paste("Crates of cookies, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Add mrs = mrt at i
# text(8, 2.8, expression(paste(mrs(x,y) == mrt(x,y))), cex = labelsize)
# Arrows(8, 2.75, 8, 2.45, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the indifference curves
# text(17, 1, expression(u[1]^A), cex = labelsize)
# text(17, 1.55, expression(u[2]^A), cex = labelsize)
# text(17, 2.25, expression(u[3]^A), cex = labelsize)

#Annotate max u point on feasibility frontier
# text(8.4, ppf(8) + 0.1, expression(paste(i)), cex = labelsize)
# segments(8, 0, 8, ppf(x = 8), lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, ppf(x = 8), 8, ppf(x = 8), lty = 2, col = "gray", lwd = segmentlinewidth)
# points(8, ppf(x = 8), pch = 16, col = "black", cex = 1.5)

# text(2.4, ppf(2) + 0.1, expression(paste(a)), cex = labelsize)
# segments(2, 0, 2, ppf(x = 2), lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, ppf(x = 2), 2, ppf(x = 2), lty = 2, col = "gray", lwd = segmentlinewidth)
# points(2, ppf(x = 2), pch = 16, col = "black", cex = 1.5)
# 

# text(16 + 0.3, ppf(16) + 0.3, expression(paste(b)), cex = labelsize)
# segments(16, 0, 16, 9, lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, 9, 16, 9, lty = 2, col = "gray", lwd = segmentlinewidth)
# points(16, ppf(x = 16), pch = 16, col = "black", cex = 1.5)
# 
# segments(0, 5.25, 20, 5.25, lty = 2, col = "gray", lwd = segmentlinewidth)
# points(16, 5.25, pch = 16, col = "black", cex = 1.5)
# text(16 + 0.3, 5.25 + 0.3, expression(paste(c)), cex = labelsize)
# 
# segments(20, 0, 20, ppf(20), lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, 6.6, 20, 6.6, lty = 2, col = "gray", lwd = segmentlinewidth)
# points(20, 5.25, pch = 16, col = "black", cex = 1.5)
# text(20 + 0.3, 5.25 + 0.3, expression(paste(d)), cex = labelsize)
# 
# points(20, 6.6, pch = 16, col = "black", cex = 1.5)
# text(20 + 0.3, 6.6 + 0.3, expression(paste(e)), cex = labelsize)
# 
# points(16, 6.6, pch = 16, col = "black", cex = 1.5)
# text(16 + 0.3, 6.6 + 0.3, expression(paste(f)), cex = labelsize)
# 

segments(13.5, 0, 13.5, 5.25, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(13.5, 0, 13.5, 9.9, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, 9.9, 13.5, 9.9, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, 5.25, 13.5, 5.25, lty = 2, col = "gray", lwd = segmentlinewidth)

points(13.5, 5.25, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(13.5 - 0.3, 5.25 - 0.3, expression(paste(g)), cex = labelsize, xpd = TRUE)

points(13.5, 9.9, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(13.5 - 0.3, 9.9 - 0.3, expression(paste(h)), cex = labelsize, xpd = TRUE)

# points(24, 3.4, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
# text(24 + 0.3, 3 + 0.3, expression(paste(z)), cex = labelsize, xpd = TRUE)

# text(8.6, 8.4, expression(paste("Anna's")), cex = labelsize)
# text(8.6, 8, expression(paste("Reservation")), cex = labelsize)
# text(8.6, 7.6, expression(paste("Indifference")), cex = labelsize)
# text(8.6, 7.2, expression(paste("Curve")), cex = labelsize)
#Arrows(8.6, 3.6, 8.6, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


text(5.5, 8.2, expression(paste("Anna's")), cex = labelsize)
text(5.5, 7.8, expression(paste("Survival")), cex = labelsize)
text(5.5, 7.4, expression(paste("Constraint")), cex = labelsize)

# text(24, 2.76, expression(ic[1]), cex = labelsize, xpd = TRUE)
# text(24, 3.75, expression(ic[2]), cex = labelsize, xpd = TRUE)
# text(24, 5.2, expression(ic[3]), cex = labelsize, xpd = TRUE)
# text(24, 7, expression(ic[4]), cex = labelsize, xpd = TRUE)

text(24, 7.8, expression(ic[2]), cex = labelsize, xpd = TRUE)


dev.off()
