#Graph Designer: Simon Halliday, Scott Cohn, Harriet Brookes-Gray
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(pBrackets)

pdf(file = "credit/credit_comparison.pdf", width = 8, height = 6)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4.5, 6, 1, 2))

brfFn <- function(delta, q = 1) {
  .5 + (delta / (2 * q)) 
}

PCFn <- function(delta, q = 1) {
  delta/q
}

isoreturnFn <- function(delta, pi=0.125) {
  1 - (pi)/delta
}


yFn <- function(d1, f1, q = 1){
  q*f1*(1 - f1) - d1*(1 - f1)
}

profitFn <- function(d1, q = 1){
  d1/2 - (1/(2*q))*(d1)^2
}

ylow <- function(delta, q = 1, ybar = 0.0625){
  (-sqrt(delta^2 - 2*delta*q + q^2 - 4*q*ybar) + delta + q)/(2*q)
}

yhigh <- function(delta, q = 1, ybar = 0.0625){
  (sqrt(delta^2 - 2*delta*q + q^2 - 4*q*ybar) + delta + q)/(2*q)
}

xlims <- c(0, 1)
ylims <- c(0, 1.05)

npts <- 501 
d1 <- seq(xlims[1], xlims[2], length.out = npts)
f1 <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(0.0625)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs = "i", 
     yaxs = "i"
)


ticksy <- c(ylims[1], 0.5, brfFn(delta = 0.5), ylims[2])
ylabels <- c(NA, expression(paste(f^{C} == frac(1,2))),expression(paste(f^{N} == frac(3,4))), NA)
ticksx <- c(xlims[1], 0.5, 0.75, xlims[2])
xlabels <- c(NA, NA, NA, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

text(0.5, -0.07, expression(paste(delta^{N} == frac(q,2))), xpd = TRUE, cex = labelsize) 
text(0.59, -0.06, expression(paste(phantom() == delta^{C})), xpd = TRUE, cex = labelsize) 
text(0.75, -0.07, expression(paste(delta[g])), xpd = TRUE, cex = labelsize) 


npts <- 503 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, 1, length.out = npts)
xx3 <- seq(0.01, xlims[2], length.out = npts)

#xpoly1 <- seq(from = 0.168, to = 0.5, length.out = 500)
#ypoly1 <- ylow(xpoly1)
#ypoly2 <- isoreturnFn(xpoly1)
#polygon(x = c(xpoly1, rev(xpoly1)), y = c(ypoly1, rev(ypoly2)), col = COL[4], density = NULL, border = NA)


#Draw the graphs
lines(xx1, brfFn(xx1), col = COLA[4], lwd = graphlinewidth)
lines(xx1, PCFn(xx1), col = COLA[2], lwd = graphlinewidth)
lines(xx1, isoreturnFn(xx1), col = COLB[4], lwd = graphlinewidth)
lines(xx1, isoreturnFn(xx1, pi = 0.25), col = COLB[4], lwd = graphlinewidth)



#Axis labels
mtext(expression(paste("Interest factor, ", delta)), side = 1, line = 3.4, cex = axislabelsize)
text(-0.15, 0.5*(ylims[2]), expression(paste("Probability of failure (risk), ", f)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#contour(d1, f1,
        # outer(d1, f1, yFn),
        # drawlabels = FALSE,
        # col = COLA[3],
        # lwd = graphlinewidth,
        # levels = a,
        # xaxs = "i",
        # yaxs = "i",
        # add = TRUE)


segments(0.5, 0, 0.5, brfFn(delta = 0.5), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, 0.5, 0.5, 0.5, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, brfFn(delta = 0.5), 0.75, brfFn(delta = 0.5), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, 0.5, 0.5, 0.5, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0.75, 0, 0.75, PCFn(delta = 0.75), lty = 2, col = grays[20] , lwd = segmentlinewidth)

#Annotate points (4,4),(2,8),(8,2) on feasibility frontier
text(0.5 + 0.015, isoreturnFn(0.5) + 0.04, expression(paste(n)), cex = labelsize)
points(0.5, isoreturnFn(0.5), pch = 16, col = "black", cex = 1.5)
points(0.5, 0.5, pch = 16, col = "black", cex = 1.5)
text(0.5 + 0.019, 0.5 - 0.03, expression(paste(c)), cex = labelsize)


points(0.75, PCFn(0.75), pch = 16, col = "black", cex = 1.5)
text(0.75 + 0.019, PCFn(0.75) - 0.03, expression(paste(g)), cex = labelsize)


#text(0.375 + 0.02, 0.6 + 0.03, expression(paste(b)), cex = labelsize)
#points(0.375, 0.6, pch = 16, col = "black", cex = 1.5)

#text(0.2, 0.87, expression(paste(y == y^{N})), cex = labelsize)

text(0.62, 1.02, expression(paste("A's best-response function (ICC)")), cex = labelsize, xpd = TRUE)
text(0.62, 0.92, expression(paste(f == frac(1,2) + frac(delta, 2*q))), cex = labelsize)

text(0.92, 0.4, expression(paste("A's participation")), cex = labelsize, xpd = TRUE)
text(0.92, 0.34, expression(paste("constraint (PC)")), cex = labelsize, xpd = TRUE)
text(0.92, 0.25, expression(paste(f == frac(delta, q))), cex = labelsize, xpd = TRUE)
Arrows(0.80, 0.44, 0.80, 0.77, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#text(0.87, 0.6, expression(paste("P's isoprofit curve")), cex = labelsize, xpd = TRUE)
text(0.96, 0.91, expression(paste(hat(pi)^{N})), cex = labelsize, xpd = TRUE)
text(0.96, 0.78, expression(paste(hat(pi)^{C})), cex = labelsize, xpd = TRUE)
#Arrows(0.87, 0.63, 0.87, 0.82, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#text(0.3, 0.13, expression(paste("Pareto-improving")), cex = labelsize)
#text(0.3, 0.07, expression(paste("lens")), cex = labelsize)
#Arrows(0.3, 0.18, 0.3, 0.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

# brackets(x1 = 0.51, y1 = 0.74,
#          x2 = 0.51, y2 = 0.52, 
#          ticks = 0.5, curvature = 0.5, type = 1,
#          col = "black", lwd = 2, lty = 1, h = 0.02, xpd = TRUE)
# 
# text(0.575, 0.64, expression(paste("Rent")), cex = labelsize, xpd = TRUE)

brackets(x1 = 0.74, y1 = 0.4,
         x2 = 0.51, y2 = 0.4,
         ticks = 0.5, curvature = 0.5, type = 1,
         col = "black", lwd = 2, lty = 1, h = 0.02, xpd = TRUE)

text(0.625, 0.35, expression(paste("Rent")), cex = labelsize, xpd = TRUE)
text(0.625, 0.28, expression(phantom() == delta[g] - delta^N), cex = labelsize, xpd = TRUE)




dev.off()
