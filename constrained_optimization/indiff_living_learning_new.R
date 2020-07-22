#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Microeconomics: Competition, Conflict and Coordination

library(shape)
pdf(file = "constrained_optimization/indiff_living_learning_new.pdf", width = 8, height = 8)

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

ppf <- function(x, slope = 1/64, bary = 4) {
  bary - slope *x^2
}

uFn <- function(x, y, alpha = 0.4){
  (x^alpha)*(y^(1-alpha))
}

indiffA <- function(x, ubar = uFn(8,3), alpha = 0.4) {
  (ubar/ (x^alpha))^(1/ (1 - alpha))
}

ylims <- c(0, 5)
xlims <- c(0, 18)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(uFn(2, 3), uFn(8, 3), uFn(14, 3)) #alpha = 0.4

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
ticksx <- seq(from = 0, to = xlims[2], by = 2)
xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(ylims[1], 1, 2, 3, 4, ylims[2])
ylabels <- c(NA, 1, 2, 3, 4, NA)
# ticksx <- c(xlims[1], 5.25, 8.944272, xlims[2])
# xlabels <- c(NA, expression(paste(x,"*")), expression(paste(bar(x))), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Draw the polygon for shading the feasible set
# xpoly1 <- seq(from = xlims[1], to = 16, length.out = 500)
# ypoly1 <- ppf(xpoly1)
# polygon(x = c(xpoly1, xpoly1[1]), y = c(ypoly1, rev(ypoly1)[1]), col=COLA[1], density=NULL, border = NA)

#Draw the graphs
# lines(xx1, ppf(xx1), col = COLA[5], lwd = graphlinewidth)

#Label the feasible frontier
# text(10.5, 0.53, expression("Feasible Frontier"), cex = labelsize)
# Arrows(12.5, 0.53, 14.5, 0.53, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

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
#mtext(expression(paste("Living (hours), ", x)), side = 1, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], -0.45, expression(paste("Living (hours), ", x)), xpd = TRUE, cex = axislabelsize) 
text(-1.5, 0.5*ylims[2], expression(paste("Learning, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Add mrs = mrt at i
#text(8.6, 3.6, expression(paste(mrs(x,y) == mrt(x,y))), cex = labelsize)
#Arrows(8, 3.5, 8, 3.15, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the indifference curves
text(17, 0.6, expression(u[1]^A), cex = labelsize)
text(17, 1.65, expression(u[2]^A), cex = labelsize)
text(17, 2.5, expression(u[3]^A), cex = labelsize)

#Annotate max u point on feasibility frontier
text(8.4, ppf(8) + 0.1, expression(paste(i)), cex = labelsize)
segments(8, 0, 8, ppf(x = 8), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, ppf(x = 8), 8, ppf(x = 8), lty = 2, col = "gray", lwd = segmentlinewidth)


#text(11.118 + 0.2, indiffA(x = 11.118, ubar = uFn(8, 3)+ 1.4) + 0.1, expression(paste(d)), cex = labelsize)
#segments(0, indiffA(x = 11.118, ubar = uFn(8, 3) + 1.4), 11.118, indiffA(x = 11.118, ubar = uFn(8, 3)+1.4), lty = 2, col = "gray", lwd = segmentlinewidth)
#segments(11.118, 0, 11.118, indiffA(x = 11.118, ubar = uFn(8, 3) + 1.4), lty = 2, col = "gray", lwd = segmentlinewidth)
#points(11.118,  indiffA(x = 11.118, ubar = uFn(8, 3) + 1.4), pch = 16, col = "black", cex = 1.5)




text(2 + 0.2, indiffA(x = 2, ubar = uFn(2, 3)) + 0.1, expression(paste(f)), cex = labelsize)
#segments(0, indiffA(x = 2, ubar = uFn(8, 3) - 1.5), 2, indiffA(x = 2, ubar = uFn(8, 3) - 1.5), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(2, 0, 2, indiffA(x = 2, ubar = uFn(2, 3)), lty = 2, col = "gray", lwd = segmentlinewidth)



text(14 + 0.3, indiffA(x = 14, ubar = uFn(2, 3)) + 0.1, expression(paste(h)), cex = labelsize)
segments(0, indiffA(x = 14, ubar = uFn(2, 3)), 14, indiffA(x = 14, ubar = uFn(2, 3)), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(14, 0, 14, indiffA(x = 14, ubar = uFn(2, 3)), lty = 2, col = "gray", lwd = segmentlinewidth)



text(14 + 0.3, indiffA(x = 14, ubar = uFn(14, 3)) + 0.1, expression(paste(g)), cex = labelsize)
segments(0, indiffA(x = 14, ubar = uFn(14, 3)), 14, indiffA(x = 14, ubar = uFn(14, 3)), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(14, 0, 14, indiffA(x = 14, ubar = uFn(14, 3)), lty = 2, col = "gray", lwd = segmentlinewidth)
points(14,  indiffA(x = 14, ubar = uFn(14, 3)), pch = 16, col = "black", cex = 1.5)
points(8, ppf(x = 8), pch = 16, col = "black", cex = 1.5)
points(2,  indiffA(x = 2, ubar = uFn(2, 3)), pch = 16, col = "black", cex = 1.5)
points(14,  indiffA(x = 14, ubar = uFn(2, 3)), pch = 16, col = "black", cex = 1.5)




#text(5.605 + 0.2, indiffA(x = 5.605, ubar = uFn(8, 3)) + 0.1, expression(paste(c)), cex = labelsize)
#segments(0, indiffA(x = 5.605, ubar = uFn(8, 3)), 5.605, indiffA(x = 5.605, ubar = uFn(8, 3)), lty = 2, col = "gray", lwd = segmentlinewidth)
#segments(5.605, 0, 5.605, indiffA(x = 5.605, ubar = uFn(8, 3)), lty = 2, col = "gray", lwd = segmentlinewidth)
#points(5.605,  indiffA(x = 5.605, ubar = uFn(8, 3)), pch = 16, col = "black", cex = 1.5)

text(3, 1.1, expression("Lower"), cex = labelsize)
text(3, 0.9, expression("Utility"), cex = labelsize)

text(14, 4.1, expression("Higher"), cex = labelsize)
text(14, 3.9, expression("Utility"), cex = labelsize)




dev.off()
