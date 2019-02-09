#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Microeconomics: Competition, Conflict and Coordination

library(shape)
pdf(file = "constrained_optimization/feasible_frontier_indiff_vegetables.pdf", width = 8, height = 8)

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

ppf <- function(x, intercept = 320) {
  (intercept - 2*x^2)^0.5
}

uFn <- function(x, y, alpha = 0.4){
  (x^alpha)*(y^(1-alpha))
}

indiffA <- function(x, ubar = uFn(8, 13.87), alpha = 0.4) {
  (ubar/ (x^alpha))^(1/ (1 - alpha))
}

ylims <- c(0, 20)
xlims <- c(0, 18)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(uFn(8, 13.87) - 1, uFn(8, 13.87), uFn(8, 13.87) + 1.5) #alpha = 0.4

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
ticksy <- seq(ylims[1],  ylims[2], 2)
ylabels <- seq(ylims[1],  ylims[2], 2)
# ticksx <- c(xlims[1], 5.25, 8.944272, xlims[2])
# xlabels <- c(NA, expression(paste(x,"*")), expression(paste(bar(x))), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], sqrt(320/2-0.001), length.out = npts)


#Draw the polygon for shading the feasible set
xpoly1 <- seq(from = xlims[1], to = sqrt(320/2-0.001), length.out = npts)
ypoly1 <- ppf(xpoly1)
polygon(x = c(xpoly1, xpoly1[1]), y = c(ypoly1, rev(ypoly1)[1]), col=COLA[1], density=NULL, border = NA)

#Draw the graphs
lines(xx1, ppf(xx1), col = COLA[5], lwd = graphlinewidth)

#Label the feasible frontier
text(8.5, 0.53, expression("Feasible Frontier"), cex = labelsize)
Arrows(10.5, 0.53, 12.2, 0.53, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

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
mtext(expression(paste("Free time (hours), ", x)), side = 1, line = 2.5, cex = axislabelsize)
text(-1.6, 0.5*ylims[2], expression(paste("Crates of vegetables, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Add mrs = mrt at i
points(8, ppf(x = 8), pch = 16, col = "black", cex = 1.5)
text(8, 5, expression(paste(mrs(x,y) == mrt(x,y))), cex = labelsize)
text(8 + 0.05, 3.6, expression(paste(frac(2*y,3*x) == frac(2*x,y))), cex = labelsize)
Arrows(8, 5.2, 8, ppf(x = 8) - 0.6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#Label the indifference curves
text(17, 6.6, expression(u[1]^A), cex = labelsize)
text(17, 7.8, expression(u[2]^A), cex = labelsize)
text(17, 9.8, expression(u[3]^A), cex = labelsize)

#Annotate max u point on feasibility frontier
# text(8.4, ppf(8) + 0.1, expression(paste(i)), cex = labelsize)
# segments(8, 0, 8, ppf(x = 8), lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, ppf(x = 8), 8, ppf(x = 8), lty = 2, col = "gray", lwd = segmentlinewidth)
# points(8, ppf(x = 8), pch = 16, col = "black", cex = 1.5)
# 
# text(3.3, ppf(2.9) + 0.1, expression(paste(a)), cex = labelsize)
# segments(2.9, 0, 2.9, ppf(x = 2.9), lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, ppf(x = 2.9), 2.9, ppf(x = 2.9), lty = 2, col = "gray", lwd = segmentlinewidth)
# points(2.9, ppf(x = 2.9), pch = 16, col = "black", cex = 1.5)
# 
# 
# text(13.15, ppf(12.8) + 0.1, expression(paste(b)), cex = labelsize)
# segments(12.8, 0, 12.8, ppf(x = 12.8), lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, ppf(x = 12.8), 12.8, ppf(x = 12.8), lty = 2, col = "gray", lwd = segmentlinewidth)
# points(12.8, ppf(x = 12.8), pch = 16, col = "black", cex = 1.5)



dev.off()
