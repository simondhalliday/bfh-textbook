#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Microeconomics: Competition, Conflict and Coordination

library(shape)
pdf(file = "constrained_optimization/feasible_frontier_indifference_check.pdf", width = 8, height = 8)

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
grays <- gray.colors(25, start = 1, end = 0)


#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 1, 1))

ppf <- function(x, slope = 1/64, bary = 4) {
  bary - slope *x^2
}

uFn <- function(x, y, alpha = 0.4){
  (x^alpha)*(y^(1-alpha))
}

indiffA <- function(x, ubar = uFn(8,3) - 1, alpha = 0.4) {
  (ubar/ (x^alpha))^(1/ (1 - alpha))
}

tangentLine <- function(x, slope, intercept){
  intercept - slope*x
}

ylims <- c(0, 5)
xlims <- c(0, 18)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(uFn(3, ppf(3)), uFn(8, 3), uFn(8, 3) + 1.4) #alpha = 0.4

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

#Series for tangent line
xx3 <- seq(4.5, 11.5, length.out = npts)

#Draw the polygon for shading the feasible set
xpoly1 <- seq(from = xlims[1], to = 16, length.out = 500)
ypoly1 <- ppf(xpoly1)
polygon(x = c(xpoly1, xpoly1[1]), y = c(ypoly1, rev(ypoly1)[1]), col=COLA[1], density=NULL, border = NA)

#Draw the graphs
lines(xx1, ppf(xx1), col = COLA[5], lwd = graphlinewidth)

lines(xx3, tangentLine(xx3, slope = 1/4, intercept = 5), col = grays[22], lwd = segmentlinewidth, lty = 2)

#Label the feasible frontier
text(10.5, 0.60, expression("Feasible"), cex = labelsize)
text(10.5, 0.46, expression("frontier"), cex = labelsize)
Arrows(12, 0.53, 14.5, 0.53, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

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
text(0.5*xlims[2], -0.45, expression(paste("Living (hours), ", x)), xpd = TRUE, cex = axislabelsize) 
text(-1.5, 0.5*ylims[2], expression(paste("Learning, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Add mrs = mrt at i
text(12.4, 3, expression(paste(mrs(x,y) == mrt(x,y))), cex = labelsize)
Arrows(9.5, 3, 8.5, 3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the indifference curves
text(17, 1.06, expression(u[1]^K), cex = labelsize)
text(17, 1.65, expression(u[2]^K), cex = labelsize)
text(17, 2.7, expression(u[3]^K), cex = labelsize)

#Annotate max u point on feasibility frontier
text(8, ppf(8) + 0.15, expression(paste(b)), cex = labelsize)
segments(8, 0, 8, ppf(x = 8), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, ppf(x = 8), 8, ppf(x = 8), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(8, ppf(x = 8), pch = 16, col = "black", cex = 1.5)

text(3.3, ppf(3) + 0.1, expression(paste(a)), cex = labelsize)
segments(3, 0, 3, ppf(x = 3), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, ppf(x = 3), 3, ppf(x = 3), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(3, ppf(x = 3), pch = 16, col = "black", cex = 1.5)


text(13.05, ppf(12.7) + 0.1, expression(paste(c)), cex = labelsize)
segments(12.7, 0, 12.7, ppf(x = 12.7), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, ppf(x = 12.7), 12.7, ppf(x = 12.7), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(12.7, ppf(x = 12.7), pch = 16, col = "black", cex = 1.5)




dev.off()
