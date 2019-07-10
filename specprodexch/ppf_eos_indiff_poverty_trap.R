#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "ppf_eos_indiff_poverty_trap.pdf", width = 9, height = 9)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 5, 4, 4))

#Change this to make it log of l 

# ppf <- function(l, k = 0.5) {
#   k * (1/l)
# }

ppf <- function(fish, k = 10/25, alpha = 2, maxfish = 5) {
  k * (fish - maxfish)^alpha 
}
# 
# fishProd <- function(l, k = 0.5){
#   (-k)*l
# }
# 
# feasibleLabor <- function(l, time = 10){
#   -time - l
# }
# 
# manufactureProd <- function(l, k = 0.1, alpha = 2){
#   k * (-l)^alpha
# }

uFn <- function(x, y, alpha = 0.5){
  (x^alpha)*(y^(1-alpha))
}

budgetExchange <- function(x, yintercept = 12, slope = 1){
  yintercept - slope * x
}

xlims <- c(0, 8)
ylims <- c(0, 11)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(sqrt(9.375), 4.1)

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
# ticksx <- seq(from = 0, to = xlims[2], by = 1)
# xlabels <- seq(from = 0, to = xlims[2], by = 1)
ticksy <- c(ylims[1], 10, ylims[2])
ylabels <- c(NA, expression(paste(bar(x)^S)) , NA)
ticksx <- c(xlims[1], 5,  xlims[2])
xlabels <- c(NA, expression(paste(bar(x)^F)), NA)


axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(0, 5, length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Draw the polygon for shading the feasible set
xpoly1 <- seq(from = 0, to = 5, length.out = 500)
ypoly1 <- ppf(xpoly1, k = 10/25, alpha = 2, maxfish = 5)
polygon(x = c(xpoly1, rev(xpoly1[1])), y = c(ypoly1, rev(ypoly1)[1]), col=COLA[1], density=NULL, border = NA)

#Draw the graphs
lines(xx1, ppf(xx1, k = 10/25, alpha = 2, maxfish = 5), col = COLA[5], lwd = graphlinewidth)
lines(xx2, budgetExchange(xx2,  yintercept = 10, slope = 1.5), col = COL[3], lwd = graphlinewidth)
lines(xx2, budgetExchange(xx2,  yintercept = 7.5, slope = 1.5), col = COL[3], lwd = graphlinewidth)
#lines(xx3, feasibleLabor(xx3, time = 10), col = COL[3], lwd = graphlinewidth)
#lines(xx4, manufactureProd(xx4, k = 0.1, alpha = 2), col = COLB[4], lwd = graphlinewidth)

#Label axes
mtext(expression(paste("Quantity of fish (kilograms), ", x)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.9, 0.5*ylims[2], expression(paste("Quantity of shirts, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 


contour(x, y, 
        outer(x, y, uFn),
        drawlabels = FALSE,
        col = COLB[3],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE)

#Label the indifference curves
# text(7.2, 0.2, expression(u[1]^A), cex = labelsize)
# text(7.2, 0.8, expression(u[2]^A), cex = labelsize)
text(7.2, 2.05, expression(u^S), cex = labelsize)
text(7.2, 1.05, expression(u^F), cex = labelsize)

#Label point d
# text(1.65 + 0.2, ppf(fish = 1.65) +.2, expression(d), cex = labelsize)
# segments(1.65, 0, 1.65, ppf(fish = 1.65), lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, ppf(fish = 1.65), 1.65, ppf(fish = 1.65, k = 10/25, alpha = 2, maxfish = 5), lty = 2, col = "gray", lwd = segmentlinewidth)
# points(1.65, ppf(fish = 1.65), pch = 16, col = "black", cex = 1.5)

#Label points A and B
text(3.4 + 0.2, budgetExchange(3.4,  yintercept = 10, slope = 1.5) +.2, expression(A), cex = labelsize)
# segments(3.4, 0, 3.4, budgetExchange(3.4,  yintercept = 10, slope = 1.5), lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, budgetExchange(3.4,  yintercept = 10, slope = 1.5), 3.4, budgetExchange(3.4,  yintercept = 10, slope = 1.5), lty = 2, col = "gray", lwd = segmentlinewidth)
points(3.4, budgetExchange(3.4,  yintercept = 10, slope = 1.5), pch = 16, col = "black", cex = 1.5)
points(2.5,3.75, pch = 16, col = "black", cex = 1.5)
text(2.7, 3.95, expression(B), cex = labelsize)


#Label point s
# text(0.2, 10.2, expression(s), cex = labelsize)
# points(0.02, 10, pch = 16, col = "black", cex = 1.5)

#Label the feasible frontier
text(4, 8, expression("Feasible Frontier"), cex = 0.8 )
text(4, 7.8, expression("(production possibilities frontier)"), cex = 0.8)
Arrows(2.8, 7.8, 0.75, 7.8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#Label the exchange constraints
text(4.8, 9, expression("Budget Constraint when Specializing in Shirts"), cex = 0.8)
Arrows(3.2, 9, 0.9, 9, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


text(6.7, 0.3, expression("Budget Constraint when Specializing"), cex = 0.8)
text(7, 0.1, expression("in Fish"), cex = 0.8)
Arrows(5.4, 0.3, 5.1, 0.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label mrt = mrs
# text(5.5, 4.5, expression(paste(mrs(x,y) == mrt(x,y))), cex = labelsize)
# Arrows(4.5, 4.5, 1.9, 4.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


dev.off()
