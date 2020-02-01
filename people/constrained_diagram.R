#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "people/constrained_diag.pdf", width = 9, height = 9)

#Set parameters for graphics
axislabelsize <- 1
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

ellipse <- function(x, y, a = 1, b = 2, constant = -4){
  (x^2/a^2) + (y^2)/(b^2) + constant
}

# y^2/b^2 = constant - x^2/a^2
# y^2 = b^2*(constant - x^2 / a^2)
# y = +/-sqrt(b^2*(constant - x^2 / a^2))
# 
# ellipsePos <- function(x, a = 1, b = 2, constant = 4, ky = 2, kx = 2){
#   sqrt(ky + b^2*(constant - (x- kx)^2 / a^2))
# }

ellipsePos <- function(x, a = 1, b = 2){
  (a/b)*(sqrt(a^2 - x^2))
}

ellipseNeg <- function(x, a = 1, b = 2){
  -(a/b)*(sqrt(a^2 - x^2))
}



# ellipseNeg <- function(x, a = 1, b = 2, constant = 4, ky = 2, kx = 2){
#   -sqrt(ky + b^2*(constant - (x - kx)^2 / a^2))
# }

# ppf <- function(fish, k = 0.1, alpha = 2, maxfish = 5) {
#   k * (fish - maxfish)^alpha 
# }
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

xlims <- c(0, 10)
ylims <- c(0, 11.5)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(1.8, 2.75, 4.1)

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

ticksy <- seq(from = 0, to = ylims[2], by = 1)
ylabels <- seq(from = 0, to = ylims[2], by = 1)
ticksx <- seq(from = 0, to = xlims[2], by = 1)
xlabels <- seq(from = 0, to = xlims[2], by = 1)
# ticksy <- c(ylims[1], -5, 0, 2.5, ylims[2])
# ylabels <- c(NA, NA, NA, NA, NA)
# ticksx <- c(xlims[1], 0, xlims[2])
# xlabels <- c(NA, NA, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Draw the polygon for shading the feasible set
#xpoly1 <- seq(from = 0, to = 5, length.out = 500)
#ypoly1 <- ppf(xpoly1, k = 10/25, alpha = 2, maxfish = 5)
#polygon(x = c(xpoly1, rev(xpoly1[1])), y = c(ypoly1, rev(ypoly1)[1]), col=COLA[1], density=NULL, border = NA)

#Draw the graphs
#lines(xx1, ppf(xx1, k = 10/25, alpha = 2, maxfish = 5), col = COLA[5], lwd = graphlinewidth)
lines(xx2, ellipsePos(xx1, a = 4, b = 2), col = COL[3], lwd = graphlinewidth)
lines(xx2, ellipseNeg(xx1, a = 4, b = 2), col = COL[3], lwd = graphlinewidth)
#lines(xx3, feasibleLabor(xx3, time = 10), col = COL[3], lwd = graphlinewidth)
#lines(xx4, manufactureProd(xx4, k = 0.1, alpha = 2), col = COLB[4], lwd = graphlinewidth)

#mtext(expression(paste("Quantity of fish, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-0.5, 8, expression(paste("Shirts, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(8, -0.5, expression(paste("Fish, ", x)), xpd = TRUE, cex = axislabelsize)
text(0.5, -7.5, expression(paste("Labor for Fish, ", L^f)), xpd = TRUE, cex = axislabelsize, srt= 90) 
text(-7.5, 0.5, expression(paste("Labor for Shirts, ", L^s)), xpd = TRUE, cex = axislabelsize)

#Label the points on the axes we want
text(-0.6, 2.8, expression(paste(12.5)), xpd = TRUE, cex = axislabelsize)
text(-0.5, 9.7, expression(paste(50)), xpd = TRUE, cex = axislabelsize)
text(2.9, -0.3, expression(paste(2.5)), xpd = TRUE, cex = axislabelsize)
text(5.2, -0.3, expression(paste(5)), xpd = TRUE, cex = axislabelsize)

#Label the two production functions
#Clothing
text(-3.25, 6, expression(paste("Shirt Production")), xpd = TRUE, cex = axislabelsize)
text(-3.25, 5, expression(paste(y == frac(1,2)(L^s)^2)), xpd = TRUE, cex = axislabelsize)
Arrows(-4.5, 5, -6.5, 5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Fishing
text(6.8, -4.8, expression(paste("Fish Production")), xpd = TRUE, cex = axislabelsize)
text(6.8, -5.8, expression(paste(x == frac(1,2)(L^f))), xpd = TRUE, cex = axislabelsize)
Arrows(5.8, -5.8, 3.5, -5.8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Draw segments for the 50/50 split of time
# segments(-5, -5, -5, 2.5, lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(-5, -5, 2.5, -5, lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(-5, 2.5, 2.5, 2.5, lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(2.5, -5, 2.5, 2.5, lty = 2, col = "gray", lwd = segmentlinewidth)

#Annotate Max time on clothes
segments(-10, 0, -10, 10, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(-10, 10, 0, 10, lty = 2, col = "gray", lwd = segmentlinewidth)
text(-6.5, 9.5, expression(paste("10 hrs of labor")))
text(-6.5, 8.9, expression(paste("for shirts produces")))
text(-6.5, 8.3, expression(paste("50 shirts")))
#points(-10, 10, pch = 16, col = "black", cex = 1.5)

#Annotate Max time on fishing
segments(0, -10, 5, -10, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(5, -10, 5, 0, lty = 2, col = "gray", lwd = segmentlinewidth)
text(8, -8.4, expression(paste("10 hrs of labor")))
text(8, -9, expression(paste("for fishing produces")))
text(8, -9.6, expression(paste("5 kgs of fish")))
#points(5, -10, pch = 16, col = "black", cex = 1.5)

#Annotate point on ppf 
text(4.75, 3.35, expression(paste("12.5 Shirts and")))
text(5, 2.75, expression(paste("2.5 Kilograms of Fish")))

#Annotate point on labor feasibility frontier
points(-5, -5, pch = 16, col = "black", cex = 1.5)
text(-6.75, -5, expression(paste(list(L^S ==5, L^F == 5) )))

text(-3, -9, expression(paste(L^S + L^F <= 10)))
Arrows(-3, -8.5, -3, -7.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the feasible frontier
text(4.5, 8, expression("Feasible Frontier"))
text(4.5, 7.4, expression("(production possibilities frontier)"))



dev.off()
