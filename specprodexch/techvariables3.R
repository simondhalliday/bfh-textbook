#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "specprodexch/techvariables3.pdf", width = 7, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 1, 1))

uFn <- function(x, y, alpha = 0.7){
  (x^alpha)*(y^(1 - alpha))
}

isoA <- function(l, alpha = 0.7, xbar = 4, A = 2) {
  (xbar / A)^2 - alpha*l
}

trsline <- function(x, constant = 10, slope = 4){
  constant - (slope)*x
}

ff <- function(x, c = 12, s = 1/3){
  c - s*x
}

xlims <- c(0, 10)
ylims <- c(0, 10)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(2, 4, 6)

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
ticksy <- c(ylims[1],  ylims[2])
ylabels <- c(NA, NA)
ticksx <- c(xlims[1], xlims[2])
xlabels <- c(NA, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(1.5, 2.5, length.out = npts)
xx4 <- seq(3, 5, length.out = npts)
xx5 <- seq(7, 9, length.out = npts)

#Draw the graphs
lines(xx1, isoA(xx1, alpha = 0.8, xbar = 3.5, A = 2), col = COLA[5], lwd = graphlinewidth)
lines(xx1, isoA(xx1, alpha = 0.8, xbar = 4.5, A = 2 ), col = COLA[5], lwd = graphlinewidth)
lines(xx1, isoA(xx1, alpha = 0.8, xbar = 5.5, A = 2), col = COLA[5], lwd = graphlinewidth)
#lines(xx1, isoA(xx1, alpha = 0.3, xbar = 4), col = COLB[5], lwd = graphlinewidth)
#lines(xx1, ff(xx1, c = 9, s = 1.34), col = COL[3], lwd = graphlinewidth)
# lines(xx3, trsline(xx3, constant = 16), col = "gray", lty = 2, lwd = graphlinewidth)
# lines(xx4, trsline(xx4, constant = 8, slope = 1), col = "gray", lty = 2, lwd = graphlinewidth)
# lines(xx5, trsline(xx5, constant = 4, slope = 0.25), col = "gray", lty = 2, lwd = graphlinewidth)
#lines(xx4, mcline(xx4, constant = totalcost(x = 3, c0 = 2, c1 = 0.05, c2 = 0.05) - 3*marginalcost(x = 3, c1 = 0.05, c2 = 0.05), slope = marginalcost(x = 3, c1 = 0.05, c2 = 0.05)), col = "gray", lty = 2, lwd = graphlinewidth)



#Label the feasible frontier
# text(3.2, 1, expression("Feasible Frontier"), cex = labelsize)
# text(3.2, 0.5, expression("(production possibilities frontier)"), cex = labelsize)
# Arrows(4.35, 0.95, 8.1, 0.95, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Axis labels
mtext(expression(paste("Hours of labor, ", l)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.8, 5, expression(paste("Quantity of capital goods, ", k)), xpd = TRUE, cex = axislabelsize, srt = 90) 


# contour(x, y, 
#         outer(x, y, uFn),
#         drawlabels = FALSE,
#         col = COLB[3],
#         lwd = graphlinewidth,
#         levels = a, 
#         xaxs="i", 
#         yaxs="i", 
#         add = TRUE)


#Label the indifference curves
#text(8, 1.6, expression(paste("Technology E, ", x[1]^E)), cex = labelsize)
# text(6, 9, expression(paste("Technology E Production: ", x == A*(alpha*l + k)^frac(1,2))), cex = labelsize)
# text(6, 8, expression(paste("Isoquant curve: ", k == bgroup("(",frac(bar(x), A),")")^2 - alpha*l   )), cex = labelsize)
# text(6, 7, expression(paste(trs(l,k) == frac(x[l],x[k]), phantom() == alpha)), cex = labelsize)

text(3.5, 0.7, expression(paste(underline(x)[1]^S)), cex = labelsize)
text(6, 0.7, expression(paste(underline(x)[2]^S)), cex = labelsize)
text(9, 0.7, expression(paste(underline(x)[3]^S)), cex = labelsize)

# text(8, 4, expression(paste(trs(l,k) == frac(2,8), phantom() == 0.25)), cex = labelsize)
# Arrows(8, 3.6, 8, 2.4, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# 
# text(4, 6, expression(paste(trs(l,k) == frac(4,4), phantom()==1)), cex = labelsize)
# Arrows(4, 5.6, 4, 4.4, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# 
# text(4, 8, expression(paste(trs(l,k) == frac(8,2), phantom() == 4)), cex = labelsize)
# Arrows(3.05, 8, 2.3, 8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#Label feasible and infeasible
#text(8, 9, expression(paste(trs(l,k) == frac(x[l], x[k]), phantom() == frac(mp[l], mp[k]), phantom() == -frac(dk,dl))), cex = labelsize)
#text(3, 2.5, expression(paste("with plan ", (list(x, l, k)) )), cex = labelsize)

# text(8, 8.5, expression(paste("Production set:")), cex = labelsize)
# text(8, 8, expression(paste(x = bar(x), " feasible")), cex = labelsize)
# text(8, 7.5, expression(paste("with plan ", (list(x, l, k)) )), cex = labelsize)

#segments(0, 0, 4, ylims[2], lty = 2, col = "gray" , lwd = segmentlinewidth)

# segments(0, 8, 2.9705, 8, lty = 2, col = "gray" , lwd = segmentlinewidth)
#segments(2.9705, 0, 2.9705, 8, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(2, 0, 2, 8, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(4, 0, 4, 4, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(0, 4, 4, 4, lty = 2, col = "gray" , lwd = segmentlinewidth)
# 
# 
# segments(8, 0, 8, 2, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(0, 2, 8, 2, lty = 2, col = "gray" , lwd = segmentlinewidth)
# 
# segments(8, 0, 8,indiffA(x = 8)  , lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(0, indiffA(x = 8) , 8,indiffA(x = 8)  , lty = 2, col = "gray" , lwd = segmentlinewidth)

#Annotate points (4,4),(2,8),(8,2) on feasibility frontier
# text(4.2, 4.2, expression(paste(b)), cex = labelsize)
# points(2.1, isoA(l = 2.1, alpha = 0.3, xbar = 4), pch = 16, col = "black", cex = 1.5)
# text(2.1 - 0.2, isoA(l = 2.1, alpha = 0.3, xbar = 4), expression(paste(b)), cex = labelsize)


# points(3.04, isoA(l = 3.04, alpha = 0.7, xbar = 4), pch = 16, col = "black", cex = 1.5)
# text(3.04 - 0.2, isoA(l = 3.04, alpha = 0.7, xbar = 4), expression(paste(a)), cex = labelsize)

#points(4.8, indiffA(x = 4.8, alpha = 0.7), pch = 16, col = "black", cex = 1.5)


# text(7, 8, expression(paste("Lily's Utility: ", u^L*(list(x^L, y^L)) == (x^L)^{0.7}*(y^L)^{0.3})), cex = labelsize)
# text(7, 7, expression(paste("Peyton's Utility: ", u^P*(list(x^P, y^P)) == (x^P)^{0.5}*(y^P)^{0.5})), cex = labelsize)



dev.off()
