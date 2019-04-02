#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "credit/non_contractual_risk.pdf", width = 8, height = 6)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 4, 4))

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
     xaxs="i", 
     yaxs="i"
)


# ticksy <- seq(from = 0, to = ylims[2], by = 0.1)
# ylabels <- seq(from = 0, to = ylims[2], by = 0.1)
# ticksx <- seq(from = 0, to = xlims[2], by = 0.1)
# xlabels <- seq(from = 0, to = xlims[2], by = 0.1)
ticksy <- c(ylims[1], brfFn(delta = 0.5), ylims[2])
ylabels <- c(NA, expression(paste(f^{NE})), NA)
ticksx <- c(xlims[1], 0.5, xlims[2])
xlabels <- c(NA, expression(paste(delta^{NE})), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 503 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, 1, length.out = npts)

xpoly1 <- seq(from = 0.168, to = 0.5, length.out = 500)
ypoly1 <- ylow(xpoly1)
ypoly2 <- isoreturnFn(xpoly1)
polygon(x = c(xpoly1, rev(xpoly1)), y = c(ypoly1, rev(ypoly2)), col=COL[4], density=NULL, border = NA)


#Draw the graphs
lines(xx1, brfFn(xx1), col = COLA[4], lwd = graphlinewidth)
lines(xx1, PCFn(xx1), col = COLA[2], lwd = graphlinewidth)
lines(xx1, isoreturnFn(xx1), col = COLB[4], lwd = graphlinewidth)
#lines(xx2, ylow(xx2, ybar = 0.05), col = COLA[4], lwd = graphlinewidth)
#lines(xx2, yhigh(xx2, ybar = 0.05), col = COLA[4], lwd = graphlinewidth)
#lines(xx1, indiffA(xx1, alpha = 0.5, uA = 4), col = COLB[5], lwd = graphlinewidth)
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
mtext(expression(paste("Interest factor, ", delta)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.09, 0.5*(ylims[2]), expression(paste("Probability of failure (risk), ", f)), xpd = TRUE, cex = axislabelsize, srt = 90) 


contour(d1, f1,
        outer(d1, f1, yFn),
        drawlabels = FALSE,
        col = COLA[3],
        lwd = graphlinewidth,
        levels = a,
        xaxs="i",
        yaxs="i",
        add = TRUE)


#Label the indifference curves
# text(9, 1, expression(paste("Lily's, ", u[1]^L)), cex = labelsize)
# text(9, 2.3, expression(paste("Peyton's, ", u[1]^P)), cex = labelsize)

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

#text(9.5, 3.15, expression(u[2]^A), cex = labelsize)
#text(9.5, 5.85, expression(u[3]^A), cex = labelsize)


# segments(0, 8, 2.9705, 8, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(2.9705, 0, 2.9705, 8, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(2, 0, 2, 8, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(4, 0, 4, 4, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(0, 4, 4, 4, lty = 2, col = "gray" , lwd = segmentlinewidth)
# 
# 
segments(0.5, 0, 0.5, brfFn(delta = 0.5), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, brfFn(delta = 0.5), 0.5, brfFn(delta = 0.5), lty = 2, col = "gray" , lwd = segmentlinewidth)

# 
# segments(8, 0, 8,indiffA(x = 8)  , lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(0, indiffA(x = 8) , 8,indiffA(x = 8)  , lty = 2, col = "gray" , lwd = segmentlinewidth)

#Annotate points (4,4),(2,8),(8,2) on feasibility frontier
text(0.5+0.015, isoreturnFn(0.5) + 0.04, expression(paste(n)), cex = labelsize)
points(0.5, isoreturnFn(0.5), pch = 16, col = "black", cex = 1.5)

text(0.375+0.02, 0.6+0.03, expression(paste(b)), cex = labelsize)
points(0.375, 0.6, pch = 16, col = "black", cex = 1.5)

#text(0.2, 1.01, expression(paste("Iso-expected income curves")), cex = labelsize)
#text(0.2, 0.95, expression(paste(y[1] == y^L)), cex = labelsize)
text(0.2, 0.87, expression(paste(y == y^{NE})), cex = labelsize)
#text(0.2, 0.77, expression(paste(y[3] == y^H)), cex = labelsize)

text(0.62, 0.98, expression(paste("A's best response function")), cex = labelsize)
text(0.62, 0.9, expression(paste(f == frac(1,2) + frac(delta, 2*q))), cex = labelsize)

text(0.62, 0.4, expression(paste("A's participation")), cex = labelsize)
text(0.62, 0.35, expression(paste("constraint")), cex = labelsize)
text(0.62, 0.25, expression(paste(f == frac(delta, q))), cex = labelsize)
Arrows(0.62, 0.43, 0.62, 0.58, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


text(0.88, 0.6, expression(paste("P's iso-profit curve")), cex = labelsize)
text(0.88, 0.55, expression(paste(pi == pi^{NE})), cex = labelsize)
Arrows(0.88, 0.63, 0.88, 0.82, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(0.3, 0.15, expression(paste("Pareto-improving")), cex = labelsize)
text(0.3, 0.1, expression(paste("lens")), cex = labelsize)
Arrows(0.3, 0.18, 0.3, 0.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


dev.off()
