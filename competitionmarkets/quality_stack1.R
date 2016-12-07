#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "credit/quality_stack1.pdf", width = 8, height = 6)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 5, 4, 4))

brfFn <- function(delta, mu = 1, k = 1) {
  .5 + (delta / (2 * mu)) * (1 - k)
}

PCFn <- function(delta, mu = 1) {
  delta/mu
}

isoreturnFn <- function(delta, pi=0.125) {
  1 - (pi)/delta
}

yFn <- function(d1, f1, mu = 1){
  mu*f1*(1 - f1) - d1*(1 - f1)
}

ylow <- function(delta, mu = 1, ybar = 0.0625){
  (-sqrt(delta^2 - 2*delta*mu + mu^2 - 4*mu*ybar) + delta + mu)/(2*mu)
}

yhigh <- function(delta, mu = 1, ybar = 0.0625){
  (sqrt(delta^2 - 2*delta*mu + mu^2 - 4*mu*ybar) + delta + mu)/(2*mu)
}

xlims <- c(0, 2)
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
ticksy <- c(ylims[1], 0.5, 0.75, ylims[2])
ylabels <- c(NA, expression(paste(frac(1,2))), expression(paste(f^{0} == frac(3,4))), NA)
ticksx <- c(xlims[1], 0.64, 1, 1.5, xlims[2])
xlabels <- c(NA, expression(paste(delta^{L})),expression(paste(delta^{0})), expression(paste(delta^{H})), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 503 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Draw the graphs
lines(xx1, brfFn(xx1, k = 0), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfFn(xx1, k = 0.5), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfFn(xx1, k = 0.666), col = COLA[4], lwd = graphlinewidth)
lines(xx1, isoreturnFn(xx1, pi = 0.25), col = COLB[4], lwd = graphlinewidth)
lines(xx1, isoreturnFn(xx1, pi = 0.375), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, isoreturnFn(xx1, pi = 0.5), col = COLB[4], lwd = graphlinewidth)


#Axis labels
mtext(expression(paste("Interest factor, ", delta)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.27, 0.5*(ylims[2]), expression(paste("Probability of failure (risk), ", f)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Label feasible and infeasible
#text(8, 9, expression(paste(trs(l,k) == frac(x[l], x[k]), phantom() == frac(mp[l], mp[k]), phantom() == -frac(dk,dl))), cex = labelsize)
#text(3, 2.5, expression(paste("with plan ", (list(x, l, k)) )), cex = labelsize)

# text(8, 8.5, expression(paste("Production set:")), cex = labelsize)
# text(8, 8, expression(paste(x = bar(x), " feasible")), cex = labelsize)
# text(8, 7.5, expression(paste("with plan ", (list(x, l, k)) )), cex = labelsize)


segments(0, 0.75, xlims[2], 0.75, lty = 2, col = "gray" , lwd = segmentlinewidth)



segments(1.5, 0, 1.5, brfFn(delta = 1.5, k = 0.666), lty = 2, col = "gray" , lwd = segmentlinewidth)
points(1.5, brfFn(1.5, k = 0.666), pch = 16, col = "black", cex = 1.5)
text(1.5 + 0.04, brfFn(1.5, k = 0.666) - 0.03, expression(paste(b)), cex = labelsize)

segments(1, 0, 1, brfFn(delta = 1, k = 0.5), lty = 2, col = "gray" , lwd = segmentlinewidth)
points(1, brfFn(1, k = 0.5), pch = 16, col = "black", cex = 1.5)
text(1, brfFn(1, k = 0.5) + 0.04, expression(paste(n)), cex = labelsize)

segments(0.64, 0, 0.64, brfFn(delta = 0.64, k = 0.666), lty = 2, col = "gray" , lwd = segmentlinewidth)
points(0.64, brfFn(0.64, k = 0.666), pch = 16, col = "black", cex = 1.5)
text(0.64 + 0.04, brfFn(0.64, k = 0.666) - 0.03, expression(paste(a)), cex = labelsize)


# 
# segments(8, 0, 8,indiffA(x = 8)  , lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(0, indiffA(x = 8) , 8,indiffA(x = 8)  , lty = 2, col = "gray" , lwd = segmentlinewidth)

#Annotate points (4,4),(2,8),(8,2) on feasibility frontier


# text(0.375+0.02, 0.6+0.03, expression(paste(b)), cex = labelsize)
# points(0.375, 0.6, pch = 16, col = "black", cex = 1.5)

#text(0.2, 1.01, expression(paste("Iso-expected income curves")), cex = labelsize)
#text(0.2, 0.95, expression(paste(y[1] == y^L)), cex = labelsize)
#text(0.2, 0.87, expression(paste(y == y^{NE})), cex = labelsize)
#text(0.2, 0.77, expression(paste(y[3] == y^H)), cex = labelsize)

text(0.61, 0.98, expression(paste("Excluded borrower's BRF")), cex = labelsize)
text(0.58, 0.93, expression(paste(f(delta, k < k^0))), cex = labelsize)

text(1.55, 0.98, expression(paste("Marginal borrower's BRF")), cex = labelsize)
text(1.48, 0.93, expression(paste(f(delta, k == k^0))), cex = labelsize)


text(1.75, 0.5, expression(paste("BRF for borrower")), cex = labelsize)
text(1.75, 0.45, expression(paste("with more than")), cex = labelsize)
text(1.75, 0.4, expression(paste("minimum collateral")), cex = labelsize)
text(1.75, 0.35, expression(paste(f(delta, k > k^0))), cex = labelsize)
Arrows(1.75, 0.53, 1.75, 0.77, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(0.8, 0.43, expression(paste(pi > 1 + rho)), cex = labelsize)

text(0.2, 0.35, expression(paste("Zero profit")), cex = labelsize)
text(0.2, 0.3, expression(paste("condition")), cex = labelsize)
text(0.2, 0.25, expression(paste(pi == 1 + rho)), cex = labelsize)

#text(0.62, 0.9, expression(paste(f == frac(1,2) + frac(delta, 2*mu))), cex = labelsize)

# text(0.62, 0.4, expression(paste("A's participation")), cex = labelsize)
# text(0.62, 0.35, expression(paste("constraint")), cex = labelsize)
# text(0.62, 0.25, expression(paste(f == frac(delta, mu))), cex = labelsize)



# text(0.88, 0.6, expression(paste("P's iso-profit curve")), cex = labelsize)
# text(0.88, 0.55, expression(paste(pi == pi^{NE})), cex = labelsize)
# Arrows(0.88, 0.63, 0.88, 0.82, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

# text(0.3, 0.15, expression(paste("Pareto-improving")), cex = labelsize)
# text(0.3, 0.1, expression(paste("lens")), cex = labelsize)
# Arrows(0.3, 0.18, 0.3, 0.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


dev.off()
