#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "credit/credit_investment_macro.pdf", width = 8, height = 6)

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

isoreturnFn <- function(delta, pi = 0.125) {
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

xlims <- c(0, 1.5)
ylims <- c(0, 1.1)

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
ticksy <- c(ylims[1], 0.5, ylims[2])
ylabels <- c(NA, expression(paste(f == frac(1,2))),  NA)
ticksx <- c(xlims[1], 0.39, 0.59, xlims[2])
xlabels <- c(NA, expression(paste(delta[1]^{W})), expression(paste(delta[0]^{W})), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 503 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Draw the graphs
lines(xx1, brfFn(xx1, k = 0.28), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfFn(xx1, k = 0.5), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfFn(xx1, k = 0.75), col = COLA[4], lwd = graphlinewidth)
lines(xx1, isoreturnFn(xx1, pi = 0.175), col = COLB[4], lwd = segmentlinewidth, lty = 2)
lines(xx1, isoreturnFn(xx1, pi = 0.25), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, isoreturnFn(xx1, pi = 0.5), col = COLB[4], lwd = graphlinewidth)


#Axis labels
mtext(expression(paste("Interest factor, ", delta)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.2, 0.5*(ylims[2]), expression(paste("Probability of failure (risk), ", f)), xpd = TRUE, cex = axislabelsize, srt = 90) 

segments(0.39, -1, 0.39, brfFn(0.39, k = 0.75), lty = 2, col = "gray" , lwd = segmentlinewidth)
points(0.39, brfFn(0.39, k = 0.75), pch = 16, col = "black", cex = 1.5)
text(0.39 + 0.02, brfFn(0.39, k = 0.75) - 0.03, expression(paste(b)), cex = labelsize)

#segments(1, -1, 1, brfFn(delta = 1, k = 0.5), lty = 2, col = "gray" , lwd = segmentlinewidth)
#points(1, brfFn(1, k = 0.5), pch = 16, col = "black", cex = 1.5)
# text(1, brfFn(1, k = 0.5) + 0.04, expression(paste(n)), cex = labelsize)

segments(.59, -1, 0.59, brfFn(delta = 0.59, k = 0.75), lty = 2, col = "gray" , lwd = segmentlinewidth)
points(0.59, brfFn(0.59, k = 0.75), pch = 16, col = "black", cex = 1.5)
text(0.59 + 0.02, brfFn(0.59, k = 0.75) - 0.03, expression(paste(a)), cex = labelsize)

#segments(.7, -1, 0.7, brfFn(delta = 0.7, k = 0.28), lty = 2, col = "gray" , lwd = segmentlinewidth)
#points(0.7, brfFn(0.7, k = 0.28), pch = 16, col = "black", cex = 1.5)

text(1.4, 0.91, expression(paste(pi[1] == 1 + rho[1])), cex = labelsize)
text(1.4, 0.78, expression(paste(pi[0] == 1 + rho[0])), cex = labelsize)


text(1.1, 1.05, expression(paste("BRF of previously excluded")), cex = labelsize)
text(1.1, 1.0, expression(paste("now marginal borrower")), cex = labelsize)
Arrows(1.1, 0.97, 1.1, 0.92, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(1.1, 0.5, expression(paste("BRF of")), cex = labelsize)
text(1.1, 0.45, expression(paste("wealthy borrower")), cex = labelsize)
Arrows(1.1, 0.53, 1.1, 0.6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


text(0.6, 0.9, expression(paste("BRF of previously")), cex = labelsize)
text(0.6, 0.85, expression(paste("marginal borrower")), cex = labelsize)
Arrows(0.6, 0.82, 0.6, 0.68, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#text(1.1, 1, expression(paste("after policy change")), cex = labelsize)
#text(1.1, 0.85, expression(paste("rate increase")), cex = labelsize)

#text(1.25, 0.55, expression(paste("Zero profit condition")), cex = labelsize)
#text(1.25, 0.51, expression(paste("before policy change")), cex = labelsize)
#Arrows(1.25, 0.58, 1.25, 0.76, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#text(1.2, 0.25, expression(paste("rate increase")), cex = labelsize)

text(0.12, 0.43, expression(paste("zpc")), cex = labelsize)
text(0.12, 0.38, expression(paste("after")), cex = labelsize)
Arrows(0.17, 0.43, 0.27, 0.43, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(0.12, 0.25, expression(paste("zpc")), cex = labelsize)
text(0.12, 0.2, expression(paste("before")), cex = labelsize)
Arrows(0.17, 0.25, 0.3, 0.25, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(0.49, 0.14, expression(paste("Policy")), cex = labelsize)
text(0.49, 0.09, expression(paste("Change")), cex = labelsize)
Arrows(0.58, 0.05, 0.42, 0.05, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)



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
