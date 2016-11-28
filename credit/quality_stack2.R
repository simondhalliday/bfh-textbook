#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "credit/quality_stack2.pdf", width = 8, height = 6)

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

xlims <- c(0, 1.5)
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
     bty = "n"
     # xaxs="i", 
     # yaxs="i"
)


# ticksy <- seq(from = 0, to = ylims[2], by = 0.1)
# ylabels <- seq(from = 0, to = ylims[2], by = 0.1)
# ticksx <- seq(from = 0, to = xlims[2], by = 0.1)
# xlabels <- seq(from = 0, to = xlims[2], by = 0.1)
ticksy <- c(ylims[1], 0.25, 0.5, 0.75, ylims[2])
ylabels <- c(NA, expression(paste(I[A])), expression(paste(I[B])),expression(paste(I[C])), NA)
ticksx <- c(xlims[1], 0.39, 0.46, 0.59, 0.7, 1, xlims[2])
xlabels <- c(NA, expression(paste(delta[t+1]^{A})),expression(paste(delta[t+1]^{B})), expression(paste(delta[t]^{A})),expression(paste(delta[t+1]^{C})), expression(paste(delta[t]^{B})), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 503 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Draw the graphs
# lines(xx1, brfFn(xx1, k = 0.28), col = COLA[4], lwd = graphlinewidth)


xpoly4 <- c(0.59, 1.2, 1.2, 0.59, 0.59)
ypoly4 <- c(0, 0, 0.25, 0.25, 0)
polygon(x = xpoly4, y = ypoly4, col=COLA[1], density=NULL, border = NA)

xpoly5 <- c(1, 1.1, 1.1, 1, 1)
ypoly5 <- c(0.25, 0.25, 0.5, 0.5, 0.25)
polygon(x = xpoly5, y = ypoly5, col=COLA[1], density=NULL, border = TRUE)


xpoly6 <- c(0.39, 0.59, 0.59, 0.39, 0.39)
ypoly6 <- c(0, 0, 0.25, 0.25, 0)
polygon(x = xpoly6, y = ypoly6, col=COLB[1], density=NULL, border = NA)

xpoly7 <- c(0.46, 1, 1, 0.46, 0.46)
ypoly7 <- c(0.25, 0.25, 0.5, 0.5, 0.25)
polygon(x = xpoly7, y = ypoly7, col=COLB[1], density=NULL, border = NA)

xpoly8 <- c(0.7, 0.8, 0.8, 0.7, 0.7)
ypoly8 <- c(0.5, 0.5, 0.75, 0.75, 0.5)
polygon(x = xpoly8, y = ypoly8, col=COLB[1], density=NULL, border = NA)


xpoly1 <- c(0, 1.2, 1.2, 0, 0)
ypoly1 <- c(0, 0, 0.25, 0.25, 0)
polygon(x = xpoly1, y = ypoly1, col=NA, density=NULL, border = TRUE)
xpoly2 <- c(0, 1.1, 1.1, 0, 0)
ypoly2 <- c(0.25, 0.25, 0.5, 0.5, 0.25)
polygon(x = xpoly2, y = ypoly2, col=NA, density=NULL, border = TRUE)
xpoly3 <- c(0, 0.8, 0.8, 0, 0)
ypoly3 <- c(0.5, 0.5, 0.75, 0.75, 0.5)
polygon(x = xpoly3, y = ypoly3, col=NA, density=NULL, border = TRUE)


#Axis labels
mtext(expression(paste("Rate of return (r) ", + 1, ", Interest factor, ", delta)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.22, 0.5*(ylims[2]), expression(paste("Investment from projects, ", I)), xpd = TRUE, cex = axislabelsize, srt = 90) 

segments(0.46, 0, 0.46, ylims[2], lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0.39, 0, 0.39, ylims[2], lty = 2, col = "gray" , lwd = segmentlinewidth)



text(1.4, 0.125, expression(paste("Project A")), cex = labelsize)
text(1.4, 0.375, expression(paste("Project B")), cex = labelsize)
text(1.4, 0.625, expression(paste("Project C")), cex = labelsize)

text(0.2, 0.65, expression(paste("Increase in")), cex = labelsize)
text(0.2, 0.60, expression(paste("investment")), cex = labelsize)
Arrows(0.35, 0.53, 0.35, 0.71, col = "black", code = 3, lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


segments(1, -1, 1, ylims[2], lty = 2, col = "gray" , lwd = segmentlinewidth)
# points(1, brfFn(1, k = 0.5), pch = 16, col = "black", cex = 1.5)
# text(1, brfFn(1, k = 0.5) + 0.04, expression(paste(n)), cex = labelsize)

segments(.59, -1, 0.59, ylims[2], lty = 2, col = "gray" , lwd = segmentlinewidth)
# points(0.59, brfFn(0.59, k = 0.75), pch = 16, col = "black", cex = 1.5)
# text(0.6 + 0.02, brfFn(0.64, k = 0.75) - 0.03, expression(paste(a)), cex = labelsize)

segments(.7, -1, 0.7, ylims[2], lty = 2, col = "gray" , lwd = segmentlinewidth)
# points(0.7, brfFn(0.7, k = 0.28), pch = 16, col = "black", cex = 1.5)

text(0.85, 0.425, expression(paste("Increase in")), cex = labelsize)
text(0.85, 0.375, expression(paste("economic")), cex = labelsize)
text(0.85, 0.325, expression(paste("profits")), cex = labelsize)


text(0.85, 0.175, expression(paste("Initial")), cex = labelsize)
text(0.85, 0.125, expression(paste("economic")), cex = labelsize)
text(0.85, 0.075, expression(paste("profits")), cex = labelsize)


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

# text(0.61, 0.98, expression(paste("Excluded borrower's BRF")), cex = labelsize)
# text(0.58, 0.93, expression(paste(f(delta, k < k^0))), cex = labelsize)

# text(1.55, 0.98, expression(paste("Marginal borrower's BRF")), cex = labelsize)
# text(1.48, 0.93, expression(paste(f(delta, k == k^0))), cex = labelsize)


# text(1.75, 0.5, expression(paste("BRF for borrower")), cex = labelsize)
# text(1.75, 0.45, expression(paste("with more than")), cex = labelsize)
# text(1.75, 0.4, expression(paste("minimum collateral")), cex = labelsize)


# text(0.1, 0.43, expression(paste(1 + rho[0])), cex = labelsize)
# Arrows(0.17, 0.43, 0.26, 0.43, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# text(0.1, 0.25, expression(paste(1 + rho[1])), cex = labelsize)
# Arrows(0.17, 0.25, 0.29, 0.25, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# Arrows(0.88, 0.63, 0.88, 0.82, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

# text(1.1, 1.04, expression(paste("Zero profit condition")), cex = labelsize)
# text(1.1, 1, expression(paste("after policy change")), cex = labelsize)
# Arrows(1.1, 0.97, 1.1, 0.88, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
#text(1.1, 0.85, expression(paste("rate increase")), cex = labelsize)

# text(1.25, 0.55, expression(paste("Zero profit condition")), cex = labelsize)
# text(1.25, 0.51, expression(paste("before policy change")), cex = labelsize)
# Arrows(1.25, 0.58, 1.25, 0.76, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#text(1.2, 0.25, expression(paste("rate increase")), cex = labelsize)



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
