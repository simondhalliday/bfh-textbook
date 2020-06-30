#' Graph Designer: Simon Halliday
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "credit/credit_investment_macro_b.pdf", width = 8, height = 6)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 6, 1, 1))

brfFn <- function(delta, q = 1, k = 1) {
  .5 + (delta / (2 * q)) * (1 - k)
}

PCFn <- function(delta, q = 1) {
  delta/q
}

isoreturnFn <- function(delta, pi = 0.125) {
  1 - (pi)/delta
}

yFn <- function(d1, f1, q = 1){
  q*f1*(1 - f1) - d1*(1 - f1)
}

ylow <- function(delta, q = 1, ybar = 0.0625){
  (-sqrt(delta^2 - 2*delta*q + q^2 - 4*q*ybar) + delta + q)/(2*q)
}

yhigh <- function(delta, q = 1, ybar = 0.0625){
  (sqrt(delta^2 - 2*delta*q + q^2 - 4*q*ybar) + delta + q)/(2*q)
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
     xaxs = "i", 
     yaxs = "i"
)


ticksy <- c(ylims[1], 0.5, ylims[2] - 0.1)
ylabels <- c(NA, expression(paste(f == frac(1,2))),  1)
ticksx <- c(xlims[1], 0.7, 1, xlims[2])
#xlabels <- c(NA, expression(paste(delta[0]^{W})), expression(paste(delta[1]^{W})), NA)
xlabels <- c(NA, NA, NA, NA)

text(0.7, -0.07, expression(paste(delta[0]^{N})), cex = labelsize, xpd = TRUE)
text(1, -0.07, expression(paste(delta[1]^{N})), cex = labelsize, xpd = TRUE)


npts <- 503 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

# Polygon
# COLD <- c("#deebf7", "#9ecae1")
# xpoly <- seq(from = 0.175, to = xlims[2], length.out = 500)
# ypoly <- isoreturnFn(xpoly, pi = 0.175)
# polygon(x = xpoly, y = ypoly, col = COLD[1], density = NULL, border = NA)
# 
# xpoly1 <- c(0.175, xlims[2], xlims[2])
# ypoly1 <- c(0, 0, isoreturnFn(xlims[2], pi = 0.175))
# polygon(x = xpoly1, y = ypoly1, col = COLD[1], density = NULL, border = NA)
# 
# xpoly2 <- seq(from = 0.25, to = xlims[2], length.out = 500)
# ypoly2 <- isoreturnFn(xpoly2, pi = 0.25)
# polygon(x = xpoly2, y = ypoly2, col = COLD[2], density = NULL, border = NA)
# 
# xpoly3 <- c(0.25, xlims[2], xlims[2])
# ypoly3 <- c(0, 0, isoreturnFn(xlims[2], pi = 0.25))
# polygon(x = xpoly3, y = ypoly3, col = COLD[2], density = NULL, border = NA)
# 
# segments(0.25, 0, xlims[2], isoreturnFn(xlims[2], pi = 0.25), col = COLD[2])


axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#Draw the graphs
lines(xx1, brfFn(xx1, k = 0.28), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfFn(xx1, k = 0.5), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfFn(xx1, k = 0.75), col = COLA[4], lwd = graphlinewidth)
lines(xx1, isoreturnFn(xx1, pi = 0.175), col = COLB[4], lwd = graphlinewidth)
lines(xx1, isoreturnFn(xx1, pi = 0.25), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, isoreturnFn(xx1, pi = 0.5), col = COLB[4], lwd = graphlinewidth)


#Axis labels
mtext(expression(paste("Interest factor, ", delta)), side = 1, line = 3.3, cex = axislabelsize)
text(-0.2, 0.5*(ylims[2]), expression(paste("Probability of failure (risk), ", f)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#segments(0.39, -1, 0.39, brfFn(0.39, k = 0.75), lty = 2, col = grays[20] , lwd = segmentlinewidth)
#points(0.39, brfFn(0.39, k = 0.75), pch = 16, col = "black", cex = 1.5)
#text(0.39 + 0.025, brfFn(0.39, k = 0.75) - 0.03, expression(paste(b)), cex = labelsize)

segments(1, -1, 1, brfFn(delta = 1, k = 0.5), lty = 2, col = grays[20] , lwd = segmentlinewidth)
#points(1, brfFn(1, k = 0.5), pch = 16, col = "black", cex = 1.5)
# text(1, brfFn(1, k = 0.5) + 0.04, expression(paste(n)), cex = labelsize)

#segments(.59, -1, 0.59, brfFn(delta = 0.59, k = 0.75), lty = 2, col = grays[20] , lwd = segmentlinewidth)
#points(0.59, brfFn(0.59, k = 0.75), pch = 16, col = "black", cex = 1.5)
#text(0.59 + 0.025, brfFn(0.59, k = 0.75) - 0.03, expression(paste(a)), cex = labelsize)

segments(.7, -1, 0.7, brfFn(delta = 0.7, k = 0.28), lty = 2, col = grays[20] , lwd = segmentlinewidth)
#points(0.7, brfFn(0.7, k = 0.28), pch = 16, col = "black", cex = 1.5)



points(0.7, 0.75, pch = 16, col = "black", cex = 1.5)
text(0.7, 0.79, expression(paste(n[0])), cex = labelsize)

points(1, 0.75, pch = 16, col = "black", cex = 1.5)
text(1, 0.79, expression(paste(n[1])), cex = labelsize)


# ZPC
text(1.4, 0.925, expression(paste(hat(pi)[0]({b==0}) )), cex = labelsize, xpd = TRUE)
text(1.4, 0.76, expression(paste(hat(pi)[1]({b>0}) )), cex = labelsize, xpd = TRUE)


text(1.1, 1.08, expression(paste("BRF of previously excluded")), cex = labelsize, xpd = TRUE)
text(1.1, 1.02, expression(paste("now marginal borrower")), cex = labelsize, xpd = TRUE)
Arrows(1.1, 0.98, 1.1, 0.93, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(1.25, 0.5, expression(paste("BRF of")), cex = labelsize)
text(1.25, 0.44, expression(paste("wealthy borrower")), cex = labelsize)
Arrows(1.25, 0.53, 1.25, 0.6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


text(0.6, 0.91, expression(paste("BRF of previously")), cex = labelsize)
text(0.6, 0.85, expression(paste("marginal borrower")), cex = labelsize)
Arrows(0.6, 0.82, 0.6, 0.69, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

# text(0.12, 0.44, expression(paste("zpc")), cex = labelsize)
# text(0.12, 0.38, expression(paste("after")), cex = labelsize)
# Arrows(0.17, 0.43, 0.27, 0.43, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
#text(0.12, 0.26, expression(paste("zpc")), cex = labelsize)
#text(0.12, 0.2, expression(paste("before")), cex = labelsize)
#Arrows(0.17, 0.25, 0.3, 0.25, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#text(0.49, 0.15, expression(paste("Policy")), cex = labelsize)
#text(0.49, 0.09, expression(paste("Change")), cex = labelsize)
#Arrows(0.58, 0.05, 0.42, 0.05, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
Arrows(0.4, 0.4, 0.33, 0.4, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


dev.off()
