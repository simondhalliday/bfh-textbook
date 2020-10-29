#' Graph Designer: Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "credit/credit_investment_interest_a.pdf", width = 8, height = 6)

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
par(mar =  c(5, 6, 0.1, 1))

brfFn <- function(delta, q = 1, k = 1) {
  .5 + (delta / (2 * q)) * (1 - k)
}

PCFn <- function(delta, q = 1) {
  delta/q
}

isoreturnFn <- function(delta, b = 0.5, rho = 0.05) {
  1 - ((1 + rho)/(1 - b))*(1/delta)
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

xlims <- c(0, 12)
ylims <- c(0, 1)

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


ticksy <- c(ylims[1], ylims[2] - 0.1)
ylabels <- c(NA, 1)
ticksx <- c(xlims[1], 2.1, 2.8, xlims[2])
xlabels <- c(NA, expression(paste(frac(1 + rho[1], 1 - b))), expression(paste(frac(1 + rho[2], 1 - b))), NA)


npts <- 503 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)


# Polygon
COLD <- c("#deebf7", "#9ecae1")
xpoly <- seq(from = 0.175, to = xlims[2], length.out = 500)
ypoly <- isoreturnFn(xpoly)
polygon(x = xpoly, y = ypoly, col = COLD[1], density = NULL, border = NA)

xpoly1 <- c(2.1, xlims[2], xlims[2])
ypoly1 <- c(0, 0, isoreturnFn(xlims[2]))
polygon(x = xpoly1, y = ypoly1, col = COLD[1], density = NULL, border = NA)

xpoly2 <- seq(from = 2.8, to = xlims[2], length.out = 500)
ypoly2 <- isoreturnFn(xpoly2, rho = 0.4)
polygon(x = xpoly2, y = ypoly2, col = COLD[2], density = NULL, border = NA)

xpoly3 <- c(2.8, xlims[2], xlims[2])
ypoly3 <- c(0, 0, isoreturnFn(xlims[2], rho = 0.4))
polygon(x = xpoly3, y = ypoly3, col = COLD[2], density = NULL, border = NA)

#segments(0.25, 0, xlims[2], isoreturnFn(xlims[2], rho = 0.4), col = COLD[2])

axis(1, at = ticksx, pos = 0, labels = NA, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)


#Draw the graphs
# lines(xx1, brfFn(xx1, k = 0.28), col = COLA[4], lwd = graphlinewidth)
# lines(xx1, brfFn(xx1, k = 0.5), col = COLA[4], lwd = graphlinewidth)
lines(xx1, isoreturnFn(xx1, rho = 0.05), col = COLB[4], lwd = graphlinewidth)
lines(xx1, isoreturnFn(xx1, rho = 0.4), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, isoreturnFn(xx1, pi = 0.5), col = COLB[4], lwd = graphlinewidth)


#Axis labels
#mtext(expression(paste("Interest factor, ", delta)), side = 1, line = 3.3, cex = axislabelsize)
text(0.5*(xlims[2]), -0.16, expression(paste("Interest factor, ", delta)), xpd = TRUE, cex = axislabelsize) 
text(-1, 0.5*(ylims[2]), expression(paste("Probability of failure (risk), ", f)), xpd = TRUE, cex = axislabelsize, srt = 90) 

text(x = c(xlims[1], 1.65, 3.1, xlims[2]), 
     par("usr")[3] - 0.01, 
     labels = xlabels, 
     srt = 0, pos = 1, xpd = TRUE, 
     cex = labelsize)

#segments(0.39, -1, 0.39, brfFn(0.39, k = 0.75), lty = 2, col = grays[20] , lwd = segmentlinewidth)
#points(0.39, brfFn(0.39, k = 0.75), pch = 16, col = "black", cex = 1.5)
#text(0.39 + 0.025, brfFn(0.39, k = 0.75) - 0.03, expression(paste(b)), cex = labelsize)

#segments(1, -1, 1, brfFn(delta = 1, k = 0.5), lty = 2, col = grays[20] , lwd = segmentlinewidth)
#points(1, brfFn(1, k = 0.5), pch = 16, col = "black", cex = 1.5)
# text(1, brfFn(1, k = 0.5) + 0.04, expression(paste(n)), cex = labelsize)

#segments(.59, -1, 0.59, brfFn(delta = 0.59, k = 0.75), lty = 2, col = grays[20] , lwd = segmentlinewidth)
#points(0.59, brfFn(0.59, k = 0.75), pch = 16, col = "black", cex = 1.5)
#text(0.59 + 0.025, brfFn(0.59, k = 0.75) - 0.03, expression(paste(a)), cex = labelsize)

#segments(.7, -1, 0.7, brfFn(delta = 0.7, k = 0.28), lty = 2, col = grays[20] , lwd = segmentlinewidth)
#points(0.7, brfFn(0.7, k = 0.28), pch = 16, col = "black", cex = 1.5)

# ZPC
text(11.5, 0.86, expression(paste(hat(pi)[rho[1]]) ), cex = labelsize, xpd = TRUE)
text(11.5, 0.71, expression(paste(hat(pi)[rho[2]] )), cex = labelsize, xpd = TRUE)

# text(1.1, 1.08, expression(paste("BRF of previously excluded")), cex = labelsize, xpd = TRUE)
# text(1.1, 1.02, expression(paste("now marginal borrower")), cex = labelsize, xpd = TRUE)
# Arrows(1.1, 0.98, 1.1, 0.92, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# 
# text(1.1, 0.5, expression(paste("BRF of")), cex = labelsize)
# text(1.1, 0.44, expression(paste("wealthy borrower")), cex = labelsize)
# Arrows(1.1, 0.53, 1.1, 0.6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# 
# 
# text(0.6, 0.91, expression(paste("BRF of previously")), cex = labelsize)
# text(0.6, 0.85, expression(paste("marginal borrower")), cex = labelsize)
# Arrows(0.6, 0.82, 0.6, 0.68, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#text(0.49, 0.15, expression(paste("Policy")), cex = labelsize)
#text(0.49, 0.09, expression(paste("Change")), cex = labelsize)
Arrows(7.2, 0.62, 5.8, 0.62, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


dev.off()