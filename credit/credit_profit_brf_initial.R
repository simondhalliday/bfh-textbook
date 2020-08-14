#' Graph Designer: Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "credit/credit_profit_brf_initial.pdf", width = 8, height = 6)

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
par(mar =  c(4, 5, 1, 1))

brfFn <- function(delta, q = 15, k = 0) {
  .5 + (delta / (2 * q)) * (1 - k)
}


PCFn <- function(delta, q = 1) {
  delta/q
}

isoprofitFn <- function(delta, rho = 0.05, b = 0.5) {
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

xlims <- c(0, 10)
ylims <- c(0, 1)

npts <- 501 
d1 <- seq(xlims[1], xlims[2], length.out = npts)
f1 <- seq(ylims[1], ylims[2], length.out = npts) 
deltalevel <- c(2.5, 4.75)

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


# ticksy <- c(ylims[1], 0.5, ylims[2] - 0.1)
# ylabels <- c(NA, expression(paste(f == frac(1,2))), 1)
ticksy <- c(ylims[1], 0.5,  brfFn(deltalevel[2], k = 0), 1, ylims[2])
ylabels <- c(NA, NA,  expression(paste(f[1]^N)), 1, NA)
#ticksx <- c(xlims[1], (1 + 0.1)/(1 - 0), (1 + 0.1)/(1 - 0.5), xlims[2])
ticksx <- c(xlims[1], deltalevel[2], xlims[2])
xlabels <- c(NA, NA, NA)

text((1 + 0.1)/(1 - 0.3), - 0.09, expression(paste(frac(1 + rho, 1 - b))), xpd = TRUE, cex = labelsize) 

#text((1 + 0.1)/(1 - 0.5), - 0.09, expression(paste(frac(1 + rho, 1 - b))), xpd = TRUE, cex = labelsize) 
text(-0.4, 0.46, expression(paste(frac(1,2))), xpd = TRUE, cex = labelsize) 

text(c(deltalevel[2]), -.07, c(expression(paste(delta[1]^N))), xpd = TRUE, cex = labelsize)  

npts <- 503 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)


# Polygon
# COLD <- c("#deebf7", "#9ecae1")
# xpoly <- seq(from = (1 + 0.1)/(1 - 0), to = xlims[2], length.out = 500)
# ypoly <- isoprofitFn(xpoly, b = 0)
# polygon(x = xpoly, y = ypoly, col = COLD[1], density = NULL, border = NA)
# 
# xpoly1 <- c((1 + 0.1)/(1 - 0), xlims[2], xlims[2])
# ypoly1 <- c(0, 0, isoprofitFn(xlims[2], b = 0))
# polygon(x = xpoly1, y = ypoly1, col = COLD[1], density = NULL, border = NA)
# 
# xpoly2 <- seq(from = (1 + 0.1)/(1 - 0.5), to = xlims[2], length.out = 500)
# ypoly2 <- isoprofitFn(xpoly2, b = 0.5)
# polygon(x = xpoly2, y = ypoly2, col = COLD[2], density = NULL, border = NA)
# 
# xpoly3 <- c((1 + 0.1)/(1 - 0.5), xlims[2], xlims[2])
# ypoly3 <- c(0, 0, isoprofitFn(xlims[2], b = 0.5))
# polygon(x = xpoly3, y = ypoly3, col = COLD[2], density = NULL, border = NA)

#segments(0.25, 0, xlims[2], isoprofitFn(xlims[2], b = 0.5), col = COLD[2])

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)



#Draw the graphs
# lines(xx1, brfFn(xx1, k = 0.28), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfFn(xx1, k = 0), col = COLA[4], lwd = graphlinewidth)
# lines(xx1, brfFn(xx1, k = 0.75), col = COLA[4], lwd = graphlinewidth)
lines(xx1, isoprofitFn(xx1, b = 0.35), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, isoprofitFn(xx1, b = 0), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, isoprofitFn(xx1, pi = 0.5), col = COLB[4], lwd = graphlinewidth)


#Axis labels
#mtext(expression(paste("Interest factor, ", delta)), side = 1, line = 3.3, cex = axislabelsize)
text(0.5*(xlims[2]), - 0.14 , expression(paste("Interest factor, ", delta)), xpd = TRUE, cex = axislabelsize) 
text(-1, 0.5*(ylims[2]), expression(paste("Probability of failure (risk), ", f)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#segments(0.39, -1, 0.39, brfFn(0.39, k = 0.75), lty = 2, col = grays[20] , lwd = segmentlinewidth)
#segments(0, brfFn(deltalevel[1], k = 0), deltalevel[1], brfFn(deltalevel[1], k = 0), lty = 2, col = grays[20] , lwd = segmentlinewidth)
#segments(deltalevel[1], 0, deltalevel[1], brfFn(deltalevel[1], k = 0), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(deltalevel[2], 0, deltalevel[2], brfFn(deltalevel[2], k = 0), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, brfFn(deltalevel[2], k = 0), deltalevel[2], brfFn(deltalevel[2], k = 0), lty = 2, col = grays[20] , lwd = segmentlinewidth)

# points(deltalevel[1], brfFn(deltalevel[1], k = 0), pch = 16, col = "black", cex = 1.5)
# text(deltalevel[1]+0.25, brfFn(deltalevel[1], k = 0) - 0.03, expression(paste(n[0])), cex = labelsize)

points(deltalevel[2], brfFn(deltalevel[2], k = 0), pch = 16, col = "black", cex = 1.5)
text(deltalevel[2] +0.25, brfFn(deltalevel[2], k = 0) - 0.03, expression(paste(n[1])), cex = labelsize)

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
# text(1.35, 0.925, expression(paste(hat(pi)[0]^{t==0} == 1 + rho[0])), cex = labelsize, xpd = TRUE)
# text(1.35, 0.75, expression(paste(hat(pi)[0]^{t==1} == 1 + rho[1])), cex = labelsize, xpd = TRUE)

#text(0.95, 0.4, expression(paste(hat(pi)[0]({b==0}) )), cex = labelsize, xpd = TRUE)
text(3.6, 0.4, expression(paste(hat(pi)[1]({b>0}) )), cex = labelsize, xpd = TRUE)


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

# text(4.7, 0.7, expression(paste("Barriers")), cex = labelsize)
# text(4.7, 0.65, expression(paste("decrease")), cex = labelsize)
# text(4.6, 0.7, expression(paste("Barriers")), cex = labelsize)
# text(4.6, 0.65, expression(paste("increase")), cex = labelsize)

#Arrows(5.5, 0.62, 3.2, 0.62, col = "black", code = 1, lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#Labels for lenders entering and leaving
# text(2, 0.91, expression(paste("Lenders")), cex = labelsize)
# text(2, 0.85, expression(paste("leaving")), cex = labelsize)
# 
# text(8, 0.36, expression(paste("Lenders")), cex = labelsize)
# text(8, 0.3, expression(paste("entering")), cex = labelsize)

text(9, 0.72, expression(paste("Best-response")), cex = labelsize, xpd = TRUE)
text(9, 0.67, expression(paste("function")), cex = labelsize, xpd = TRUE)
#text(9, 0.5, expression(paste(f == frac(1, 2) + frac(delta, 2*q))), cex = labelsize)


#text(8.5, 0.93, expression(paste("Credit market") ), cex = labelsize, xpd = TRUE)
text(1.3, 0.33, expression(paste("Competition") ), cex = labelsize, xpd = TRUE)
text(1.3, 0.28, expression(paste("condition") ), cex = labelsize, xpd = TRUE)
text(1.3, 0.23, expression(paste(b > 0) ), cex = labelsize, xpd = TRUE)

# text(0.225, 0.45, expression(paste("Competition")), cex = labelsize)
# text(0.225, 0.39, expression(paste("condition")), cex = labelsize)
# text(0.225, 0.34, expression(paste(b > 0)), cex = labelsize)


dev.off()