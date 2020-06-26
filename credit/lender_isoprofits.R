#Graph Designer: Harriet Brookes-Gray
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "credit/lender_isoprofits.pdf", width = 8, height = 6)

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

brfFn <- function(delta, q = 1) {
  .5 + (delta / (2 * q)) 
}

PCFn <- function(delta, q = 1) {
  delta/q
}

isoreturnFn <- function(delta, pi=0.125) {
  1 - (pi)/delta
}

deltaFn <- function(f, pi=0.125) {
  pi/ (1 - f)
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

xlims <- c(0, 0.6)
ylims <- c(0, 1.05)

npts <- 501 
d1 <- seq(xlims[1], xlims[2], length.out = npts)
f1 <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(0.0625, 0.125, 0.14)

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


ticksy <- c(ylims[1], isoreturnFn(delta = deltaFn(f = 0.5), pi = 0.075), isoreturnFn(delta = deltaFn(f = 0.5), pi = 0.125), isoreturnFn(delta = deltaFn(f = 0.5), pi = 0.175), ylims[2])
ylabels <- c(NA, expression(paste(f[i])), expression(paste(f^C)), expression(paste(f[k])),NA)
ticksx <- c(xlims[1], deltaFn(f = 0.5, pi = 0.075), deltaFn(f = 0.5), deltaFn(f = 0.5, pi = 0.175), xlims[2])
xlabels <- c(NA, expression(paste(delta[e])),  expression(paste(delta^C)), expression(paste(delta[j])), NA)


npts <- 503 
xx1 <- seq(xlims[1], 0.8, length.out = npts)
xx2 <- seq(0, 1, length.out = npts)

# xpoly1 <- seq(from = 0.168, to = 0.5, length.out = 500)
# ypoly1 <- ylow(xpoly1)
# ypoly2 <- isoreturnFn(xpoly1)
# polygon(x = c(xpoly1, rev(xpoly1)), y = c(ypoly1, rev(ypoly2)), col = COL[4], density = NULL, border = NA)


#Draw the graphs
#lines(xx1, brfFn(xx1), col = COLA[4], lwd = graphlinewidth)
#lines(xx1, PCFn(xx1), col = COLA[2], lwd = graphlinewidth)
# lines(xx1, isoreturnFn(xx1), col = COLB[4], lwd = graphlinewidth)
# lines(xx1, isoreturnFn(xx1 - 0.2), col = COLB[4], lwd = graphlinewidth)
# lines(0, isoreturnFn(xx2 - 0.4), col = COLB[4], lwd = graphlinewidth)
xx1 <- seq(0.01, xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(1.5, 2.5, length.out = npts)
xx4 <- seq(3, 5, length.out = npts)
xx5 <- seq(7, 9, length.out = npts)

xpoly <- seq(from = 0.075, to = xlims[2], length.out = 500)
ypoly <- isoreturnFn(xpoly, pi = 0.075)
polygon(x = xpoly, y = ypoly, col = COLB[1], density = NULL, border = NA)

xpoly1 <- c(0.075, xlims[2], xlims[2])
ypoly1 <- c(0, 0, isoreturnFn(xlims[2], pi = 0.075))
polygon(x = xpoly1, y = ypoly1, col = COLB[1], density = NULL, border = NA)

segments(0.175, 0, xlims[2], isoreturnFn(xlims[2], pi = 0.175), col = COLB[1])
#Draw the graphs
#lines(xx1, PCFn(xx1), col = COLA[4], lwd = graphlinewidth)
lines(xx1, isoreturnFn(xx1), col = COLB[4], lwd = graphlinewidth)
lines(xx1, isoreturnFn(xx1, pi = 0.075), col = COLB[4], lwd = graphlinewidth)
lines(xx1, isoreturnFn(xx1, pi = 0.175), col = COLB[4], lwd = graphlinewidth)

#Axis labels
mtext(expression(paste("Interest factor, ", delta)), side = 1, line = 3.5, cex = axislabelsize)
text(-0.06, 0.5*(ylims[2]), expression(paste("Probability of failure (risk), ", f)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#contour(d1, f1,
      #  outer(d1, f1, yFn),
      #  drawlabels = FALSE,
      # col = COLA[3],
      #  lwd = graphlinewidth,
      #  levels = a,
      # xaxs = "i",
      # yaxs = "i",
      # add = TRUE)


#segments(0.5, 0, 0.5, brfFn(delta = 0.5), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, isoreturnFn(delta = 0.25, pi = 0.125), deltaFn(f = 0.5, pi = 0.175), isoreturnFn(delta = 0.25, pi = 0.125), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(deltaFn(f = 0.5), 0, deltaFn(f = 0.5), isoreturnFn(delta = 0.25, pi = 0.075) , lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(deltaFn(f = 0.5, pi = 0.175), 0, deltaFn(f = 0.5, pi = 0.175), 0.5, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(deltaFn(f = 0.5, pi = 0.075), 0, deltaFn(f = 0.5, pi = 0.075), 0.5, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, isoreturnFn(delta = deltaFn(f = 0.5), pi = 0.175), deltaFn(f = 0.5), isoreturnFn(delta = deltaFn(f = 0.5), pi = 0.175), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, isoreturnFn(delta = deltaFn(f = 0.5), pi = 0.075), deltaFn(f = 0.5), isoreturnFn(delta = deltaFn(f = 0.5), pi = 0.075), lty = 2, col = grays[20] , lwd = segmentlinewidth)


#Label 3 points on the same vertical delta^C
points(0.25, isoreturnFn(0.25), pch = 16, col = "black", cex = 1.5)
text(0.25 - 0.01, isoreturnFn(0.25) + 0.03, expression(paste(c)), cex = labelsize)

points(0.25, isoreturnFn(0.25, pi = 0.075), pch = 16, col = "black", cex = 1.5)
text(0.25 - 0.01, isoreturnFn(0.25, pi = 0.075) + 0.03, expression(paste(i)), cex = labelsize)

points(0.25, isoreturnFn(0.25, pi = 0.175), pch = 16, col = "black", cex = 1.5)
text(0.25 - 0.01, isoreturnFn(0.25, pi = 0.175) + 0.03, expression(paste(k)), cex = labelsize)

#label 2 points on the same horizontal as f^C
#Use deltaFn to find the x coordinate
points(deltaFn(f = 0.5, pi = 0.075), 0.5, pch = 16, col = "black", cex = 1.5)
text(deltaFn(f = 0.5, pi = 0.075) - 0.01, 0.5 + 0.03, expression(paste(e)), cex = labelsize)

points(deltaFn(f = 0.5, pi = 0.175), 0.5, pch = 16, col = "black", cex = 1.5)
text(deltaFn(f = 0.5, pi = 0.175) - 0.01, 0.5 + 0.03, expression(paste(j)), cex = labelsize)


# text(0.23, 0.55, expression(paste(e)), cex = labelsize, xpd = TRUE)
# text(0.35, 0.55, expression(paste(k)), cex = labelsize, xpd = TRUE)
# text(0.48, 0.55, expression(paste(j)), cex = labelsize, xpd = TRUE)
# text(0.48, 0.67, expression(paste(i)), cex = labelsize, xpd = TRUE)
# text(0.48, 0.8, expression(paste(h)), cex = labelsize, xpd = TRUE)

#text(-0.05, 0.5, expression(paste(f[j])), cex = labelsize, xpd = TRUE)
#text(-0.05, 0.67, expression(paste(f[i])), cex = labelsize, xpd = TRUE)
#text(-0.05, 0.8, expression(paste(f[h])), cex = labelsize, xpd = TRUE)

text(0.84, 0.88, expression(paste(hat(pi)[0] == zpc)), cex = labelsize, xpd = TRUE)
text(0.84, 0.78, expression(paste(hat(pi)[1])), cex = labelsize, xpd = TRUE)
text(0.84, 0.7, expression(paste(hat(pi)[2])), cex = labelsize, xpd = TRUE)

Arrows(0.36, 0.45, 0.46, 0.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
#text(0.48, 0.3, expression(paste('More profits')), cex = labelsize, xpd = TRUE)
text(0.46, 0.25, expression(paste('Better for lender')), cex = labelsize, xpd = TRUE)

text(0.53, 0.97, expression(paste("Iso-expected")), cex = labelsize)
text(0.53, 0.9, expression(paste("profits")), cex = labelsize)
text(0.53, 0.82, expression(paste(hat(pi)[0])), cex = labelsize)
text(0.53, 0.72, expression(paste(hat(pi)[1])), cex = labelsize)
text(0.53, 0.62, expression(paste(hat(pi)[2])), cex = labelsize)



#Annotate points (4,4),(2,8),(8,2) on feasibility frontier
#text(0.5 + 0.015, isoreturnFn(0.5) + 0.04, expression(paste(n)), cex = labelsize)
#points(0.5, isoreturnFn(0.5), pch = 16, col = "black", cex = 1.5)

#text(0.375 + 0.02, 0.6 + 0.03, expression(paste(b)), cex = labelsize)
#points(0.5, brfFn(delta = 0.5), pch = 16, col = "black", cex = 1.5)
#points(0.25, 0.5, pch = 16, col = "black", cex = 1.5)
#points(0.165, 0.25, pch = 16, col = "black", cex = 1.5)



#text(0.62, 1.05, expression(paste("A's best-response function")), cex = labelsize, xpd = TRUE)
#text(0.62, 0.95, expression(paste(f == frac(1,2) + frac(delta, 2*q))), cex = labelsize)

#text(0.64, 0.4, expression(paste("A's participation")), cex = labelsize)
#text(0.64, 0.34, expression(paste("constraint")), cex = labelsize)
#text(0.64, 0.22, expression(paste(f == frac(delta, q))), cex = labelsize)
#Arrows(0.64, 0.44, 0.64, 0.58, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


text(0.35, 0.98, expression(paste("Slope ", phantom() == frac((1 - f),delta ))), cex = labelsize, xpd = TRUE)
Arrows(0.35, 0.93, 0.35, 0.82, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#text(0.3, 0.13, expression(paste("Pareto-improving")), cex = labelsize)
#text(0.3, 0.07, expression(paste("lens")), cex = labelsize)
#Arrows(0.3, 0.18, 0.3, 0.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)


dev.off()
