#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(pBrackets)
pdf(file = "indmarketdemand/segregation_initial.pdf", width = 8, height = 6)

#Set parameters for graphics
namesize <- 1.3
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
Grays <- gray.colors(25, start =1, end = 0)

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 8.1, 1, 6.2))

GreenPrice <- function(f, delta = 0.1, p = 0.1) {
  (1/2)*(f - delta)  - (1/2)*(f - delta)^2 + p 
}

BluePrice <- function(f, delta = 0.1, p = 0.1) {
  (1/2)*(f + delta)  - (1/2)*(f + delta)^2 + p
}

xlims <- c(0, 1)
ylims <- c(0, 0.3)

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
ticksy <- c(ylims[1], GreenPrice(f = 0), GreenPrice(f = 0.6), ylims[2])
ylabels <- c(NA, expression(paste(p^G*(g == 0))), expression(paste(bar(p)^G)), NA)
ticksx <- c(xlims[1], 0.6, xlims[2])
xlabels <- c(0, expression(paste(g[h] == 0.6 )), 1)
ticksy2 <- c(ylims[1], GreenPrice(f = 1), ylims[2])
ylabels2 <- c(NA, expression(paste(p^G*(g == 1))), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)
axis(4, at = ticksy2, pos = 1, labels = ylabels2, las = 1, cex.axis = labelsize)

npts <- 503 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(xlims[1], 0.6, length.out = npts)
xx3 <- seq(0.6, xlims[2], length.out = npts)

# #Polygon 1
# xpoly1 <- c(xx2, 0.6, 0, 0)
# ypoly1 <- c(GreenPrice(xx2), 0, 0, GreenPrice(0))
# polygon(x = xpoly1, y = ypoly1, col=COLA[1], density=NULL, border = NA)

#Polygon 2
# xpoly2 <- c(xx3, 1, 1, 0.6)
# ypoly2 <- c(GreenPrice(xx3), GreenPrice(1), 0, 0)
# polygon(x = xpoly2, y = ypoly2, col=COLB[1], density=NULL, border = NA)


#Draw the graphs
lines(xx1, GreenPrice(xx1), col = COLA[4], lwd = graphlinewidth)
#lines(xx1, BluePrice(xx1), col = COLB[4], lwd = graphlinewidth)

#Axis labels
#mtext(expression(paste("Fraction of Greens in the neighborhood, ", f)), side = 1, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], -0.05,  expression(paste("Fraction of Greens in the neighborhood, ", g)), xpd = TRUE, cex = axislabelsize) 
text(-0.27, 0.5*(ylims[2]), expression(paste("Greens' willingness to pay, ", p^G)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Segments and labeled points on the graphs
# segments(0.5, 0, 0.5, GreenPrice(f = 0.5), lty = 2, col = grays[20] , lwd = segmentlinewidth)
# points(0.5, GreenPrice(f = 0.5), pch = 16, col = "black", cex = 1.5)
# text(0.5, GreenPrice(f = 0.5) + 0.01, expression(paste(e)), cex = labelsize)
# 
# segments(0.4, 0, 0.4, BluePrice(f = 0.4), lty = 2, col = grays[20] , lwd = segmentlinewidth)
# points(0.4, BluePrice(f = 0.4), pch = 16, col = "black", cex = 1.5)
# points(0.4, GreenPrice(f = 0.4), pch = 16, col = "black", cex = 1.5)
# text(0.4-0.02, BluePrice(f = 0.4) - 0.0075, expression(paste(a)), cex = labelsize)
# text(0.4+0.02, GreenPrice(f = 0.4) - 0.005, expression(paste(b)), cex = labelsize)
# 

segments(0.45, GreenPrice(f = 0.6), 0.75, GreenPrice(f = 0.6), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0.6, 0, 0.6, GreenPrice(f = 0.6), lty = 2, col = grays[20] , lwd = segmentlinewidth)
points(0.6, GreenPrice(f = 0.6), pch = 16, col = "black", cex = 1.5)
#text(0.6 - 0.02, BluePrice(f = 0.6) - 0.005, expression(paste(d)), cex = labelsize)
text(0.6 + 0.02, GreenPrice(f = 0.6) - 0.0075, expression(paste(h)), cex = labelsize)

#Label graphs
text(0.9, GreenPrice(f = 0.9) + 0.02, expression(paste(p^G)), cex = labelsize)

#Explain the dynamics
text(0.3, 0.3, expression(paste("increasing")), cex = labelsize, xpd = TRUE)
text(0.3, 0.28, expression(paste("willingness to pay")), cex = labelsize)
text(0.3, 0.26, expression(paste("as ", g %->% 0.6)), cex = labelsize)
#Arrows(0.05, 0.24, 0.55, 0.24, col = "black", code = 2, lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(0.78, 0.3, expression(paste("decreasing")), cex = labelsize, xpd = TRUE)
text(0.78, 0.28, expression(paste("willingness to pay")), cex = labelsize)
text(0.78, 0.26, expression(paste("as ", g %->% 1)), cex = labelsize)

brackets(0.02, 0.24, 0.58, 0.24, h = 0.01,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = segmentlinewidth, lty = 1, xpd = TRUE)

brackets(0.62, 0.24, 0.98, 0.24, h = 0.01, ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = segmentlinewidth, lty = 1, xpd = TRUE)

#Arrows(0.65, 0.24, 0.95, 0.24, col = "black", code = 2, lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


dev.off()
