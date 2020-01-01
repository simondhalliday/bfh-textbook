#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(pBrackets)
pdf(file = "public_mechanism/segregation.pdf", width = 8, height = 6)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 2
segmentlinewidth <-1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 6.5, 4, 4.5))

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
ticksy <- c(ylims[1], GreenPrice(f = 0), BluePrice(f = 0), ylims[2])
ylabels <- c(NA, expression(paste(p^G*(f == 0))), expression(paste(p^B*(f == 0))), NA)
ticksx <- c(xlims[1], 0.4, 0.5, 0.7, xlims[2])
xlabels <- c(NA, expression(paste(f == 0.4 )), expression(paste(f,"*", phantom()==0.5 )), expression(paste(f == 0.7 )), NA)
ticksy2 <- c(ylims[1], BluePrice(f = 1), GreenPrice(f = 1), ylims[2])
ylabels2 <- c(NA, expression(paste(p^B*(f == 1))), expression(paste(p^G*(f == 1))), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)
axis(4, at = ticksy2, pos = 1, labels = ylabels2, las = 1)

npts <- 503 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Draw the graphs
lines(xx1, GreenPrice(xx1), col = COLA[4], lwd = graphlinewidth)
lines(xx1, BluePrice(xx1), col = COLB[4], lwd = graphlinewidth)

#Axis labels
mtext(expression(paste("Proportion of Greens in the neighborhood, ", f)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.18, 0.5*(ylims[2]), expression(paste("Price or Willingness to Pay, ", list(p^G, p^B))), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Segments and labeled points on the graphs
segments(0.5, 0, 0.5, GreenPrice(f = 0.5), lty = 2, col = "gray" , lwd = segmentlinewidth)
points(0.5, GreenPrice(f = 0.5), pch = 16, col = "black", cex = 1.5)
text(0.5, GreenPrice(f = 0.5) + 0.01, expression(paste(e)), cex = labelsize)

segments(0.4, 0, 0.4, BluePrice(f = 0.4), lty = 2, col = "gray" , lwd = segmentlinewidth)
points(0.4, BluePrice(f = 0.4), pch = 16, col = "black", cex = 1.5)
points(0.4, GreenPrice(f = 0.4), pch = 16, col = "black", cex = 1.5)
text(0.4-0.02, BluePrice(f = 0.4) - 0.0075, expression(paste(a)), cex = labelsize)
text(0.4+0.02, GreenPrice(f = 0.4) - 0.005, expression(paste(b)), cex = labelsize)


segments(0.7, 0, 0.7, GreenPrice(f = 0.7), lty = 2, col = "gray" , lwd = segmentlinewidth)
points(0.7, BluePrice(f = 0.7), pch = 16, col = "black", cex = 1.5)
points(0.7, GreenPrice(f = 0.7), pch = 16, col = "black", cex = 1.5)
text(0.7-0.02, BluePrice(f = 0.7) - 0.005, expression(paste(d)), cex = labelsize)
text(0.7+0.02, GreenPrice(f = 0.7) - 0.0075, expression(paste(c)), cex = labelsize)

#Label graphs
text(0.1, BluePrice(f = 0.1) + 0.01, expression(paste(p^B)), cex = labelsize)
text(0.9, GreenPrice(f = 0.9) + 0.015, expression(paste(p^G)), cex = labelsize)

#Explain the dynamics
text(0.25, 0.27, expression(paste("Greens Sell to Blues")), cex = labelsize)
text(0.25, 0.25, expression(paste("because ", p^B > p^G, " so ", f %->% 0)), cex = labelsize)
#Arrows(0.03, 0.24, 0.47, 0.24, col = "black", code = 1, lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(0.75, 0.27, expression(paste("Blues Sell to Greens")), cex = labelsize)
text(0.75, 0.25, expression(paste("because ", p^G > p^B, " so ", f %->% 1)), cex = labelsize)
#Arrows(0.53, 0.24, 0.97, 0.24, col = "black", code = 2, lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

brackets(0.53, 0.23, 0.97, 0.23, h = 0.01,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = segmentlinewidth, lty = 1, xpd = TRUE)

brackets(0.03, 0.23, 0.47, 0.23, h = 0.01, ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd =segmentlinewidth, lty = 1, xpd = TRUE)

dev.off()
