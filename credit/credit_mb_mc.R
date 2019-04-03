require(shape)
pdf(file = "credit/credit_mb_mc.pdf", width = 7, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(5, 5, 1, 1))

mc <- function(f, q = 2) {
  -q + 2*q*f
}


xlims <- c(0, 1.15)
ylims <- c(0, 3)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(46.08, 55, 64)
b <- c(46.08, 55, 64)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
# ticksx <- seq(from = 0, to = xlims[2], by = 2)
# xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(-2, 0, mc(f = 0.65), mc(f = 0.85), 2, ylims[2])
ylabels <- c(expression(paste(-q)), 0, expression(paste(delta^L)), expression(paste(delta^N)), expression(paste(delta == q)), NA)
ticksx <- c(0, 0.5, 0.65, 0.85, 1, xlims[2])
xlabels <- c(NA, 0.5, expression(paste(f*(delta^{L}) )), expression(paste(f*(delta^{N}) )), 1.0, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Lines for mrs graph
lines(xx1, mc(xx1, q = 2), col = COLA[4], lwd = graphlinewidth)
#lines(xx1, mrpL(xx1, pmax = 15), col = COLA[4], lty = 2, lwd = segmentlinewidth)

#Label axes
#mtext(expression(paste("Speed of the machine, ", f)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.12, 0.5*(ylims[2] + ylims[1]), expression(paste("Interest factor, ", delta )), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(0.5*(xlims[2]), -2.2, expression(paste("Speed of the machine, ", f)), xpd = TRUE, cex = axislabelsize) 

#Arrows(18, 4, 18, 6.5, col = "black", lty = 1, code = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


text(0.95, 2.4, expression(paste(mc == -q*(1 - 2*f) )), cex = labelsize)
text(0.25, 1.5, expression(paste(mb^N == delta^N )), cex = labelsize)
text(0.25, 0.7, expression(paste(mb^L == delta^L )), cex = labelsize)
#text(19, 5, expression(paste(q[1] == frac(w[1],e))), cex = labelsize)
#text(16, 12.6, expression(paste("marginal cost")), cex = labelsize)
#text(16, 12, expression(paste("of labor")), cex = labelsize)
#Arrows(16, 11.6, 16, 4.4, col = "black", lty = 1, code = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

Arrows(2, 9, 6.8, 9, col = "black", lty = 1, code = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(3.5, 10, expression(paste("Increase in ", frac(dy, dl))), cex = labelsize)

text(9.3, 12, expression(paste(frac(dy, dl)==phantom())), cex = labelsize)
text(11, 12.3, expression(paste("marginal")), cex = labelsize)
text(11, 11.7, expression(paste("benefit")), cex = labelsize)
Arrows(11, 11.3, 11, 2.4, col = "black", lty = 1, code = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

segments(0.65, 0, 0.65, mc(f = 0.65), lty = 2, col = COLB[4] , lwd = segmentlinewidth)
segments(0, mc(f = 0.65), 0.65, mc(f = 0.65), lty = 2, col = COLB[4] , lwd = segmentlinewidth)
segments(0.85, 0, 0.85, mc(f = 0.85), lty = 2, col = COLB[4] , lwd = segmentlinewidth)
segments(0, mc(f = 0.85), 0.85, mc(f = 0.85), lty = 2, col = COLB[4] , lwd = segmentlinewidth)
segments(1, 0, 1, mc(f = 1), lty = 2, col = COLB[4] , lwd = segmentlinewidth)
segments(0, mc(f = 1), 1, mc(f = 1), lty = 2, col = COLB[4] , lwd = segmentlinewidth)

points(1, mc(1), pch = 16, col = "black", cex = 1.5)
points(0.85, mc(0.85), pch = 16, col = "black", cex = 1.5)
points(0.65, mc(0.65), pch = 16, col = "black", cex = 1.5)

text(1 + 0.025, mc(1) - 0.05, expression(paste(c)), cex = labelsize)
text(0.85 + 0.025, mc(0.85)- 0.05, expression(paste(n)), cex = labelsize)
text(0.65 + 0.025, mc(0.65)- 0.05, expression(paste(b)), cex = labelsize)



# text(80, 12.5, expression(paste("Excess Supply at ", p^H)), cex = labelsize)
# 
# Arrows(62, 5.5, 82, 5.5, col = "black", lty = 1, code = 3, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# text(72, 4.75, expression(paste("Excess")), cex = labelsize)
# text(72, 3.75, expression(paste("Demand")), cex = labelsize)
# text(72, 2.75, expression(paste("at ", p^L)), cex = labelsize)



#Label Demand
text(13, 1, expression(paste(mb[1])), cex = labelsize)
text(19.5, 1, expression(paste(mb[2])), cex = labelsize)

dev.off()
