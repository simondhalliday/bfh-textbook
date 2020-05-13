require(shape)
pdf(file = "credit/credit_mb_mc.pdf", width = 7, height = 7)

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
grays <- gray.colors(25, start = 1, end = 0)

par(mar =  c(5, 6, 1, 1))

mc <- function(f, q = 2) {
  -q + 2*q*f
}

xlims <- c(0, 1.15)
ylims <- c(0, 3)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs = "i", 
     yaxs = "i")

ticksy <- c(-2, 0, mc(f = 0.65), mc(f = 0.85), 2, ylims[2])
ylabels <- c(expression(paste(-q)), 0, expression(paste(delta^L)), expression(paste(delta^N)), expression(paste(delta == q)), NA)
ticksx <- c(0, 0.5, 0.65, 0.85, 1, xlims[2])
xlabels <- c(NA, 0.5, expression(paste(f*(delta^{L}) )), expression(paste(f*(delta^{N}) )), 1.0, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Lines for mrs graph
lines(xx1, mc(xx1, q = 2), col = COLA[4], lwd = graphlinewidth)

#Label axes
#mtext(expression(paste("Speed of the machine, ", f)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.18, 0.5*(ylims[2] + ylims[1]), expression(paste("Interest factor, ", delta )), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(0.5*(xlims[2]), -2.2, expression(paste("Speed of the machine, ", f)), xpd = TRUE, cex = axislabelsize) 

text(0.9, 2.4, expression(paste(mc == -q*(1 - 2*f) )), cex = labelsize)
text(0.25, 2.1, expression(paste(mb == q, phantom() == delta )), cex = labelsize)
text(0.25, 1.5, expression(paste(mb^N == delta^N )), cex = labelsize)
text(0.25, 0.7, expression(paste(mb^L == delta^L )), cex = labelsize)

segments(0.65, 0, 0.65, mc(f = 0.65), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, mc(f = 0.65), xlims[2], mc(f = 0.65), lty = 1, col = COLB[4] , lwd = segmentlinewidth)
segments(0.85, 0, 0.85, mc(f = 0.85), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, mc(f = 0.85), xlims[2], mc(f = 0.85), lty = 1, col = COLB[4] , lwd = segmentlinewidth)
segments(1, 0, 1, mc(f = 1), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, mc(f = 1), xlims[2], mc(f = 1), lty = 1, col = COLB[4] , lwd = segmentlinewidth)

points(1, mc(1), pch = 16, col = "black", cex = 1.5)
points(0.85, mc(0.85), pch = 16, col = "black", cex = 1.5)
points(0.65, mc(0.65), pch = 16, col = "black", cex = 1.5)

text(1 + 0.025, mc(1) - 0.05, expression(paste(c)), cex = labelsize)
text(0.85 + 0.025, mc(0.85) - 0.05, expression(paste(n)), cex = labelsize)
text(0.65 + 0.025, mc(0.65) - 0.05, expression(paste(b)), cex = labelsize)

dev.off()
