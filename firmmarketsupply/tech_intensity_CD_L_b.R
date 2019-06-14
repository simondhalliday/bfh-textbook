#Graph Designer: Simon Halliday / Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "firmmarketsupply/tech_intensity_CD_L_b.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 5, 4, 4))

isoquant <- function(l, alpha = 0.5, x = 5, A = 1) {
  (x / (A*l^alpha))^(1/(1 - alpha))
}

xlims <- c(0, 12)
ylims <- c(0, 12)

npts <- 501 


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

ticksy <- c(0, 4, 8, ylims[2])
ylabels <- c(NA, expression(paste(k[1])), expression(paste(k[2])), NA)
ticksx <- c(0, 2, 4, xlims[2])
xlabels <- c(NA, expression(paste(l[1])), expression(paste(l[2])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

# Isoquant
lines(xx1, isoquant(xx1, x = 5, alpha = 0.5, A = 1.77), col = COLB[4], lwd = graphlinewidth)
lines(xx1, isoquant(xx1, x = 5, alpha = 0.5, A = 0.885), col = COLB[5], lwd = graphlinewidth)

# dash diagonal
segments(0, 0, 6, ylims[2], lty = 2, col = "black", lwd = segmentlinewidth)

# Grey segments
segments(0, 4, 2, 4, lty = 2, col = "grey", lwd = segmentlinewidth)
segments(2, 0, 2, 4, lty = 2, col = "grey", lwd = segmentlinewidth)

segments(0, 8, 4, 8, lty = 2, col = "grey", lwd = segmentlinewidth)
segments(4, 0, 4, 8, lty = 2, col = "grey", lwd = segmentlinewidth)


# Leontief
segments(2, 4, xlims[2], 4, lty = 1, col = COLA[4], lwd = graphlinewidth)
segments(2, 4, 2, ylims[2], lty = 1, col = COLA[4],  lwd = graphlinewidth)

segments(4, 8, xlims[2], 8, lty = 1, col = COLA[5], lwd = graphlinewidth)
segments(4, 8, 4, ylims[2], lty = 1, col = COLA[5],  lwd = graphlinewidth)


# Axis 
mtext(expression(paste("Hours of labor, ", l)), side=1, line = 2.5, cex = axislabelsize)
text(-1, 0.5*ylims[2], expression(paste("Amount of Capital, ", k)), xpd = TRUE, cex = axislabelsize, srt = 90) 

# Points
points(2, 4, pch = 16, col = "black", cex = 1.5)
text(2.25, 4.25, expression(i))

points(4, 8, pch = 16, col = "black", cex = 1.5)
text(4.25, 8.25, expression(e))

dev.off()