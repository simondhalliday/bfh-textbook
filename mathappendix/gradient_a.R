#Graph Designer: Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(pBrackets)
pdf(file = "mathappendix/gradient_a.pdf", width = 8, height = 6)
par(mar = c(3, 3, 0.75, 1))
#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

xlims <- c(0, 45)
ylims <- c(0, 8)

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
     xaxs="i", 
     yaxs="i"
)



ticksx <- c(xlims[1], 9, 16, 25, 36, xlims[2])
xlabels <- c(NA, 9, 16, 25, 36, expression(x))
ticksy <- c(ylims[1], 3, 4, 5, 6, ylims[2])
ylabels <- c(NA, 3, 4, 5, 6, expression(y))

axis(1,at = ticksx,  pos = 0, labels = xlabels, cex.axis = axislabelsize)
axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1, cex.axis = axislabelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <-seq(xlims[1], xlims[2] - 4, length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)

#Axis labels
#mtext(expression(paste("", x)), side = 1, line = 2.5, cex = axislabelsize)
#text(-4, 0.5*ylims[2], expression(paste("", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#equations:
sqrt <- function(x){
  x^(1/2)
}

# lines
lines(xx2, sqrt(xx2), col = COLB[4], lwd = graphlinewidth)

# segments
segments(9, 0, 9, 3, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(36, 0, 36, 6, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, 3, 36, 3, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(25, 3, 25, 5, lty = 2, col = "gray", lwd = segmentlinewidth)

segments(9, 3, 25, 5, lty = 1, col = COLA[4], lwd = segmentlinewidth)
segments(9, 3, 36, 6, lty = 1, col = COLA[5], lwd = segmentlinewidth)

# points
points(9, 3, pch = 16, col = "black", cex = 1.5)
points(25, 5, pch = 16, col = "black", cex = 1.5)
points(36, 6, pch = 16, col = "black", cex = 1.5)
points(25, 3, pch = 16, col = "black", cex = 1.5)
points(36, 3, pch = 16, col = "black", cex = 1.5)

# brackets
brackets(36.5, 5.9, 36.5, 3.1, h = NULL, ticks = 0.5, curvature = 0.5, type = 1, col = 1, lwd = 1, lty = 1, xpd = FALSE)
#brackets(25.5, 4.9, 25.5, 3.1, h = NULL, ticks = 0.5, curvature = 0.5, type = 1, col = 1, lwd = 1, lty = 1, xpd = FALSE)
brackets(35.5, 2.8, 9.5, 2.8, h = .4, ticks = 0.5, curvature = 0.5, type = 1, col = 1, lwd = 1, lty = 1, xpd = FALSE)
#brackets(25.5, 4.9, 25.5, 3.1, h = NULL, ticks = 0.5, curvature = 0.5, type = 1, col = 1, lwd = 1, lty = 1, xpd = FALSE)


# label function
text(44, 6.5, expression(paste("y = ", sqrt(x))),  xpd = TRUE, cex = labelsize)

text(22.5, 2.0, expression(paste(Delta, x[ae])),  xpd = TRUE, cex = labelsize)
text(39, 4.5, expression(paste(Delta, y[ce])),  xpd = TRUE, cex = labelsize)

text(9, 3.25, expression(paste(a)),  xpd = TRUE, cex = labelsize)
text(25, 5.25, expression(paste(b)),  xpd = TRUE, cex = labelsize)
text(36, 6.25, expression(paste(c)),  xpd = TRUE, cex = labelsize)
text(24.5, 3.25, expression(paste(d)),  xpd = TRUE, cex = labelsize)
text(35.5, 3.25, expression(paste(e)),  xpd = TRUE, cex = labelsize)


dev.off()

