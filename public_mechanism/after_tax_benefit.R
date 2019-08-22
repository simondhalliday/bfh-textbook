#Graph Designer: Simon Halliday + Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "public_mechanism/after_tax_benefit.pdf", width = 7, height = 9)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 6, 4, 6))

# Post Tax Benefit

delta_PT <- function(delta = -0.1, f){
  f*(4*delta - 1) + 1/2 - 2*delta
}


#Add limits on axes and levels of utility for each function 
ylims <- c(-0.8, 0.8)
xlims <- c(0, 1)

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

#x and y limits with plain axes without ticks/numbers to match previous graph; y axes on both sides

ticksy <- c(ylims[1], 0, delta_PT(delta = -0.1, f = 0), ylims[2])
ylabels <- c(NA, 0, expression(paste(frac(1,2) - 2, delta)), NA)
ticksx <- c(xlims[1], 0.5, xlims[2])
xlabels <- c(NA, expression(paste(0.5)), NA)
ticksy2 <- c(ylims[1], delta_PT(delta = -0.1, f = 1), 0, ylims[2])
#ylabels2 <- c(NA,expression( paste(4, delta, - bgroup("(", 2 *  delta + frac(1, 2), ")" ))), 0, NA)
ylabels2 <- c(NA,expression( paste(2, delta, -  frac(1, 2))), 0, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)
axis(4, at = ticksy2, pos = 1, labels = ylabels2, las = 1)

npts <- 503 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)


#Axis labels 
# text(0.5, -.9, expression(paste("x")), xpd = TRUE, cex = axislabelsize, srt = 0) 
# text(xlims[1] - 0.1, 0, expression(paste("y")), xpd = TRUE, cex = axislabelsize, srt = 90) 

# Private Return
lines(xx1, delta_PT(f = xx1), col = COLA[4], lwd = graphlinewidth)

# Text Labels
text(0.2, - 0.1, expression(paste("Greens buy")), cex = labelsize)
text(0.2, - 0.15, expression(paste("Blue's houses")), cex = labelsize)

text(0.8, 0.15, expression(paste("Greens sell")), cex = labelsize)
text(0.8, 0.1, expression(paste("houses to Blues")), cex = labelsize)

text(0.25, 0.5, expression(paste(Delta^"PT")), cex = labelsize)

#Axis arrow
arrows(0, 0, 0.25, 0, code = 2, length = 0.1, lwd = 0.75*axislabelsize, lty = 1)
arrows(0.75, 0, 1, 0, code = 1, length = 0.1, lwd = 0.75*axislabelsize, lty = 1)
dev.off()