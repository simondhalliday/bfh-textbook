#Graph Designer: Simon Halliday + Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "public_mechanism/housing_pre_tax_benefit.pdf", width = 7, height = 7)

# Set parameters for graphics
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
par(mar =  c(1, 4, 1, 4))

# Post Tax Benefit

delta_P <- function(delta = 0.1, f){
  2*delta*(2*f - 1) 
}


#Add limits on axes and levels of utility for each function 
ylims <- c(-0.3, 0.3)
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

ticksy <- c(ylims[1], delta_P(delta = 0.1, f = 0), 0,  ylims[2]) 
ylabels <- c(NA, expression(paste(-delta)), 0, NA)
ticksx <- c(xlims[1], 0.5, xlims[2])
xlabels <- c(NA, NA, NA)
ticksy2 <- c(ylims[1], delta_P(delta = 0.1, f = 1), 0, ylims[2])
#ylabels2 <- c(NA,expression( paste(4, delta, - bgroup("(", 2 *  delta + frac(1, 2), ")" ))), 0, NA)
ylabels2 <- c(NA,expression( paste(delta)), 0, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)
axis(4, at = ticksy2, pos = 1, labels = ylabels2, las = 1, cex.axis = labelsize)

npts <- 503 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)


#Axis labels 
text(0.5, -.6, expression(paste("Proportion of greens, ", f)), xpd = TRUE, cex = axislabelsize, srt = 0) 
text(xlims[1] - 0.1, 0, expression(paste("Private gain from exchange, ", Delta^P)), xpd = TRUE, cex = axislabelsize, srt = 90) 

# Private Return
lines(xx1, delta_P(f = xx1), col = COLA[4], lwd = graphlinewidth)

# Text Labels
text(0.2, - 0.02, expression(paste("Greens sell")), cex = labelsize)
text(0.2, - 0.04, expression(paste("houses to Blues")), cex = labelsize)

text(0.8, 0.04, expression(paste("Greens buy")), cex = labelsize)
text(0.8, 0.02, expression(paste("houses from Blues")), cex = labelsize)

text(0.65, 0.14, expression(paste(Delta^P == delta*(2*f - 1))), cex = labelsize)

text(0.02, -0.0175, paste(0), cex = labelsize)
text(0.5, -0.0175, paste(0.5), cex = labelsize)
text(0.98, -0.0175, paste(1), cex = labelsize)

#Axis arrow
arrows(0.5, 0, 0.25, 0, code = 2, length = 0.1, lwd = 0.75*axislabelsize, lty = 1)
arrows(0.5, 0, 0.75, 0, code = 2, length = 0.1, lwd = 0.75*axislabelsize, lty = 1)

dev.off()