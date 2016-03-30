#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
require(shape)
pdf(file = "/Users/rileyboeth/Library/Mobile Documents/com~apple~CloudDocs/bfh textbook - backup/coordination_failures/figure4_8.pdf", width = 10, height = 8)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 8, 4, 4))

#proportion of wealth functions

MUb <- function(x,y){
  x
}

MUa <- function(x,y){
  40-x
}


#Add limits on axes and levels of utility for each function 
ylims <- c(0, 40)
xlims <- c(0, 40)

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

#x and y limits with plain axes without ticks/numbers to match previous graph

ticksx <- seq(from = 0, to = xlims[2]+1, by = 41)
xlabels <- seq(from = 0, to = xlims[2], by = 1)
ticksy <- seq(from = 0, to = ylims[2]+1, by = 41)
ylabels <- seq(from = 0, to = ylims[2], by = 10)

axis(1,at = ticksx,  pos = 0, labels = FALSE)
axis(2,at = ticksy,  pos = 0, labels = FALSE, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Axis labels and draw linear utility functions
mtext(expression(paste("A's Share of Wealth")), side = 1, line = 2, cex = axislabelsize)
text(-2, 0.5*ylims[2], expression(paste("Utility")), xpd = TRUE, cex = axislabelsize, srt = 90) 

lines(xx1, MUb(xx1, y), col = COLA[6], lwd = graphlinewidth)
lines(xx1, MUa(xx1, y), col = COLA[5], lwd = graphlinewidth)

#Label points on line

text(20, 20+1, expression(paste("a")), cex = labelsize)
segments(20, 0, 20, 20, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, 20, 20, 20, lty = 2, col = "gray", lwd = segmentlinewidth)
points(20, 20, pch = 16, col = "black", cex = 1.5)

text(33, 33+1.2, expression(paste("b")), cex = labelsize)
segments(33, 33, 0, 33, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(33, 0, 33, 33, lty = 2, col = "gray", lwd = segmentlinewidth)
points(33, 33, pch = 16, col = "black", cex = 1.5)

text(33, 7+1.2, expression(paste("c")), cex = labelsize)
segments(33, 7, 0, 7, lty = 2, col = "gray", lwd = segmentlinewidth)
points(33, 7, pch = 16, col = "black", cex = 1.5)


#Label relevant points on axes

text(20, -.9, expression(paste("a*")), xpd = TRUE, cex = labelsize)
text(33, -.9, expression(paste("a"[x])), xpd = TRUE, cex = labelsize)

text(-3, 33, expression(paste("-MU(B)"+"(a)"[x])),  xpd = TRUE, cex = labelsize)
text(-3, 7, expression(paste("MU(A)"+"(a)"[x])),  xpd = TRUE, cex = labelsize)

dev.off()