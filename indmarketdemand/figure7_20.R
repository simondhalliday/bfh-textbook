#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
require(shape)
pdf(file = "/Users/rileyboeth/Library/Mobile Documents/com~apple~CloudDocs/bfh textbook - backup/indmarketdemand/figure7_20.pdf", width = 10, height = 8)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 5, 4, 4))

#Original budget constraint and pivoted budget constraint

bc1 <- function(x,y) {
  40 - .5*x
}

bc2 <- function(x, y) {
  40 - .345*x
}


#Compensated budget constraint parallel to bc1

cbc1 <- function(x, y) {
  48.8 - .5*x
}

#Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 40)
xlims <- c(0, 80)

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

ticksx <- seq(from = 0, to = xlims[2]+1, by = 81)
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

#contour(x, y,
 #       drawlabels = FALSE,
  #      col = COLB[3],
   #     lwd = graphlinewidth,
    #    levels = a, 
     #   xaxs="i", 
      #  yaxs="i", 
       # add = TRUE)

#Utility functions 

segments(56.8, bc2(56.8), 56.8, ylims[2], lty = 1, col = COLA[2] , lwd = graphlinewidth)
segments(56.8, bc2(56.8), xlims[2], bc2(56.8), lty = 1, col = COLA[2] , lwd = graphlinewidth)

segments(49, bc1(49), 49, ylims[2], lty = 1, col = COLA[2] , lwd = graphlinewidth)
segments(49, bc1(49), xlims[2], bc1(49), lty = 1, col = COLA[2] , lwd = graphlinewidth)


#Axis labels
mtext(expression(paste("Quantity of knives (x)")), side = 1, line = 2.5, cex = axislabelsize)
text(-5.5, 0.5*ylims[2], expression(paste("Money of forks (y)")), xpd = TRUE, cex = axislabelsize, srt = 90) 

lines(xx1, bc1(xx1, y), col = COLA[5], lwd = graphlinewidth)
lines(xx1, bc2(xx1, y), col = COLA[5], lwd = graphlinewidth)
lines(xx1, cbc1(xx1, y), col = COLA[6], lwd = graphlinewidth)


#Label curves

text(78, bc1(49)+1, expression("u"[1]), cex = labelsize)
text(78, bc2(56.8)+1, expression("u"[2]), cex = labelsize)
text(78, bc1(78)+1.3, expression("bc"[1]), cex = labelsize)
text(78, bc2(78)+1.2, expression("bc"[2]), cex = labelsize)
text(78, cbc1(78)+1.4, expression("cbc"[1]), cex = labelsize)

#Label points e-sub, e, e'

text(60.8, bc2(56.8)+.7, expression(paste(" = e"[sub])), cex = labelsize)

text(57.8, bc2(56.8)+.8, expression(paste("e")), cex = labelsize)
segments(56.8, 0, 56.8, bc2(x = 56.8), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, bc2(x = 56.8), 56, bc2(x = 56.8), lty = 2, col = "gray", lwd = segmentlinewidth)
points(56.8, bc2(x = 56.8), pch = 16, col = "black", cex = 1.5)

text(50.2, bc1(49)+.8, expression(paste("e'")), cex = labelsize)
segments(49, 0, 49, bc1(49), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, bc1(49), 49, bc1(49), lty = 2, col = "gray", lwd = segmentlinewidth)
points(49, bc1(49), pch = 16, col = "black", cex = 1.5)

#Label y-sub,x-sub,etc. on axes

text(59, -.9, expression(paste(" = x"[sub])), xpd = TRUE, cex = labelsize)
text(49, -.9, expression(paste("x"["e'"])),  xpd = TRUE,  cex = labelsize)
text(56, -.9, expression(paste("x"[e])),  xpd = TRUE, cex = labelsize)

text(-2, bc1(39)+.5, expression(paste("y"[sub])), xpd = TRUE, cex = labelsize)
text(-2, bc1(39)-1, expression(paste(" = y"[e])),  xpd = TRUE, cex = labelsize)
text(-2, bc1(49)+.5, expression(paste("y"["e'"])),  xpd = TRUE,  cex = labelsize)


dev.off()