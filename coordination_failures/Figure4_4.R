#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
require(shape)
pdf(file = "/Users/rileyboeth/Library/Mobile Documents/com~apple~CloudDocs/bfh textbook - backup/coordination_failures/figure4_4.pdf", width = 10, height = 8)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 10, 4, 4))

#Concave utility of wealth function

ConvexU <- function(x,y){
  (-(1600-(x)^2)^(1/2))+40
}

#Add limits on axes and levels of utility for each indifference curve
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

#Axis labels and draw linear utility function
mtext(expression(paste("The Value of Wealth, v(y)")), side = 1, line = 2.5, cex = axislabelsize)
text(-9, 0.5*ylims[2], expression(paste("Wealth, y")), xpd = TRUE, cex = axislabelsize, srt = 90) 

lines(xx1, ConvexU(xx1, y), col = COLA[5], lwd = graphlinewidth)

#Add line from pt a to pt c

segments(10, ConvexU(10), 39, ConvexU(39), lty = 1, col = COLA[6] , lwd = graphlinewidth)


#Label 5 points on line

text(10, ConvexU(10)+1, expression(paste("a")), cex = labelsize)
segments(10, 0, 10, ConvexU(10), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, ConvexU(10), 10, ConvexU(10), lty = 2, col = "gray", lwd = segmentlinewidth)
points(10, ConvexU(10), pch = 16, col = "black", cex = 1.5)


text(33, ConvexU(33)+1, expression(paste("c")), cex = labelsize)
segments(33, 0, 33, ConvexU(33), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, ConvexU(33), 33, ConvexU(33), lty = 2, col = "gray", lwd = segmentlinewidth)
points(33, ConvexU(33), pch = 16, col = "black", cex = 1.5)

text(25.5, ConvexU(33)+1, expression(paste("e")), cex = labelsize)
segments(25.6, 0, 25.6, ConvexU(33), lty = 2, col = "gray", lwd = segmentlinewidth)
points(25.6, ConvexU(33), pch = 16, col = "black", cex = 1.5)

text(25.6, ConvexU(25.7)+1.1, expression(paste("b")), cex = labelsize)
segments(25.6, 0, 25.6, ConvexU(13), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, ConvexU(25.55), 25.6, ConvexU(25.55), lty = 2, col = "gray", lwd = segmentlinewidth)
points(25.6, ConvexU(25.55), pch = 16, col = "black", cex = 1.5)

text(38.5, ConvexU(39)+1, expression(paste("d")), cex = labelsize)
segments(39, 0, 39, ConvexU(39), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, ConvexU(39), 39, ConvexU(39), lty = 2, col = "gray", lwd = segmentlinewidth)
points(39, ConvexU(39), pch = 16, col = "black", cex = 1.5)

#Label y-sub,x-sub,etc. on axes

text(39, -.9, expression(paste("y + ", Delta, "y"[1])), xpd = TRUE, cex = labelsize)
text(33, -.9, expression(paste("y"[ce])),  xpd = TRUE,  cex = labelsize)
text(10, -.9, expression(paste("y - ",Delta,"y"[2])),  xpd = TRUE,  cex = labelsize)
text(25.6, -.9, expression(paste("y")),  xpd = TRUE,  cex = labelsize)


text(-2.7, ConvexU(39), expression(paste("v(y + ",Delta,"y)")), xpd = TRUE, cex = labelsize)
text(-1.95, ConvexU(33)+1.7, expression(paste("v(ce) = ")),  xpd = TRUE, cex = labelsize)
text(-2.8, ConvexU(33), expression(paste("Pv(y- ",Delta,"y) + ")),  xpd = TRUE, cex = labelsize)
text(-4.2, ConvexU(33)-1.5, expression(paste("(1 - P)(v)(y + ",Delta,"y)")),  xpd = TRUE, cex = labelsize)
text(-1.8, ConvexU(33)-3, expression(paste(" = v(y)")),  xpd = TRUE, cex = labelsize)
text(-2.7, ConvexU(10), expression(paste("v(y - ",Delta,"y)")),  xpd = TRUE,  cex = labelsize)
text(-1.5, ConvexU(25.6), expression(paste("v(y)")),  xpd = TRUE,  cex = labelsize)

#Add risk premium distance arrow and label

Arrows(26.2, ConvexU(13), 32.2, ConvexU(13), col = "black", lty = 1, lwd = 2, code = 3, arr.type = "triangle", arr.lwd = 0.5)
text((26.2+32.2)/2, ConvexU(13)+1.5, expression(paste("Risk Premium")),  xpd = TRUE,  cex = labelsize)


dev.off()