#Graph Designer: Simon Halliday & Madeleine Wettach '20
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics
library(shape)
pdf(file = "risk/step_by_step_graphs_4/palanpur_risk_dom_step4.pdf", width = 10, height = 8)

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

expected_late <- function(x){
  20 + 0.25*x
}

expected_early <- function(x){
  x
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

#x and y limits with plain axes without ticks/numbers to match previous graph; y axes on both sides

ticksx <- c(0, 20, 26.67, 40)
xlabels <- c(0, expression(paste(p == 1/2)),expression(paste(p == 2/3)), 1)
ticksy <- c(ylims[1], 20, expected_late(20), expected_late(26.67), ylims[2])
ylabels <- c(NA, 2, 2.5, 2.67, NA)
ticksy2 <- c(ylims[1], 30, 40, ylims[2])
ylabels2 <- seq(from = 0, to = ylims[2], by = 10)


axis(1, at = ticksx,  pos = 0, labels = xlabels)
axis(2, at = ticksy,  pos = 0, labels = ylabels, las = 1)
axis(4, at = ticksy2, pos = xlims[2], labels = FALSE,las=1)


npts <- 503 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Axis labels and draw linear utility functions
text(xlims[2]*0.5, -3.5, expression(paste("Fraction planting early, p")), xpd = TRUE, cex = axislabelsize)
text(-3.5, 0.5*ylims[2], expression(paste("Expected payoff")), xpd = TRUE, cex = axislabelsize, srt = 90) 


text(41, ylims[2], expression(paste(4)), xpd = TRUE, cex = labelsize) 
text(41, 0.75*ylims[2], expression(paste(3)), xpd = TRUE, cex = labelsize) 

lines(xx1, expected_early(xx1), col = COLB[4], lwd = graphlinewidth)
lines(xx1, expected_late(xx1), col = COLA[5], lwd = graphlinewidth)

#Label points on line

##text(20 - 0.5, expected_late(20) - 1.2, expression(paste("f")), cex = labelsize)
##segments(20, 0, 20, expected_late(20), lty = 2, col = "gray", lwd = segmentlinewidth)
##segments(0, expected_late(20), 20, expected_late(20), lty = 2, col = "gray", lwd = segmentlinewidth)
##points(20, expected_late(20), pch = 16, col = "black", cex = 1.5)

##text(26.67 + 0.7, expected_early(26.67) - 0.7, expression(paste("g")), cex = labelsize)
##segments(26.67, expected_early(26.67), 0, expected_early(26.67), lty = 2, col = "gray", lwd = segmentlinewidth)
##segments(26.67, 0, expected_early(26.67), 26.67, lty = 2, col = "gray", lwd = segmentlinewidth)
##points(26.67, expected_early(26.67), pch = 16, col = "black", cex = 1.5)

# text(33 + 0.7, 7 + 0.7, expression(paste("h")), cex = labelsize)
# segments(33, 7, 0, 7, lty = 2, col = "gray", lwd = segmentlinewidth)
# points(33, 7, pch = 16, col = "black", cex = 1.5)


#Label relevant points on axes
text(8, 23.5, expression(paste("Plant Late")), xpd = TRUE, cex = labelsize)
text(8, 11, expression(paste("Plant Early")),  xpd = TRUE, cex = labelsize)

#Axis arrow
arrows(28, -3.5, 35, -3.5, xpd = TRUE, length=0.1,angle=40,lwd=3)

#Risk Dominant Strategy
##text(17, 36.5, expression(paste("Strategy with higher")), xpd = TRUE, cex = labelsize)
##text(17, 35, expression(paste("expected payoff at ", p == 1/2)), xpd = TRUE, cex = labelsize)
##text(17, 33.5, expression(paste("is risk-dominant")), xpd = TRUE, cex = labelsize)
##Arrows(20, 32.5, 20, expected_late(20) + 1.5, col = "black", lty = 1, lwd = 2, code = 2, arr.type = "triangle", arr.lwd = 0.5)

#Equal expected payoffs
##text(29, 35, expression(paste("Equal expected")), xpd = TRUE, cex = labelsize)
##text(29, 33.5, expression(paste("payoffs at ", p == 2/3)), xpd = TRUE, cex = labelsize)
##Arrows(26.67, 32.5, 26.67, expected_late(26.67) + 1.5, col = "black", lty = 1, lwd = 2, code = 2, arr.type = "triangle", arr.lwd = 0.5)


dev.off()