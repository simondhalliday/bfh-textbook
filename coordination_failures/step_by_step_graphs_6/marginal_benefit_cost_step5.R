#Graph Designer: Simon Halliday & Madeleine Wettach '20
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "coordination_failures/step_by_step_graphs_6/marginal_benefit_cost_step5.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 3

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 5, 4, 4))

MBenefit <- function(eb = 8, alpha = 16, beta = 1/24) {
  alpha*(1 - beta*eb)
}

MCost <- function(ea, slope = 1, intercept = 0){
  intercept + slope*ea
}


xlims <- c(0, 14)
ylims <- c(0, 14)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(12, 46.08, 90)

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

ticksy <- c(0, 4, 8, 12, ylims[2])
ylabels <- c(NA, expression(paste(u[e^A1]^A)), expression(paste(u[e^A2]^A)), expression(paste(u[e^A3]^A)), NA)
ticksx <- c(0, 4, 8, 12, xlims[2])
xlabels <- c(NA, expression(paste(e[1]^A)), expression(paste(e[2]^A)), expression(paste(e[3]^A)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)


mtext(expression(paste("Aram's effort, ", e^A)), side=1, line = 2.5, cex = axislabelsize)
text(-1.4, 0.5*ylims[2], expression(paste("Aram's marginal utility and disutility, ", u[e^A]^A)), xpd = TRUE, cex = axislabelsize, srt = 90) 

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(4, 8, length.out = npts)

#lines(xx1, MBenefit(xx1, ua = 10), col = COLA[4], lwd = graphlinewidth)
lines(xx1, MCost(xx1), col = COLA[4], lwd = graphlinewidth)
#lines(xx1, indiffA(xx1, ua = 46.08), col = COLA[4], lwd = graphlinewidth)
#lines(xx2, mrsA(xx2), col = "gray", lwd = graphlinewidth, lty = 2)


segments(0, 12, xlims[2], 12, lty = 2, col =  COLB[3] , lwd = segmentlinewidth)
segments(0, 8, xlims[2], 8, lty = 1, col = COLB[3] , lwd = segmentlinewidth)
segments(0, 4, xlims[2], 4, lty = 2, col = COLB[3] , lwd = segmentlinewidth)

# segments(9.6, 0, 9.6, 9.6, lty = 2, col = "gray" , lwd = segmentlinewidth)
# 
# segments(0, 6.9, 6.9, 6.9, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(6.9, 0, 6.9, 6.9, lty = 2, col = "gray" , lwd = segmentlinewidth)


#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
# text(9.5, 30, expression(paste("Slope at ", (list(e^A, y^A)) == (list(6, 28)), " is ", e^A == 6)))

text(3, 8.5, expression(paste("Marginal Benefit, ", mb[2]^A== alpha(1 - beta*e^B))), cex = labelsize)
text(0.7, 12.5, expression(paste(mb[3]^A)), cex = labelsize)
text(0.7, 4.5, expression(paste(mb[1]^A)), cex = labelsize)
text(10.8, 13, expression(paste("Marginal Distutility", phantom() == e^A)), cex = labelsize)

Arrows(1.5, 9, 1.5, 11.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(3, 11, expression(paste(alpha, " increases, or ")), cex = labelsize)
text(3, 10.5, expression(paste(e^B, " decreases")), cex = labelsize)

Arrows(1.5, 7.5, 1.5, 4.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(3, 5.5, expression(paste(alpha, " decreases, or ")), cex = labelsize)
text(3, 5, expression(paste(e^B, " increases")), cex = labelsize)

segments(8, 0, 8, 8, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(8, 8, pch = 16, col = "black", cex = 1.5)
text(8.25, 7.5, expression(paste("g")), cex = labelsize)

segments(12, 0, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(12, 12, pch = 16, col = "black", cex = 1.5)
text(12.25, 11.5, expression(paste("h")), cex = labelsize)

segments(4, 0, 4, 4, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(4, 4, pch = 16, col = "black", cex = 1.5)
text(4.25, 3.5, expression(paste("f")), cex = labelsize)

#Label the iso-welfare functions for the HG, Aisha
##text(9.7, 60, expression(u[1]^A))
##text(7.6, 60, expression(u[2]^A))
##text(4.8, 60, expression(u[3]^A))



dev.off()
