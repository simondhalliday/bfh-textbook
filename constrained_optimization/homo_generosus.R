#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "bfh-textbook/constrained_optimization/constrained_optimization_hg.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5

indiffcurveA1 <- function(x, v = 10, lambda = 1) {
  (v - 4) - lambda * x
}

indiffcurveA2 <- function(x, v = 10, lambda = 1) {
  v - lambda * x
}

indiffcurveA3 <- function(x, v = 10, lambda = 1) {
  (v + 4) - lambda * x
}


COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
par(mar =  c(6, 5, 4, 4))
xlims <- c(0, 15)
ylims <- c(0, 15)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = "",
     ylab = "", 
     line = 3,
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")


npts <- 500 
npts2 <- 501

#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Draw the lines for the graphs
lines(xx1, indiffcurveA1(xx1), col = COLA[2], lwd = 4)
lines(xx1, indiffcurveA2(xx1), col = COLA[2], lwd = 4)
lines(xx1, indiffcurveA3(xx1), col = COLA[2], lwd = 4)

#Customize ticks and labels for the plot
ticksy <- c(ylims[1], 4, 6, 8, 10, 14, ylims[2])
ylabels <- c(NA, 4, expression(paste(v[1]^A == u[1]^A)), 8, expression(paste(v[2]^A == u[2]^A)), expression(paste(v[3]^A == u[3]^A)), NA)
ticksx <- c(xlims[1], 2, 10 - 4, 10, 10 + 4, xlims[2])
xlabels <- c(NA, 2, expression(paste(v[1]^A == u[1]^B)==6), expression(paste(v[2]^A == u[2]^B)), expression(paste(v[3]^A == u[3]^B)), NA)
# 
# ticksy <- seq(from = ylims[1], to = ylims[2], by = 2)
# ylabels <- seq(from = ylims[1], to = ylims[2], by = 2)
# ticksx <- seq(from = xlims[1], to = xlims[2], by = 2)
# xlabels <- seq(from = xlims[1], to = xlims[2], by = 2)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)


mtext(expression(paste("B's Utility, ", u^B)), side = 1, line = 2.5, cex = axislabelsize)
text(-1.7, 0.5*(ylims[2]), expression(paste("A's Utility, ", u^A)), xpd = TRUE, cex = axislabelsize, srt = 90) 



#Add arrows:
#arrows(-0.9, 10.5, -0.9, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
#arrows(6.2, -1.6, 9, -1.6, xpd = TRUE, length=0.1,angle=40,lwd=3)

#Annotation of the three graphs
text(4.5, .6, expression(v[1]^A))
text(10, .6, expression(v[2]^A))
text(14, .6, expression(v[3]^A))

#Lines to label 2 points
segments(0, 8, 2, 8, lty = 2, col = "darkgray", lwd = 2)
segments(2, 0, 2, 10, lty = 2, col = "darkgray", lwd = 2)
segments(0, 4, 6, 4, lty = 2, col = "darkgray", lwd = 2)
segments(6, 0, 6, 10, lty = 2, col = "darkgray", lwd = 2)
segments(0, 10, 6, 10, lty = 2, col = "darkgray", lwd = 2)

#Annotate 4 points
#c and d
points(2, 8, pch = 16, col = "black", cex = 1.5)
text(2.4, 8.4, expression(c))
points(6, 4, pch = 16, col = "black", cex = 1.5)
text(6.4, 4.4, expression(d))

#f and g
points(2, 10, pch = 16, col = "black", cex = 1.5)
text(2.4, 10.4, expression(f))
points(6, 10, pch = 16, col = "black", cex = 1.5)
text(6.4, 10.4, expression(g))

#Mention lambda
text(12, 12.5, expression("A's iso-value curves"), cex = axislabelsize)
text(12, 11.5, expression(paste("for ", lambda == 1)), cex = axislabelsize)




dev.off()

