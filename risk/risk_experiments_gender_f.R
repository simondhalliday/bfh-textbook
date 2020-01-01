require(shape)
require(plotrix)

pdf(file = "risk/risk_experiments_gender_f.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2
a <- c(2, 4, 6)


COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(5, 5, 1, 1))
xlims <- c(0, 60)
ylims <- c(15, 28)


indiffA <- function(g, intercept = 3, slope = 0.125){
  intercept  + slope*g^2
}


#Plot command 
plot(0, 0, 
     xlim = xlims, 
     ylim = ylims, 
     type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     axes = FALSE,
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i"
)


#Customize ticks and labels for the plot
# ticksy <- seq(ylims[1], ylims[2], 3)
# ylabels <- seq(ylims[1], ylims[2], 3)
# ticksx <- seq(xlims[1], xlims[2], 5)
# xlabels <- seq(xlims[1], xlims[2], 5)
ticksy <- c(ylims[1], 18, 20, 22, 24, 26, ylims[2])
ylabels <- c(NA, 18, 20, 22, 24, 26, NA)
ticksx <- c(0, 12, 24, 36, 48, 56,  xlims[2])
xlabels <- c(NA, 12, 24, 36, 48, 56, NA)

axis(1, at = ticksx, pos = ylims[1], labels = xlabels)
axis(2, at = ticksy, pos = xlims[1], labels = ylabels, las = 1)
mtext(expression(paste("Difference in wealth (good versus bad outcome), ", Delta, ", risk")), side = 1, line = 2.5, cex = axislabelsize)
text(xlims[1] - 5.5, ylims[2] - 0.5*(ylims[2] - ylims[1]), expression(paste("Expected payoff, ", hat(y) )), xpd = TRUE, cex = axislabelsize, srt = 90) 

axis.break(axis = 2, bgcol="white", breakcol="black",
           style="slash", brw=0.01)



npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
#lines(xx1, riskreturn(xx1), col = COLA[4], lwd = graphlinewidth, lty = 1)
lines(xx1, indiffA(xx1, intercept = 20, slope = 0.0035), col = COLB[4], lwd = graphlinewidth, lty = 1)
#Low
lines(xx1, indiffA(xx1, intercept = 16, slope = 0.0035), col = COLB[4], lwd = graphlinewidth, lty = 1)
#high
lines(xx1, indiffA(xx1, intercept = 24, slope = 0.0035), col = COLB[4], lwd = graphlinewidth, lty = 1)


#lines(xx1, indiffA(xx1, intercept = 21, slope = 0.00235), col = COLB[4], lwd = graphlinewidth, lty = 1)
# lines(xx1, indiffA(xx1, intercept = 5.6), col = COLB[4], lwd = graphlinewidth, lty = 1)
# lines(xx1, indiffA(xx1, intercept = 9), col = COLB[4], lwd = graphlinewidth, lty = 1)

#Setting up the segments for the choices 
segments(0, 18, 12, 20, lty = 1, col = COLA[4], lwd = segmentlinewidth)
segments(12, 20, 24, 22, lty = 1, col = COLA[4], lwd = segmentlinewidth)
segments(24, 22, 36, 24, lty = 1, col = COLA[4], lwd = segmentlinewidth)
segments(36, 24, 48, 26, lty = 1, col = COLA[4], lwd = segmentlinewidth)
segments(48, 26, 56, 26, lty = 1, col = COLA[4], lwd = segmentlinewidth)


#For choice: 
segments(0, 22, 24, 22, lty = 2, col = "grey", lwd = segmentlinewidth)
segments(24, 0, 24, 22, lty = 2, col = "grey", lwd = segmentlinewidth)

points(0, 18, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(0 + 1, 18 - 0.3, expression("L1"), cex = labelsize)
points(12, 20, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(12 + 1, 20 - 0.3, expression("L2"), cex = labelsize)
points(24, 22, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(24 + 1, 22 - 0.3, expression("L3"), cex = labelsize)
points(36, 24, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(36 + 1, 24 - 0.3, expression("L4"), cex = labelsize)
points(48, 26, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(48 + 1, 26 - 0.3, expression("L5"), cex = labelsize)
points(56, 26, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(56 + 1, 26 - 0.3, expression("L6"), cex = labelsize)

#Add points a, b, c and c
segments(5.6, 0, 5.6, riskreturn(g = 5.6), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, riskreturn(g = 5.6), 5.6, riskreturn(g = 5.6), lty = 2, col = "gray", lwd = segmentlinewidth)
points(5.6, riskreturn(g = 5.6), pch = 16, col = "black", cex = 1.5)
text(5.6 + 0.25, riskreturn(g = 5.6) - 0.3, expression(i), cex = labelsize)


segments(12, 0, 12, riskreturn(g = 12) , lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, riskreturn(g = 12) , 12, riskreturn(g = 12) , lty = 2, col = "gray", lwd = segmentlinewidth)
points(12, riskreturn(g = 12) , pch = 16, col = "black", cex = 1.5)
text(12 + 0.25, riskreturn(g = 12) - 0.3, expression(d), cex = labelsize)


points(1.8, riskreturn(g = 1.8) , pch = 16, col = "black", cex = 1.5)
text(1.8 + 0.25, riskreturn(g = 1.8) - 0.3, expression(a), cex = labelsize)


points(9.5, riskreturn(g = 9.5) , pch = 16, col = "black", cex = 1.5)
text(9.5 + 0.25, riskreturn(g = 9.5) - 0.3, expression(b), cex = labelsize)


#Label risk return schedule
text(14, 12.7, expression(paste(omega == g(Delta))), cex = labelsize)

#Label value functions
text(26.5, 27, expression(u[3]^F), cex = labelsize)
text(42.5, 27, expression(u[2]^F), cex = labelsize)
text(54, 27, expression(u[1]^F), cex = labelsize)


dev.off()

