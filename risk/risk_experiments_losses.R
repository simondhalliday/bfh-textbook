require(shape)
require(plotrix)

pdf(file = "risk/risk_experiments_losses.pdf", width = 9, height = 7)

# Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.8
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)

a <- c(2, 4, 6)
par(mar =  c(5, 5, 1, 1))
xlims <- c(0, 60)
ylims <- c(-28, 0)

riskreturn <- function(g, int1 = 14, int2 = 4, coeff = 1/3){
  int1 - (int2 - (coeff)*g)^2
}

indiffA <- function(d, intercept = 3, xbar = 60, pbar = 0.5){
  intercept - pbar*d + (1/2)*(pbar/xbar)*d^2
}

# indiffA <- function(d, intercept = 3, xbar = 160, pbar = 0.5){
#   intercept - pbar*d + (1/2)*(pbar/xbar)*d^2
# }

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
     xaxs = "i", 
     yaxs = "i"
)


ticksy <- c(ylims[1], -18, -20, -22, -24, -26, ylims[2])
ylabels <- c(NA, -18, -20, -22, -24, -26, NA)
ticksx <- c(0, 12, 24, 36, 48, 56,  xlims[2])
xlabels <- c(NA, 12, 24, 36, 48, 56, NA)

axis(1, at = ticksx, pos = ylims[1], labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = xlims[1], labels = ylabels, las = 1, cex.axis = labelsize)


text(0.5*xlims[2], 13.5, expression(paste("Difference in payoff (good versus bad outcome), ", Delta, ", risk")), xpd = TRUE, cex = axislabelsize)
text(xlims[1] - 6, ylims[2] - 0.5*(ylims[2] - ylims[1]), expression(paste("Expected payoff, ", hat(y) )), xpd = TRUE, cex = axislabelsize, srt = 90) 
# 
# axis.break(axis = 2, bgcol="white", breakcol="black",
#            style="slash", brw=0.01)



npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
#lines(xx1, riskreturn(xx1), col = COLA[4], lwd = graphlinewidth, lty = 1)
lines(xx1, indiffA(xx1, intercept = -7), col = COLB[4], lwd = graphlinewidth, lty = 1)
#Low
lines(xx1, indiffA(xx1, intercept = -11), col = COLB[4], lwd = graphlinewidth, lty = 1)
#high
lines(xx1, indiffA(xx1, intercept = -3), col = COLB[4], lwd = graphlinewidth, lty = 1)


#lines(xx1, indiffA(xx1, intercept = 21, slope = 0.00235), col = COLB[4], lwd = graphlinewidth, lty = 1)
# lines(xx1, indiffA(xx1, intercept = 5.6), col = COLB[4], lwd = graphlinewidth, lty = 1)
# lines(xx1, indiffA(xx1, intercept = 9), col = COLB[4], lwd = graphlinewidth, lty = 1)

#Setting up the segments for the choices 
# segments(0, 18, 12, 20, lty = 1, col = COLA[4], lwd = graphlinewidth)
# segments(12, 20, 24, 22, lty = 1, col = COLA[4], lwd = graphlinewidth)
# segments(24, 22, 36, 24, lty = 1, col = COLA[4], lwd = graphlinewidth)
# segments(36, 24, 48, 26, lty = 1, col = COLA[4], lwd = graphlinewidth)
# segments(48, 26, 56, 26, lty = 1, col = COLA[4], lwd = graphlinewidth)


#For choice: 
# segments(0, 24, 36, 24, lty = 2, col = "grey", lwd = segmentlinewidth)
# segments(36, 0, 36, 24, lty = 2, col = "grey", lwd = segmentlinewidth)

# points(0, 18, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
# text(0 + 2, 18 - 0.3, expression("L1"), cex = labelsize)
# points(12, 20, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
# text(12 + 2, 20 - 0.2, expression("L2"), cex = labelsize)
# points(24, 22, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
# text(24 + 2, 22 - 0.3, expression("L3"), cex = labelsize)
# points(36, 24, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
# text(36 + 2, 24 - 0.3, expression("L4"), cex = labelsize)
# points(48, 26, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
# text(48 + 2, 26 - 0.3, expression("L5"), cex = labelsize)
# points(56, 26, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
# text(56 + 2, 26 - 0.3, expression("L6"), cex = labelsize)

#Add points a, b, c and c
segments(5.6, 0, 5.6, riskreturn(g = 5.6), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, riskreturn(g = 5.6), 5.6, riskreturn(g = 5.6), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(5.6, riskreturn(g = 5.6), pch = 16, col = "black", cex = 1.5)
text(5.6 + 0.25, riskreturn(g = 5.6) - 0.3, expression(i), cex = labelsize)


segments(12, 0, 12, riskreturn(g = 12) , lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, riskreturn(g = 12) , 12, riskreturn(g = 12) , lty = 2, col = grays[20], lwd = segmentlinewidth)
points(12, riskreturn(g = 12) , pch = 16, col = "black", cex = 1.5)
text(12 + 0.25, riskreturn(g = 12) - 0.3, expression(d), cex = labelsize)


points(1.8, riskreturn(g = 1.8) , pch = 16, col = "black", cex = 1.5)
text(1.8 + 0.25, riskreturn(g = 1.8) - 0.3, expression(a), cex = labelsize)


points(9.5, riskreturn(g = 9.5) , pch = 16, col = "black", cex = 1.5)
text(9.5 + 0.25, riskreturn(g = 9.5) - 0.3, expression(b), cex = labelsize)


#Label risk return schedule
text(14, 12.7, expression(paste(omega == g(Delta))), cex = labelsize)

#Label value functions
text(39, 27, expression(u[3]), cex = labelsize)
text(48.5, 27, expression(u[2]), cex = labelsize)
text(56.5, 27, expression(u[1]), cex = labelsize)

Arrows(15, 24, 10, 26.5,
       col = "black", lty = 1, lwd = 2, arr.type = "triangle",
       arr.lwd = 0.5, code = 2)
text(16.5, 25.25, expression(paste("Better")), xpd = TRUE, cex = annotatesize)
#text(8.05, 3.5, expression(paste("due to insurance")), xpd = TRUE)

# Arrows(36, 19.5, 36, 21.7,
#        col = "black", lty = 1, lwd = 2, arr.type = "triangle",
#        arr.lwd = 0.5, code = 2)
# text(36, 19, expression(paste("Slope",  phantom() == -mrs)), xpd = TRUE, cex = annotatesize)
# text(38.1, 17.8, expression(paste(phantom() == frac(-u[Delta],u[hat(y)]))), xpd = TRUE, cex = annotatesize)
#text(8.05, 3.5, expression(paste("due to insurance")), xpd = TRUE)


dev.off()

