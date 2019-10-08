require(shape)
pdf(file = "what_can_markets_do/upf_bargaining_coase.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(2, 4, 2, 2))
xlims <- c(-15, 9)
ylims <- c(-9, 15)

upf <- function(uA, intercept = -4.5, slope = 1){
  intercept - slope*uA
}


#Plot command 
plot(40, 40, xlim = xlims, ylim = ylims, 
     type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     axes = FALSE,
     # xlab = expression(paste("Bob's Payoff, ", u^B)),
     # ylab = expression(paste("Alfredo's Payoff, ", u^A)),
     #xaxt = "n", 
     #yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i"
)

#Customize ticks and labels for the plot
# ticksy <- seq(ylims[1], ylims[2], 1)
# ylabels <- seq(ylims[1], ylims[2], 1)
# ticksx <- seq(xlims[1], xlims[2], 1)
# xlabels <- seq(xlims[1], xlims[2], 1)
ticksy <- c(ylims[1], -4.5, -2.25, 2.25, 4.5, ylims[2])
ylabels <- c(NA, NA, NA, NA, NA, NA)
ticksx <- c(xlims[1], -9, -6.75, -4.5, -2.25, xlims[2])
xlabels <- c(NA, -9, -6.75, -4.5, NA, NA)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)
#mtext(expression(paste("A's utility, ", u^A)), side = 1, line = 2.5, cex = axislabelsize)

text(xlims[1], ylims[2] - 0.5*(ylims[2] - ylims[1]) - 5, expression(paste("A's utility, ", u^A)), xpd = TRUE, cex = axislabelsize) 

text(xlims[1] - 1.5, ylims[2] - 0.5*(ylims[2] - ylims[1]), expression(paste("B's utility, ", u^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Label 2.25 and 4.5
text(0.8, 2.25, expression(paste(2.25)), xpd = TRUE, cex = labelsize) 
text(0.8, 4.5, expression(paste(4.5)), xpd = TRUE, cex = labelsize) 
text(0.9, -2.25 , expression(paste(-2.25)), xpd = TRUE, cex = labelsize) 
text(-2.25, 0.5 , expression(paste(-2.25)), xpd = TRUE, cex = labelsize) 

#Rent polygon
xrent <- c(-9, -9, -4.5, -9)
yrent <- c(0, 4.5, 0, 0)
polygon(xrent, yrent, col=COL[4], density = NULL, border = NA)

npts <- 500 
xx1 <- seq(xlims[1], -9, length.out = npts)
xx2 <- seq(-9, -4.5, length.out = npts)
xx3 <- seq(-4.5, xlims[2], length.out = npts)
lines(xx1, upf(xx1), col = COLA[5], lwd = segmentlinewidth, lty = 2)
lines(xx2, upf(xx2), col = COLA[5], lwd = graphlinewidth)
lines(xx3, upf(xx3), col = COLA[5], lwd = segmentlinewidth, lty = 2)



#Lines for the coordinates of the Nash equilbrium
text(-7.9, -7, expression(paste("Pareto-improving")), cex = labelsize)
text(-7.9, -8, expression(paste("lens")), cex = labelsize)
Arrows(-7.9, -6, -7.9, 1, col = "black", lty = 1, lwd = 2, arr.type = "triangle")

#Label the UPF
text(-5.625, 14.2, expression(paste("Utility possibilities frontier, or")), cex = labelsize)
text(-5.625, 13, expression(paste("Bargaining set")), cex = labelsize)
Arrows(-5.625, 12, -5.625, 2, col = "black", lty = 1, lwd = 2, arr.type = "triangle")

#Lines for Fallback positions
segments(-9, 0, -9, 12, lty = 2, col = "darkgrey", lwd = segmentlinewidth)
text(-11.2, 3, expression(paste("A's fallback")), cex = labelsize)
text(-11.2, 1.5, expression(paste(u[e]^A == -9)), cex = labelsize)

segments(46, 40, 46, ylims[2], lty = 2, col = "darkgrey", lwd = segmentlinewidth)

text(4, 2.5, expression(paste("B's fallback")), cex = labelsize)
text(4, 1, expression(paste(u[e]^B == 0)), cex = labelsize)

#Add points

points(0, -4.5, pch = 16, col = "black", cex = 1.5)
text(0.5, -4, expression(paste(e*minute)))


points(-4.5, 0, pch = 16, col = "black", cex = 1.5)
text(-4.25, 1, expression(paste("f")))

segments(0, 55, 55, 55, lty = 2, col = "darkgrey", lwd = segmentlinewidth)
segments(55, 0, 55, 55, lty = 2, col = "darkgrey", lwd = segmentlinewidth)
points(55, 55, pch = 16, col = "black", cex = 1.5)
text(55.75, 55.75, expression(paste("i")))

segments(0, 64, 46, 64, lty = 2, col = "darkgrey", lwd = segmentlinewidth)

points(-9, 4.5, pch = 16, col = "black", cex = 1.5)
text(-8.5, 5, expression(paste("g")))

segments(64, 0, 64, 46, lty = 2, col = "darkgrey", lwd = segmentlinewidth)
points(46, 46, pch = 16, col = "black", cex = 1.5)
text(45.25, 45.25, expression(paste("n")))

points(-6.75, 2.25, pch = 16, col = "black", cex = 1.5)
text(-6.25, 3, expression(paste(n)))

points(-9, 0, pch = 16, col = "black", cex = 1.5)
text(-9.5, -0.5, expression(paste("e")))



points(-2.25, -2.25, pch = 16, col = "black", cex = 1.5)
text(-1.75, -1.75, expression(paste(i)))



dev.off()

