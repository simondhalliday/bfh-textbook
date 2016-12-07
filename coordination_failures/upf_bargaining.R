require(shape)
pdf(file = "coordination_failures/upf_bargaining.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(5, 5, 4, 2))
xlims <- c(40, 72)
ylims <- c(40, 72)

upf <- function(uA, intercept = 110, slope = 1){
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
# ticksy <- seq(ylims[1], ylims[2], 5)
# ylabels <- seq(ylims[1], ylims[2], 5)
# ticksx <- seq(xlims[1], xlims[2], 5)
# xlabels <- seq(xlims[1], xlims[2], 5)
ticksy <- c(40, 46, 55, 64, ylims[2])
ylabels <- c(NA, expression(paste(u[n]^B==u[f]^B)), expression(paste(u[i]^B)), expression(paste(u[g]^B)), NA)
ticksx <- c(40, 46, 55, 64, ylims[2])
xlabels <- c(NA, expression(paste(u[n]^A==u[g]^A)), expression(paste(u[i]^A)), expression(paste(u[f]^A)), NA)
axis(1, at = ticksx, pos = 40, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 40, labels = ylabels, las = 1, cex.axis = labelsize)
mtext(expression(paste("A's utility, ", u^A)), side=1, line = 2.5, cex = axislabelsize)
text(xlims[1] - 3, ylims[2] - 0.5*(ylims[2] - ylims[1]), expression(paste("B's utility, ", u^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Rent polygon
xrent <- c(46, 64, 46, 46)
yrent <- c(46, 46, 64, 46)
polygon(xrent, yrent, col=COL[4], density=NULL, border = NA)

npts <- 500 
xx1 <- seq(xlims[1], 46, length.out = npts)
xx2 <- seq(46, 64, length.out = npts)
xx3 <- seq(64, xlims[2], length.out = npts)
lines(xx1, upf(xx1), col = COLA[5], lwd = segmentlinewidth, lty = 2)
lines(xx2, upf(xx2), col = COLA[5], lwd = graphlinewidth)
lines(xx3, upf(xx3), col = COLA[5], lwd = segmentlinewidth, lty = 2)



#Lines for the coordinates of the Nash equilbrium


text(52, 65, expression(paste("Pareto-improving")), cex = labelsize)
text(52, 64, expression(paste("")), cex = labelsize)
Arrows(52, 63, 52, 52, col = "black", lty = 1, lwd = 2, arr.type = "triangle")


text(62, 56, expression(paste("Feasible utility frontier, or")), cex = labelsize)
text(62, 55, expression(paste("Bargaining set")), cex = labelsize)
Arrows(62, 54, 62, 49, col = "black", lty = 1, lwd = 2, arr.type = "triangle")

#Lines for Fallback positions
segments(40, 46, xlims[2], 46, lty = 2, col = "darkgrey", lwd = segmentlinewidth)
text(48.5, 70, expression(paste("A's fallback")), cex = labelsize)
segments(46, 40, 46, ylims[2], lty = 2, col = "darkgrey", lwd = segmentlinewidth)
text(70, 47, expression(paste("B's fallback")), cex = labelsize)

#Add points a, b, c and c

# points(72, 72, pch = 16, col = "black", cex = 1.5)
# text(72.5, 72.5, expression(paste("a")))


points(64, 46, pch = 16, col = "black", cex = 1.5)
text(64.75, 46.75, expression(paste("f")))

segments(0, 55, 55, 55, lty = 2, col = "darkgrey", lwd = segmentlinewidth)
segments(55, 0, 55, 55, lty = 2, col = "darkgrey", lwd = segmentlinewidth)
points(55, 55, pch = 16, col = "black", cex = 1.5)
text(55.75, 55.75, expression(paste("i")))

segments(0, 64, 46, 64, lty = 2, col = "darkgrey", lwd = segmentlinewidth)
points(46, 64, pch = 16, col = "black", cex = 1.5)
text(46.75, 64.75, expression(paste("g")))

segments(64, 0, 64, 46, lty = 2, col = "darkgrey", lwd = segmentlinewidth)
points(46, 46, pch = 16, col = "black", cex = 1.5)
text(45.25, 45.25, expression(paste("n")))

# points(3.5, 2, pch = 16, col = "black", cex = 1.5)
# text(3.6, 2.1, expression(paste("f")))
# points(2, 3.5, pch = 16, col = "black", cex = 1.5)
# text(2.1, 3.6, expression(paste("e")))
#text(49, 47, expression(paste("Nash equilibrium")))


# points(77, 73, pch = 16, col = "black", cex = 1.5)
# text(77.5, 73.5, expression(paste("q")))
# 
# points(73, 77, pch = 16, col = "black", cex = 1.5)
# text(73.5, 77.5, expression(paste("p")))

# points(62, 84.5, pch = 16, col = "black", cex = 1.5)
# points(84.5, 62, pch = 16, col = "black", cex = 1.5)
# points(38, 98, pch = 16, col = "black", cex = 1.5)
# points(98, 38, pch = 16, col = "black", cex = 1.5)
# points(91.125, 51.75, pch = 16, col = "black", cex = 1.5)
# points(51.75, 91.125, pch = 16, col = "black", cex = 1.5)
# points(20.75, 105.25, pch = 16, col = "black", cex = 1.5)
# points(105.25, 20.75, pch = 16, col = "black", cex = 1.5)
# points(0, 112.5, pch = 16, col = "black", cex = 1.5)
# points(112.5, 0, pch = 16, col = "black", cex = 1.5)


#Label utility possibilities frontier
# text(3.8, 2.6, expression(paste("Utility Possibilities")))
# text(3.8, 2.35, expression(paste("Frontier (upf)")))

#Arrows and economic rent label
# Arrows(2.7, 3.7, 2.7, 2.7, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
# text(2.8, 3.8,  expression(paste("Economic Rent")))

dev.off()

