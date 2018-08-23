require(ggplot2)
require(shape)
pdf(file = "society/society_assurance_upf.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 5)
ylims <- c(0, 5)

axislabelsize <- 1.5
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")


#Plot command 
plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("Aram's Payoff, ", u^A)),
     ylab = expression(paste("Bina's Payoff, ", u^B)),
     xaxt = "n", yaxt = "n", cex.lab = axislabelsize, bty = "n")


#Customize ticks and labels for the plot
ticksy <- seq(0, 5, 1)
ylabels <- seq(0, 5, 1)
ticksx <- seq(0, 5, 1)
xlabels <- seq(0, 5, 1)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Rent polygon
xrent <- c(2, 4, 3.33, 2)
yrent <- c(3.33, 4, 2, 2)
polygon(xrent, yrent, col=COL[4], density=NULL, border = NA)


#Lines for the coordinates of the Nash equilbrium
segments(1, 3, 4, 4, lty = 1, col = "#7fc97f", lwd = 4)
segments(3, 1, 4, 4, lty = 1, col = "#7fc97f", lwd = 4)

#Lines for Fallback positions
segments(0, 2, 5, 2, lty = 2, col = "darkgrey", lwd = 2)
text(4.5, 1.8, expression(paste("B's fallback")))
segments(2, 0, 2, 5, lty = 2, col = "darkgrey", lwd = 2)
text(2.4, 4.8, expression(paste("A's fallback")))

#Add points a, b, c and c
points(1, 3, pch = 16, col = "black", cex = 1.5)
text(1.1, 2.9, expression(paste(b)))
points(4, 4, pch = 16, col = "black", cex = 1.5)
text(4.1, 3.9, expression(paste(c)))
text(4.4, 4.4, expression(paste("Pareto-efficient")))
text(4.4, 4.2, expression(paste("Nash equilibrium")))

points(3, 1, pch = 16, col = "black", cex = 1.5)
text(3.1, 1, expression(paste(d)))
points(2, 2, pch = 16, col = "black", cex = 1.5)
text(2.1, 2.1, expression(paste(a)))

points(3.33, 2, pch = 16, col = "black", cex = 1.5)
text(3.4, 1.9, expression(paste(f)))
points(2, 3.33, pch = 16, col = "black", cex = 1.5)
text(1.9, 3.45, expression(paste(e)))
text(1.5, 1.8, expression(paste("Pareto-inferior")))
text(1.5, 1.6, expression(paste("Nash equilibrium")))

#Label utility possibilities frontier
text(4.05, 2.6, expression(paste("Utility Possibilities")))
text(4.05, 2.4, expression(paste("Frontier (upf)")))

#Arrows and economic rent label
Arrows(2.7, 3.7, 2.7, 2.7, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(2.7, 4,  expression(paste("Economic")))
text(2.7, 3.8,  expression(paste("Rent")))

dev.off()

