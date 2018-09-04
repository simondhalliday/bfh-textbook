require(ggplot2)
require(shape)
pdf(file = "society/society_fig2.pdf", width = 7, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 5)
ylims <- c(0, 5)

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

axislabelsize <- 1.5
graphlinewidth <- 2.5
segmentlinewidth <- 2

#Plot command 
plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = "",
     ylab = "",
     xaxt = "n", yaxt = "n", cex.lab = axislabelsize, bty = "n")

mtext(expression(paste("Alfredo's payoffs, ", u^A)), side=1, line = 2.5, cex = axislabelsize)
text(-0.5, 0.5*ylims[2], expression(paste("Bob's payoffs, ", u^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Customize ticks and labels for the plot
ticksy <- seq(0, 5, 1)
ylabels <- seq(0, 5, 1)
ticksx <- seq(0, 5, 1)
xlabels <- seq(0, 5, 1)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Rent polygon
xrent <- c(2, 3, 3.5, 2)
yrent <- c(3.5, 3, 2, 2)
polygon(xrent, yrent, col=COL[4], density=NULL, border = NA)


#Lines for the coordinates of the Nash equilbrium
segments(1, 4, 3, 3, lty = 1, col = "#7fc97f", lwd = graphlinewidth)
segments(3, 3, 4, 1, lty = 1, col = "#7fc97f", lwd = graphlinewidth)

#Lines for Fallback positions
segments(0, 2, 5, 2, lty = 2, col = "darkgrey", lwd = segmentlinewidth)
text(4.5, 1.9, expression(paste("B's fallback")))
segments(2, 0, 2, 5, lty = 2, col = "darkgrey", lwd = segmentlinewidth)
text(2.45, 4.8, expression(paste("A's fallback")))

#Add points a, b, c and c
points(1, 4, pch = 16, col = "black", cex = 1.5)
text(1.1, 4.1, expression(paste("d")))
points(3, 3, pch = 16, col = "black", cex = 1.5)
text(3.1, 3.1, expression(paste("c")))
points(4, 1, pch = 16, col = "black", cex = 1.5)
text(4.1, 1.1, expression(paste("b")))
points(2, 2, pch = 16, col = "black", cex = 1.5)
text(2.1, 2.1, expression(paste("a")))
points(3.5, 2, pch = 16, col = "black", cex = 1.5)
text(3.6, 2.1, expression(paste("e")))
points(2, 3.5, pch = 16, col = "black", cex = 1.5)
text(2.1, 3.6, expression(paste("f")))
text(1.35, 1.8, expression(paste("Nash equilibrium")))

#Label utility possibilities frontier
text(3.9, 2.6, expression(paste("Utility Possibilities")))
text(3.9, 2.35, expression(paste("Frontier (upf)")))


#Arrows and economic rent label
Arrows(2.7, 3.7, 2.7, 2.7, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(2.8, 3.8,  expression(paste("Economic Surplus")))

dev.off()

