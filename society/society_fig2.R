require(ggplot2)
require(shape)
pdf(file = "society_fig2.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 5)
ylims <- c(0, 5)

#Plot command 
plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("Bob's Payoff, ", u^B)),
     ylab = expression(paste("Alfredo's Payoff, ", u^A)),
     xaxt = "n", yaxt = "n", cex.lab = axislabelsize, bty = "n")


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
polygon(xrent, yrent, col="powderblue", density=NULL, border = NA)


#Lines for the coordinates of the Nash equilbrium
segments(1, 4, 3, 3, lty = 1, col = "#7fc97f", lwd = 4)
segments(3, 3, 4, 1, lty = 1, col = "#7fc97f", lwd = 4)

#Lines for Fallback positions
segments(0, 2, 5, 2, lty = 2, col = "darkgrey", lwd = 3)
text(4.5, 1.8, expression(paste("A's fallback")))
segments(2, 0, 2, 5, lty = 2, col = "darkgrey", lwd = 3)
text(2.4, 4.8, expression(paste("B's fallback")))

#Add points a, b, c and c
points(1, 4, pch = 16, col = "black", cex = 1.5)
text(1.1, 4.1, expression(paste("b")))
points(3, 3, pch = 16, col = "black", cex = 1.5)
text(3.1, 3.1, expression(paste("c")))
points(4, 1, pch = 16, col = "black", cex = 1.5)
text(4.1, 1.1, expression(paste("d")))
points(2, 2, pch = 16, col = "black", cex = 1.5)
text(2.1, 2.1, expression(paste("a")))
points(3.5, 2, pch = 16, col = "black", cex = 1.5)
text(3.6, 2.1, expression(paste("f")))
points(2, 3.5, pch = 16, col = "black", cex = 1.5)
text(2.1, 3.6, expression(paste("e")))
text(1.5, 1.8, expression(paste("Nash equilibrium")))

#Label utility possibilities frontier
text(3.8, 2.6, expression(paste("Utility Possibilities")))
text(3.8, 2.35, expression(paste("Frontier (upf)")))

#Arrows and economic rent label
Arrows(2.7, 3.7, 2.7, 2.7, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(2.8, 3.8,  expression(paste("Economic Rent")))

dev.off()

