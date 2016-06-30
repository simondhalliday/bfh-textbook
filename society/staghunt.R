require(ggplot2)
require(shape)
pdf(file = "society/staghunt.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 1.75)
ylims <- c(0, 1.75)

#Plot command 
plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     ylab = expression(paste("Sara's Payoff, ", u^S)),
     xlab = expression(paste("Ran's Payoff, ", u^R)),
     xaxt = "n", yaxt = "n", cex.lab = axislabelsize, bty = "n")


#Customize ticks and labels for the plot
ticksy <- c(0, 1, 1.5, 1.75)
ylabels <- c(0, 1, 1.5, 1.75)
ticksx <- c(0, 1, 1.5, 1.75)
xlabels <- c(0, 1, 1.5, 1.75)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)


#Lines for the coordinates of the Nash equilbrium
segments(0, 1, 1, 0, lty = 2, col = "gray", lwd = 2)
segments(1, 1, 1.5, 1.5, lty = 2, col = "gray", lwd = 2)

#Add points a, b, c and c
points(1.5, 1.5, pch = 16, col = "black", cex = 1.5)
text(1.55, 1.55, expression(paste("a")))
points(1, 1, pch = 16, col = "black", cex = 1.5)
text(0.95, 0.95, expression(paste("b")))
points(1, 0, pch = 16, col = "black", cex = 1.5)
text(1.05, 0.05, expression(paste("c")))
points(0, 1, pch = 16, col = "black", cex = 1.5)
text(0.05, 1.05, expression(paste("d")))

#Label utility possibilities frontier
# text(3.8, 1.9, expression(paste("Feasible Utility")))
# text(3.8, 1.65, expression(paste("Frontier")))

dev.off()

