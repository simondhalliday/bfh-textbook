require(ggplot2)
require(shape)
pdf(file = "society_fig1.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
par(mar =  c(5, 5, 4, 2))
xlims <- c(-5, 5)
ylims <- c(-5, 5)

#Plot command 
plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")),
     xaxt = "n", yaxt = "n", cex.lab = axislabelsize, bty = "n")


#Customize ticks and labels for the plot
axis(1, pos = 0)
axis(2, pos = 0, las = 1)

#Lines for the coordinates of the Nash equilbrium
segments(1, 4, 3, 3, lty = 1, col = "#7fc97f", lwd = 4)
segments(3, 3, 4, 1, lty = 1, col = "#7fc97f", lwd = 4)

#Add points a, b, c and c
points(1, 4, pch = 16, col = "black", cex = 1.5)
text(1.1, 4.1, expression(paste("b")))
points(3, 3, pch = 16, col = "black", cex = 1.5)
text(3.1, 3.1, expression(paste("c")))
points(4, 1, pch = 16, col = "black", cex = 1.5)
text(4.1, 1.1, expression(paste("d")))
points(2, 2, pch = 16, col = "black", cex = 1.5)
text(2.1, 2.1, expression(paste("a")))
text(1.5, 1.8, expression(paste("Nash equilibrium")))

#Label utility possibilities frontier
text(3.8, 2.6, expression(paste("Utility Possibilities")))
text(3.8, 2.35, expression(paste("Frontier (upf)")))


dev.off()

