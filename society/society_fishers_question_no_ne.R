require(ggplot2)
require(shape)
pdf(file = "society/society_fishers_question_no_ne.pdf", width = 7, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
par(mar =  c(5, 5, 2, 2))
xlims <- c(0, 4.5)
ylims <- c(0, 4.5)


ticksy <- c(ylims[1], 1, 2, 3, 4, ylims[2])
ylabels <- c(ylims[1], 1, 2, 3, 4, NA)
ticksx <- c(xlims[1], 1, 2, 3, 4, xlims[2])
xlabels <- c(xlims[1], 1, 2, 3, 4, NA)

#Plot command 
plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")),
     xaxt = "n", yaxt = "n", cex.lab = axislabelsize, bty = "n")

#Customize ticks and labels for the plot
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)


mtext(expression(paste("Alfredo's payoffs, ", u^A)), side=1, line = 2.5, cex = axislabelsize)
text(-0.5, 0.5*ylims[2], expression(paste("Bob's payoffs, ", u^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Lines for the coordinates of the Nash equilbrium
#segments(1, 4, 3, 3, lty = 1, col = "#7fc97f", lwd = 4)
#segments(3, 3, 4, 1, lty = 1, col = "#7fc97f", lwd = 4)

#Add points a, b, c and c
points(1, 4, pch = 16, col = "black", cex = 1.5)
text(1.1, 4.1, expression(paste("d")))
points(3, 3, pch = 16, col = "black", cex = 1.5)
text(3.1, 3.1, expression(paste("c")))
points(4, 1, pch = 16, col = "black", cex = 1.5)
text(4.1, 1.1, expression(paste("b")))
points(2, 2, pch = 16, col = "black", cex = 1.5)
text(2.1, 2.1, expression(paste("a")))
#atext(1.5, 1.8, expression(paste("Nash equilibrium")))

#Label utility possibilities frontier
#text(3.9, 2.6, expression(paste("Utility Possibilities")))
#text(3.9, 2.35, expression(paste("Frontier (upf)")))


dev.off()

