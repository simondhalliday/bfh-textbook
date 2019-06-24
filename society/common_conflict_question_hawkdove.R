require(ggplot2)
require(shape)
pdf(file = "society/common_conflict_question_hawkdove.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
par(mar =  c(5, 5, 4, 2))
xlims <- c(-30, 115)
ylims <- c(-30, 115)

#Plot command 
plot(0, 0, xlim = xlims, ylim = ylims, 
     type = "n",
     xlab = expression(paste("Alfredo's Payoff, ", u^A)),
     ylab = expression(paste("Bob's Payoff, ", u^B)),
     axes = FALSE,
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
ticksy <- c(ylims[1], -20, 0, 50, 100, ylims[2])
ylabels <- c(NA,-20, 0, 50, 100, NA)
ticksx <- c(xlims[1],-20, 0, 50, 100, xlims[2])
xlabels <- c(NA,-20, 0, 50, 100, NA)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Lines fuf/upf
#segments(100, 0, 0, 100, lty = 1, col = "black", lwd = graphlinewidth)

#Add points for outcomes
points(-20, -20, pch = 16, col = "black", cex = 1.5)
text(-20, -25, expression(paste("a")))
points(0, 100, pch = 16, col = "black", cex = 1.5)
text(2, 100, expression(paste("b")))
points(100, 0, pch = 16, col = "black", cex = 1.5)
text(102, 2, expression(paste("c")))
points(50, 50, pch = 16, col = "black", cex = 1.5)
text(52, 52, expression(paste("d")))


#Label utility possibilities frontier
#Arrows(40, 80, 40, 65, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#text(40, 90, expression(paste("Utility Possibilities")))
#text(40, 85, expression(paste("Frontier (upf)")))

dev.off()

