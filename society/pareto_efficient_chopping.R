require(ggplot2)
require(shape)
pdf(file = "society/pareto_efficient_chopping.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 80)
ylims <- c(0, 80)

#Plot command 
plot(0, 0, xlim = xlims, ylim = ylims, 
     type = "n",
     xlab = expression(paste("Player M's Payoff, ", u^M)),
     ylab = expression(paste("Player N's Payoff, ", u^N)),
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
ticksy <- seq(ylims[1], ylims[2], 20)
ylabels <- seq(ylims[1], ylims[2], 20)
ticksx <- seq(xlims[1], xlims[2], 20)
xlabels <- seq(xlims[1], xlims[2], 20)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Lines fuf/upf
#segments(100, 0, 0, 100, lty = 1, col = "black", lwd = graphlinewidth)

#Add points for outcomes
points(66, 24, pch = 16, col = "black", cex = 1.5)
text(66, 24-3, expression(paste("(12 Hours, 6 Hours)")))
points(12, 12, pch = 16, col = "black", cex = 1.5)
text(12, 12-3, expression(paste("(12 Hours, 12 Hours)")))
points(24, 66, pch = 16, col = "black", cex = 1.5)
text(24, 66-3, expression(paste("(6 Hours, 12 Hours)")))
points(51, 51, pch = 16, col = "black", cex = 1.5)
text(51, 51 - 3, expression(paste("(6 Hours, 6 Hours)")))


dev.off()

