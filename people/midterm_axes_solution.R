require(ggplot2)
require(shape)
pdf(file = "society/midterm_axes_solution.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
par(mar =  c(5, 5, 4, 2))
xlims <- c(-10, 6)
ylims <- c(-10, 6)

#Plot command 
plot(0, 0, xlim = xlims, ylim = ylims, 
     type = "n",
     xlab = expression(paste("India, ",u[I])),
     ylab = expression(paste("Pakistan, ",u[P])),
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
ticksy <- c(-10, -5, 0, 5, 6)
ylabels <- c(-10, -5, NA, 5, NA)
ticksx <-c(-10, -5, 0, 5, 6)
xlabels <-c(-10, -5, NA, 5, NA)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Lines fuf/upf
# segments(100, 0, 0, 100, lty = 1, col = "black", lwd = graphlinewidth)

# #Add points for outcomes
points(-5, -5, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(-5, -6, expression(paste("(Escalate, Escalate)")))
points(-10, 5, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(-8.5, 4, expression(paste("(Disarm, Escalate)")))
points(5, -10, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(4, -9, expression(paste("(Escalate, Disarm)")))
points(0, 0, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(2, 1, expression(paste("(Disarm, Disarm)")))


#Label utility possibilities frontier
# Arrows(40, 80, 40, 65, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
# text(40, 90, expression(paste("Utility Possibilities")))
# text(40, 85, expression(paste("Frontier (upf)")))

dev.off()

