require(ggplot2)
require(shape)
pdf(file = "constrained_optimization/midterm_mrs_xy.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 21)
ylims <- c(0, 21)

mrs <- function(x){
  20 - x
}

#Plot command 
plot(0, 0, xlim = xlims, ylim = ylims, 
     type = "n",
     ylab = expression(paste("Marginal rate of substitution, ", mrs(x,y))),
     xlab = expression(paste("Quantity of hot chocolate, ", x)),
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
ticksy <- seq(ylims[1], ylims[2], 5)
ylabels <- seq(ylims[1], ylims[2], 5)
ticksx <-seq(xlims[1], xlims[2], 5)
xlabels <-seq(xlims[1], xlims[2], 5)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)


xx1 <- seq(xlims[1], xlims[2], length.out = 500)
lines(xx1, mrs(xx1), col = COLB[4], lwd = graphlinewidth)

#Lines fuf/upf
# segments(100, 0, 0, 100, lty = 1, col = "black", lwd = graphlinewidth)

# #Add points for outcomes
# points(-5, -5, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
# text(-5, -6, expression(paste("(Escalate, Escalate)")))
# points(-10, 5, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
# text(-8.5, 4, expression(paste("(Disarm, Escalate)")))
# points(5, -10, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
# text(4, -9, expression(paste("(Escalate, Disarm)")))
# points(0, 0, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
# text(2, 1, expression(paste("(Disarm, Disarm)")))


#Label utility possibilities frontier
# Arrows(40, 80, 40, 65, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(10, 12, expression(paste(mrs(x,y) == 20 - x)))
#text(40, 85, expression(paste("Frontier (upf)")))

dev.off()

