require(ggplot2)
require(shape)
pdf(file = "information_power/agent_brf_prices.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5

brfFn <- function(p, delta = 5) {
  1 - (2 * delta) /p
}

solowCondition <- function(p, delta = 5){
  (p*(1/(8*delta)))
}

#COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")
par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 40)
ylims <- c(0, 1)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = "",
     ylab = "",
     xaxt = "n", yaxt = "n", cex.lab = axislabelsize, bty = "n")

mtext(expression(paste("Price, ", p)), side=1, line = 2.5, cex = axislabelsize)
text(-4, ylims[2] - 0.5*(ylims[2] - ylims[1]), expression(paste("Quality, ", q)), xpd = TRUE, cex = axislabelsize, srt = 90) 


npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(10, xlims[2], length.out = npts)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)

#Draw the lines for the graphs
lines(xx1, brfFn(xx1), col = COL[2], lwd = graphlinewidth)


#lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 4)

#Customize ticks and labels for the plot
ticksy <- c(0, 0.33, 0.5, 2/3, 1)
ylabels <- c(0, expression(paste(frac(1,3))), expression(paste(frac(1,2))), expression(paste(frac(2,3))), 1)
ticksx <- c(0, 10, 15, 20, 30, 40)
xlabels <- c(0, 10, 15, 20, 30, 40)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Annotation of the three graphs and the NE

text(35, 0.87, expression(paste("Best-response")))
text(35, 0.8, expression(paste("Function = q = ", 1 - frac(2*delta, p))))
#text(16, 0.52, expression(paste("Nash Equilibrium")))

#Lines for the coordinates of the Nash equilbrium
segments(20, 0, 20, 0.5, lty = 2, col = "darkgray", lwd = segmentlinewidth)
segments(0, 0.5, 20, 0.5, lty = 2, col = "darkgray", lwd = segmentlinewidth)

segments(15, 0, 15, 1/3, lty = 2, col = "darkgray", lwd = segmentlinewidth)
segments(0, 1/3, 15, 1/3, lty = 2, col = "darkgray", lwd = segmentlinewidth)

segments(30, 0, 30, 2/3, lty = 2, col = "darkgray", lwd = segmentlinewidth)
segments(0, 2/3, 30, 2/3, lty = 2, col = "darkgray", lwd = segmentlinewidth)


#BRF Slope annotation
text(25, 0.81, expression(paste("Slope = ", frac(2*delta, p^2))))
Arrows(25, 0.79, 25, 0.63, col = "black", lty = 1, lwd = 2, arr.type = "triangle")

#Add a point for the NE
points(20, 0.5, pch = 16, col = "black", cex = 1.5)
text(20 + 0.5, 0.5 - 0.025, expression(paste(b)))

points(15, 0.33, pch = 16, col = "black", cex = 1.5)
text(15 + 0.5, 0.33 - 0.025, expression(paste(a)))

points(30, 2/3, pch = 16, col = "black", cex = 1.5)
text(30 + 0.5, 2/3 - 0.025, expression(paste(c)))

#Add a ray and a point for d.  
# segments(0, 0, 20, 1, lty = 2, lwd = 3, col = "darkgray")
# points(10, 0.5, pch = 16, col = "black", cex = 1.2)
# text(9.6, 0.53, expression(paste("d")))

#Add a ray and a point for a. 
# segments(0, 0, 20, 0.25, lty = 2, lwd = 3, col = "darkgray")
# points(11.8, 0.15, pch = 16, col = "black", cex = 1.2)
# text(11.3, 0.17, expression(paste("c")))

dev.off()

