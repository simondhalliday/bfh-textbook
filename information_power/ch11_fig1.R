require(ggplot2)
require(shape)
pdf(file = "bfh-textbook/information_power/ch11_fig1.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5

brfFn <- function(p, delta = 5) {
  1 - (2*delta) /p
}

solowCondition <- function(p, delta = 5){
  (p*(1/(8*delta)))
}

solowInfeas <- function(p, delta = 5){
  (p*(1/(4*delta)))
}

#COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")
par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 40)
ylims <- c(0, 1)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("Price, ", p)),
     ylab = expression(paste("Quality, ", q)),
     xaxt = "n", yaxt = "n", cex.lab = axislabelsize, bty = "n")

npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(10, xlims[2], length.out = npts)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
xx4 <- seq(xlims[1], 10, length.out = npts2)

#Draw the lines for the graphs
lines(xx1, brfFn(xx1), col = COL[2], lwd = 4)
lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 4)
lines(xx2, solowInfeas(xx2, delta = 5), col = COL[1], lwd = 4, lty = 2)

#Customize ticks and labels for the plot
ticksy <- c(0, 0.5, 1)
ylabels <- c(0, expression(paste(frac(1,2))), 1)
ticksx <- c(0, 10, 20, 40)
xlabels <- c(0, NA, NA, 40)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Annotation of the three graphs and the NE
text(35, 0.98, expression(paste("Iso-profit")))
text(15, 0.98, expression(paste("Infeasible Iso-profit")))
text(35, 0.63, expression(paste("Best response function")))
text(16, 0.52, expression(paste("Nash Equilibrium")))

#Lines for the coordinates of the Nash equilbrium
segments(20, 0, 20, 0.5, lty = 2, col = "darkgray")
segments(0, 0.5, 20, 0.5, lty = 2, col = "darkgray")

#Add a point for the NE
points(20, 0.5, pch = 16, col = "black", cex = 1.5)

#Arrow to Slope of BRF
Arrows(14.2, 0.12, 12.2, 0.12, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(16.8, 0.12, expression(paste("Slope = ", q[p])))

#Arrow to Slope of isoprofit
Arrows(13, 0.80, 15, 0.80, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(10.2, 0.80, expression(paste("Slope = ", frac(q, p))))

dev.off()

