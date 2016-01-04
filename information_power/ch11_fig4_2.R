require(ggplot2)
require(shape)
pdf(file = "ch11_fig4_2.pdf", width = 9, height = 7)
brfFn <- function(p, delta = 5) {
  1 - (2 * delta) /p
}

isovhigh1 <- function(p, delta = 1.25, v = 5){
  (sqrt(p^2 - 4 * delta * v) - p + 2*v)/( 2 * v )
}

isovlow1 <- function(p, delta = 1.25, v = 5){
  (-sqrt(p^2 - 4 * delta * v) - p + 2*v)/( 2 * v )
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
     xlab = expression(paste("Price, ", p)),
     ylab = expression(paste("Quality, ", q)),
     xaxt = "n", yaxt = "n", cex.lab = 2, bty = "n")

npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(10, xlims[2], length.out = npts)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
xx4 <- seq(xlims[1], 6.125, length.out = npts2)
#yy1 <- isovhigh(xx3, delta = 5, v = 20)

#Draw the lines for the graphs
lines(xx1, brfFn(xx1), col = COL[2], lwd = 4)
lines(xx3, isovhigh(xx3, v = 5, delta = 1.25), col = COL[1], lwd = 4)
lines(xx4, isovlow(xx4, v = 5, delta = 1.25), col = COL[1], lwd = 4)
lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 4)

#Customize ticks and labels for the plot
ticksy <- c(0, 0.5, 1)
ylabels <- c(0, expression(paste(frac(1,2))), 1)
ticksx <- c(0, 10, 20, 40)
xlabels <- c(0, expression(paste(2*delta)), expression(paste(4*delta)), 40)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Annotation of the three graphs and the NE
text(5, 0.3, expression(paste("Iso-profit: ", frac(q, p) ," = ", frac(1, 8*delta))))
text(35, 0.62, expression(paste("BRF: q = ", 1 - frac(2*delta, p))))
text(28, 0.1, expression(paste("Iso-v curve: v = ", v,"*")))
text(19, 0.55, expression(paste("NE")))

#Lines for the coordinates of the Nash equilbrium
segments(20, 0, 20, 0.5, lty = 2, col = "darkgray")
segments(0, 0.5, 20, 0.5, lty = 2, col = "darkgray")

#Add a point for the NE
points(20, 0.5, pch = 16, col = "black", cex = 1.5)

#Add a point for f. referred to in the text
points(26, 0.72, pch = 16, col = "black", cex = 1.2)
text(26.5, 0.74, expression(paste("f")))

#Arrow to Pareto-improving Lens
Arrows(20, 0.8, 23.8, 0.68, col = "black", lty = 1, lwd = 3)
text(20, 0.82, expression(paste("Pareto-Improving Lens")))

dev.off()

