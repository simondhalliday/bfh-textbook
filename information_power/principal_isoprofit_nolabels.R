require(ggplot2)
require(shape)
pdf(file = "information_power/principal_isoprofit_nolabels.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5

solowCondition <- function(p, delta = 5, slope = 8){
  (p*(1/(slope*delta)))
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

npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(10, xlims[2], length.out = npts)
xx2 <- seq(1, xlims[2], length.out = npts)

#Draw the lines for the graphs
lines(xx2, solowCondition(xx2, delta = 5, slope = 4), col = COLB[4], lwd = graphlinewidth)
lines(xx2, solowCondition(xx2, delta = 5), col = COLB[4], lwd = graphlinewidth)
lines(xx2, solowCondition(xx2, delta = 5, slope = 16), col = COLB[4], lwd = graphlinewidth)

#Customize ticks and labels for the plot
ticksy <- c(0, 0.5, 1)
ylabels <- c(0, expression(paste(frac(1,2))), 1)
ticksx <- c(0, 10, 20, 40)
xlabels <- c(0, 10, 20, 40)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

mtext(expression(paste("Price, ", p)), side=1, line = 2.5, cex = axislabelsize)
text(-4, ylims[2] - 0.5*(ylims[2] - ylims[1]), expression(paste("Quality, ", q)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Annotation of the three graphs and the NE
# text(18, 0.98, expression(paste(pi[3])))
# text(38, 0.98, expression(paste(pi[2])))
# text(38, 0.45, expression(paste(pi[1])))

#Lines for the coordinates of the Nash equilbrium
segments(0, 0.5, 20, 0.5, lty = 2, col = "darkgray")
segments(10, 0, 10, 0.5, lty = 2, col = "darkgray")

#Add a point for the NE
points(20, 0.5, pch = 16, col = "black", cex = 1.5)
text(20 + 0.5, 0.5 - 0.025, expression(paste(d)))

points(10, 0.5, pch = 16, col = "black", cex = 1.5)
text(10 + 0.5, 0.5 - 0.025, expression(paste(c)))

points(10, 0.25, pch = 16, col = "black", cex = 1.5)
text(10 + 0.5, 0.25  - 0.025, expression(paste(b)))

points(10, 0.125, pch = 16, col = "black", cex = 1.5)
text(10 + 0.5, 0.125 - 0.025, expression(paste(a)))

#Arrow to Slope of BRF
# Arrows(14.2, 0.12, 12.2, 0.12, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
# text(16.8, 0.12, expression(paste("Slope = ", q[p])))

#Arrow to Slope of isoprofit
Arrows(13, 0.80, 15, 0.80, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(10.2, 0.80, expression(paste("Slope = ", frac(q, p))))

dev.off()

