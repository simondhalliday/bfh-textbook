require(ggplot2)
require(shape)
pdf(file = "information_power/principal_isoprofit.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")


solowCondition <- function(p, delta = 5, slope = 8){
  (p*(1/(slope*delta)))
}

par(mar =  c(4, 4, 0.5, 0.5))
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
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = annotatesize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = annotatesize)

mtext(expression(paste("Price, ", p)), side=1, line = 2.5, cex = axislabelsize)
text(-4, ylims[2] - 0.5*(ylims[2] - ylims[1]), expression(paste("Quality, ", q)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Annotation of the three graphs and the NE
text(18, 0.98, expression(paste(pi[3])), cex = annotatesize)
text(37, 0.98, expression(paste(pi[2])), cex = annotatesize)
text(39, 0.525, expression(paste(pi[1])), cex = annotatesize)

#Lines for the coordinates of the Nash equilbrium
segments(0, 0.5, 20, 0.5, lty = 2, col = "darkgray", lwd = segmentlinewidth)
segments(10, 0, 10, 0.5, lty = 2, col = "darkgray", lwd = segmentlinewidth)

#Add a point for the NE
points(20, 0.5, pch = 16, col = "black", cex = 1.5)
text(20 + 0.5, 0.5 - 0.025, expression(paste(d)), cex = annotatesize)

points(10, 0.5, pch = 16, col = "black", cex = 1.5)
text(10 + 0.75, 0.5 - 0.025, expression(paste(c)), cex = annotatesize)

points(10, 0.25, pch = 16, col = "black", cex = 1.5)
text(10 + 0.75, 0.25  - 0.025, expression(paste(b)), cex = annotatesize)

points(10, 0.125, pch = 16, col = "black", cex = 1.5)
text(10 + 0.75, 0.125 - 0.025, expression(paste(a)), cex = annotatesize)

#Arrow to Slope of BRF
# Arrows(14.2, 0.12, 12.2, 0.12, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
# text(16.8, 0.12, expression(paste("Slope = ", q[p])))

#Arrow to Slope of isoprofit
Arrows(12, 0.80, 15, 0.80, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(8.5, 0.80, expression(paste("Slope = ", frac(q, p))), cex = annotatesize)

dev.off()

