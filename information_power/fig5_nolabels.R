require(ggplot2)
require(shape)
pdf(file = "information_power/fig5_nolabels.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
segmentlinewidth <- 1.5
graphlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")


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
     xlab = expression(paste("Price, ", p)),
     ylab = expression(paste("Quality, ", q)),
     xaxt = "n", yaxt = "n", cex.lab = axislabelsize, bty = "n")

npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(10, xlims[2], length.out = npts)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)

#Draw the lines for the graphs
lines(xx1, brfFn(xx1), col = COLA[5], lwd = graphlinewidth)
lines(xx2, solowCondition(xx2, delta = 5), col = COLB[4], lwd = graphlinewidth)

#Customize ticks and labels for the plot
ticksy <- c(0, 0.5, 1)
ylabels <- c(0, expression(paste(frac(1,2))), 1)
ticksx <- c(0, 10, 20, 40)
xlabels <- c(0, expression(paste(2*delta)), expression(paste(4*delta)), 40)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Annotation of the three graphs and the NE
# text(33, 0.98, expression(paste("Iso-profit: ", q," = ", frac(p, 8*delta))))
text(35, 0.64, expression(paste("Best-response")))
text(35, 0.57, expression(paste("Function: ", q == 1 - frac(2*delta, p))))
text(16, 0.52, expression(paste("Nash Equilibrium")))

#Lines for the coordinates of the Nash equilbrium
segments(20, 0, 20, 0.5, lty = 2, col = "darkgray", lwd = segmentlinewidth)
segments(0, 0.5, 20, 0.5, lty = 2, col = "darkgray", lwd = segmentlinewidth)

#Iso-profit slope annotation
text(25, 0.81, expression(paste("Slope = ", frac(p, q))))
Arrows(25, 0.79, 25, 0.66, col = "black", lty = 1, lwd = 2, arr.type = "triangle")

#BRF Slope annotation
text(25, 0.41, expression(paste("Slope = ?")))
Arrows(25, 0.43, 25, 0.56, col = "black", lty = 1, lwd = 2, arr.type = "triangle")

#Add a point for the NE
points(20, 0.5, pch = 16, col = "black", cex = 1.5)

#Add a ray and a point for d.  
segments(0, 0, 20, 1, lty = 2, lwd = 1.8, col = "darkgray")
points(10, 0.5, pch = 16, col = "black", cex = 1.2)
text(9.6, 0.53, expression(paste("d")))

#Add a ray and a point for a. 
segments(0, 0, 20, 0.25, lty = 2, lwd = 1.8, col = "darkgray")
points(11.8, 0.15, pch = 16, col = "black", cex = 1.2)
text(11.3, 0.17, expression(paste("c")))

dev.off()

