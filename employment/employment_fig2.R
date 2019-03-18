require(ggplot2)
require(shape)
pdf(file = "employment/employment_fig2.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5


brfFn <- function(w, delta = 5) {
  1 - (2 * delta) /w
}

solowCondition <- function(w, delta = 5){
  (w*(1/(8*delta)))
}

#COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")
par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 40)
ylims <- c(0, 1)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("Wage, ", w)),
     ylab = expression(paste("Effort, ", e)),
     xaxt = "n", yaxt = "n", 
     cex.lab = axislabelsize, 
     line = 2.5,
     bty = "n", 
     xaxs="i", 
     yaxs="i")

npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(10, xlims[2], length.out = npts)
xx2 <- seq(xlims[1]+0.5, xlims[2], length.out = npts)

#Draw the lines for the graphs
lines(xx1, brfFn(xx1), col = COLA[4], lwd = graphlinewidth)
lines(xx2, solowCondition(xx2, delta = 5), col = COLB[4], lwd = graphlinewidth)

#Customize ticks and labels for the plot
ticksy <- c(0, 0.5, 1)
ylabels <- c(0, NA, 1)
ticksx <- c(0, 10, 20, 40)
xlabels <- c(0, expression(paste(2*delta)), expression(paste(4*delta)), NA)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Annotation of the three graphs and the NE
text(31, 0.96, expression(paste("Maximum Iso-profit: ", e == frac(w, 8*delta))))
text(35.5, 0.65, expression(paste("Employee's")))
text(35.5, 0.61, expression(paste("Best Response")))
text(35.5, 0.56, expression(paste("Function ", e == 1 - frac(2*delta, w))))
text(16, 0.52, expression(paste("Nash Equilibrium")))
text(20.5, 0.48, expression(n))

#Lines for the coordinates of the Nash equilbrium
segments(20, 0, 20, 0.5, lty = 2, col = "darkgray", lwd = segmentlinewidth)
segments(0, 0.5, 20, 0.5, lty = 2, col = "darkgray", lwd = segmentlinewidth)

#Iso-profit slope annotation
text(25, 0.81, expression(paste("Slope = ", frac(1, 8*delta) == frac(e,w))))
Arrows(25, 0.79, 25, 0.66, col = "black", lty = 1, lwd = 2, arr.type = "triangle")

#BRF Slope annotation
text(25, 0.41, expression(paste("Slope = ", frac(2*delta, w^2) == paste(e[w]))))
Arrows(25, 0.43, 25, 0.56, col = "black", lty = 1, lwd = 2, arr.type = "triangle")

#Add a point for the NE
points(20, 0.5, pch = 16, col = "black", cex = 1.5)

#Add a ray and a point for d.  
segments(0.5, 0.01, 20, 1, lty = 2, lwd = graphlinewidth, col = "darkgray")
text(15.8, 0.95, expression(paste("Infeasible")))
text(15.8, 0.91, expression(paste("Iso-profit")))
#points(10, 0.5, pch = 16, col = "black", cex = 1.2)
#text(9.6, 0.53, expression(d))

#Add a ray and a point for a. 
segments(0.5, 0.01, 22.5, 0.28125, lty = 2, lwd = graphlinewidth, col = "darkgray")
points(11.8, 0.15, pch = 16, col = "black", cex = 1.2)
text(11.3, 0.17, expression(paste("c")))

dev.off()

