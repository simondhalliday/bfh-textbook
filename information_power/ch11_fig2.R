require(ggplot2)
require(shape)
require(plotrix)
pdf(file = "information_power/ch11_fig2.pdf", width = 9, height = 7)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

disutilityFn <- function(q, delta = 5) {
  delta /(1 - q)
}

#COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
COL <- c("#238b45","#7fc97f", "#beaed4", "#fdc086", "#ffff99")
par(mar =  c(3, 3, 1, 1))
xlims <- c(0, 1.2)
ylims <- c(0, 40)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste(""), cex = axislabelsize),
     ylab = expression(paste(""), cex = axislabelsize),
     xaxt = "n", yaxt = "n", cex.lab = axislabelsize, bty = "n")

npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], 0.9, length.out = npts)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
xx4 <- seq(xlims[1], 25, length.out = npts2)

#Draw the lines for the graphs
lines(xx1, disutilityFn(xx1), col = COL[1], lwd = graphlinewidth)
#lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 4)
#lines(xx2, solowInfeas(xx2, delta = 5), col = COL[1], lwd = 4, lty = 2)

#Customize ticks and labels for the plot
ticksy <- c(0, 5, 40)
ylabels <- c(0, 5, 40)
ticksx <- c(0, 1, 1.1)
xlabels <- c(0, 1.0, NA)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

# Axis labels 
text(0.5*xlims[2], -3.5, expression(paste("Quality, ", q)), xpd = TRUE, cex = axislabelsize) 
text(-0.115, 0.55*ylims[2], expression(paste("Disutility of providing quality, ", u)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Annotation of the  graphs
text(0.65, 35, expression(paste("Distutility = ", frac(underline("u"), (1 - q)))), cex = labelsize)

#Line for the absolute maximum quality
segments(1, 0, 1, 42, lty = 2, lwd = 1.5, col = "darkgray")

#Arrow to Slope of BRF
Arrows(0.5, 13, 0.58, 13, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(0.27, 13, expression(paste("Slope = " - u[q], " = ", frac(underline("u"), (1 - q)^2))), cex = labelsize)

#Text to indicate delta = 5
text(0.2, 38, expression(paste("Disutility of effort")), cex = labelsize)
text(0.2, 36, expression(paste("set to ", underline("u"), " = 5")), cex = labelsize)

#Text for max quality
text(1.1, 38, expression(paste("Maximum")), cex = labelsize)
text(1.1, 36, expression(paste("level of")), cex = labelsize)
text(1.1, 34, expression(paste("quality, q")), cex = labelsize)


dev.off()

