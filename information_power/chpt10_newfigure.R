require(ggplot2)
require(shape)
require(plotrix)
pdf(file = "information_power/ch10_newfigure.pdf", width = 9, height = 7)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

disutilityFn <- function(t, cbar = 1.2, underbarc = 1, z = 0) {
  (cbar - underbarc)/(t) + z
}

#COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
COL <- c("#238b45","#7fc97f", "#beaed4", "#fdc086", "#ffff99")
par(mar =  c(3, 5, 3, 7))
xlims <- c(0, 1)
ylims <- c(0,1)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste(""), cex = axislabelsize),
     ylab = expression(paste(""), cex = axislabelsize),
     xaxt = "n", yaxt = "n", cex.lab = axislabelsize, bty = "n")

npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(0, 1, length.out = npts)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
xx4 <- seq(xlims[1], 25, length.out = npts2)

#Draw the lines for the graphs
lines(xx1, disutilityFn(xx1), col = COL[1], lwd = graphlinewidth)
#lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 4)
#lines(xx2, solowInfeas(xx2, delta = 5), col = COL[1], lwd = 4, lty = 2)

#Customize ticks and labels for the plot
ticksy <- c(0,0.2,0.8,1)
ylabels <- c(NA, NA, NA, NA)
ticksx <- c(0, 0.25, 1)
xlabels <- c(0, NA, NA)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)

axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#Segments
segments(0, 0.8, 1, 0.8, lty = 2, lwd = 1.5, col = "darkgray", xpd = TRUE)
segments(0.25, 0, 0.25, 0.8, lty = 2, lwd = 1.5, col = "darkgray")
segments(0, 0.2, 1,0.2,lty = 2, lwd = 1.5, col = "darkgray")
segments(1, 0, 1,0.8, lty = 2, lwd = 1.5, col = "darkgray")

#Axis tick labels
#y
text(-0.1, 0.2, expression(paste(bar(c)+(p^z-underline(c)))), cex = labelsize, xpd = TRUE)
text(-0.05, 0.8, expression(paste(p^B)), cex = labelsize, xpd = TRUE)
#x
text(0.25, -0.07, expression(paste(underline(t))), cex = labelsize, xpd = TRUE)
text(1, -0.07, expression(paste(1)), cex = labelsize, xpd = TRUE)

#Text annotations
text(0.35, .75, expression(paste(p^N,(t))), cex = labelsize, xpd = TRUE)
text(0.35, .35, expression(paste("A's rent")), cex = labelsize, xpd = TRUE)
text(.7, .6, expression(paste("P's profit")), cex = labelsize, xpd = TRUE)
text(1.12, 0.5, expression(paste("Total surplus")), cex = labelsize, xpd = TRUE)

#Arrows
Arrows(0.32, 0.375, .32, .55, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
Arrows(0.32, 0.315, .32, .23, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
Arrows(.7, .63, 0.7, 0.76, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
Arrows(.7, .57, 0.7, 0.32, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
Arrows(1.12, 0.55, 1.12, 0.76, col = "black", lty = 1, lwd = 2, arr.type = "triangle", xpd = TRUE)
Arrows(1.12, 0.45, 1.12, 0.24, col = "black", lty = 1, lwd = 2, arr.type = "triangle", xpd = TRUE)



dev.off()

