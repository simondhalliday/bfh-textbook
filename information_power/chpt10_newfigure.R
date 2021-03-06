require(ggplot2)
require(shape)
require(plotrix)
require(pBrackets)
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
grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")

par(mar =  c(4, 6, 1, 10))
xlims <- c(0, 1)
ylims <- c(0, 1)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste(""), cex = axislabelsize),
     ylab = expression(paste(""), cex = axislabelsize),
     xaxt = "n", yaxt = "n", cex.lab = axislabelsize, bty = "n")

npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(0.2, 1, length.out = npts)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
xx4 <- seq(xlims[1], 25, length.out = npts2)

#Draw the lines for the graphs
lines(xx1, disutilityFn(xx1), col = CBCols[1], lwd = graphlinewidth)
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
segments(0, 0.8, 1.08, 0.8, lty = 2, lwd = 1.5, col = grays[20], xpd = TRUE)
segments(0.25, 0, 0.25, 0.8, lty = 2, lwd = 1.5, col = grays[20],xpd = TRUE)
segments(0, 0.2, 1.08, 0.2,lty = 2, lwd = 1.5, col = grays[20],xpd = TRUE)
segments(1, 0, 1,0.8, lty = 2, lwd = 1.5, col = grays[20],xpd = TRUE)

#Axis tick labels
#y
text(-0.15, 0.2, expression(paste(bar(u)+(p^z-underline(u)))), cex = labelsize, xpd = TRUE)
text(-0.1, 0.8, expression(paste(p^B == 1)), cex = labelsize, xpd = TRUE)
#x
text(0.25, -0.07, expression(paste(underline(t))), cex = labelsize, xpd = TRUE)
text(1, -0.07, expression(paste(1)), cex = labelsize, xpd = TRUE)

#Text annotations
text(0.375, 1, expression(paste(p^N,(t) == frac(bar(u)-underline(u),t) + p^z)), cex = labelsize, xpd = TRUE)
text(0.42, .335, expression(paste("Agent's rent")), cex = labelsize, xpd = TRUE)
text(.55, .65, expression(paste("Principal's profit")), cex = labelsize, xpd = TRUE)
text(1.25, 0.525, expression(paste("Total")), cex = labelsize, xpd = TRUE)
#text(1.3, 0.5, expression(paste("economic")), cex = labelsize, xpd = TRUE)
text(1.25, 0.475, expression(paste("rents")), cex = labelsize, xpd = TRUE)
text(1.25, 0.8, expression(paste("P's Willingness")), cex = labelsize, xpd = TRUE)
text(1.25, 0.74, expression(paste("to pay")), cex = labelsize, xpd = TRUE)

text(1.25, 0.2, expression(paste("A's Willingness")), cex = labelsize, xpd = TRUE)
text(1.25, 0.14, expression(paste("to sell")), cex = labelsize, xpd = TRUE)


#Arrows
Arrows(0.4, 0.375, .4, .45, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
Arrows(0.4, 0.315, .4, .23, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
Arrows(.4, .68, 0.4, 0.78, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
Arrows(.4, .62, 0.4, 0.55, col = "black", lty = 1, lwd = 2, arr.type = "triangle")

#Brackets
brackets(x1 = 1.12, y1 = 0.76, x2 = 1.12, y2 = 0.24, ticks = 0.5, curvature = 0.5, type = 1, 
         col = "black", lwd = 2, lty = 1, xpd = TRUE)


#Axes labels
text(0.5, -0.15, expression(paste("Degree of contractual completeness, t")), xpd = TRUE, cex = axislabelsize) 
text(-0.15, 0.5, expression(paste("Price, p")), xpd = TRUE, cex = axislabelsize,srt = 90) 


dev.off()

