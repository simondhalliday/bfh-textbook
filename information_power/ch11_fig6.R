require(ggplot2)
require(shape)
pdf(file = "information_power/ch11_fig6.pdf", width = 9, height = 7)

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

isovhigh <- function(p, delta = 5, v = 20){
  (sqrt(p^2 - 4 * delta * v) - p + 2*v)/( 2 * v )
}

isovlow <- function(p, delta = 5, v = 20){
  (-sqrt(p^2 - 4 * delta * v) - p + 2*v)/( 2 * v )
}


isovhigh1 <- function(p, delta = 5, v = 30){
  (sqrt(p^2 - 4 * delta * v) - p + 2*v)/( 2 * v )
}

isovlow1 <- function(p, delta = 5, v = 30){
  (-sqrt(p^2 - 4 * delta * v) - p + 2*v)/( 2 * v )
}

solowCondition <- function(p, delta = 5){
  (p*(1/(8*delta)))
}

#COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
#COL <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00")
COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0")
#COL <- c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4", "#fed9a6", "#ffffcc", "#e5d8bd", "#fddaec")
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
xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
xx4 <- seq(xlims[1], 25, length.out = npts2)
xx5 <- seq(xlims[1], xlims[2], length.out = npts2)
xx6 <- seq(xlims[1], 35, length.out = npts2)
yy1 <- isovhigh(xx3, delta = 5, v = 20)

#Below, note I have gone to number 416 because at that observation we can 
#find the closest outcome to the intersection of the curves. Serves fine 
#for the relevant polygon
#Draw the shaded area of the Pareto-improving lens
#For the polygon need the intersection of v and iso-profit
#(2p - 40)^2 = p^2 - 400
#4p^2 - 160p + 1600 = p^2 - 400
#3p^2 - 160p + 2000 = 0
#p = 20 or p = 100/3 = 33.3 
# => q = (1/40)*33.3 = 0.8325
polygon(c(xx3, xx3[416]), c(yy1, yy1[416]), col=COL[4], density=NULL, border = NA)

#Draw the lines for the graphs
lines(xx1, brfFn(xx1), col = COLA[5], lwd = graphlinewidth)
lines(xx3, isovhigh(xx3, v = 20, delta = 5), col = COL[1], lwd = graphlinewidth)
lines(xx4, isovlow(xx4, v = 20, delta = 5), col = COL[1], lwd = graphlinewidth)
lines(xx2, solowCondition(xx2, delta = 5), col = COLB[4], lwd = graphlinewidth)
lines(xx5, isovhigh1(xx5, v = 30, delta = 5), col = COL[1], lwd = graphlinewidth, lty = 2)
lines(xx6, isovlow1(xx6, v = 30, delta = 5), col = COL[1], lwd = graphlinewidth, lty = 2)


#Customize ticks and labels for the plot
ticksy <- c(0, 0.5, 1)
ylabels <- c(0, expression(paste(frac(1,2))), 1)
ticksx <- c(0, 10, 20, 40)
xlabels <- c(0, expression(paste(2*delta)), expression(paste(4*delta)), 40)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Annotation of the three graphs and the NE
text(33, 0.98, expression(paste("Iso-profit: ", q ," = ", frac(p, 8*delta))))
text(35, 0.62, expression(paste("Best Response Function")))
text(35, 0.55, expression(paste("q = ", 1 - frac(2*delta, p))))
text(26.5, 0.1, expression(paste("Iso-v curve")))
text(26.5, 0.05, expression(paste("v = ", v,"*")))
text(36, 0.1, expression(paste("Iso-v curve")))
text(36, 0.05, expression(paste("v = ", v^"+")))
text(16, 0.52, expression(paste("Nash Equilibrium")))

#Lines for the coordinates of the Nash equilbrium
segments(20, 0, 20, 0.65, lty = 2, col = "darkgray")
segments(0, 0.5, 20, 0.5, lty = 2, col = "darkgray")

#Add a point for the NE
points(20, 0.5, pch = 16, col = "black", cex = 1.5)

#Add a point for f. referred to in the text
points(26.2, 0.72, pch = 16, col = "black", cex = 1.2)
text(26.5, 0.7, expression(paste("f")))

#Arrow to Pareto-improving Lens
Arrows(20, 0.68, 23.8, 0.68, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(14.5, 0.68, expression(paste("Pareto-Improving Lens")))

dev.off()

