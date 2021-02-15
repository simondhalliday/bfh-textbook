#' Graph Designer: Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics
require(plotrix)
require(ggplot2)
require(shape)
require(extrafontdb)
require(pBrackets)
pdf(file = "information_power/iso_vmap_b_checkpoint.pdf", width = 9, height = 7)

#Set parameters for graphics
pointsize <- 1.8
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
grays <- gray.colors(25, start = 1, end = 0)


#The equation is below when v = 0. See Wolfram Alpha output. 
isov <- function(p, delta = 5) {
  (p - delta)/p
}

isovhigh1 <- function(p, delta = 5, v = 5){
  (sqrt(p^2 - 4 * delta * v) - p + 2*v)/( 2 * v )
}

isovlow1 <- function(p, delta = 5, v = 5){
  (-sqrt(p^2 - 4 * delta * v) - p + 2*v)/( 2 * v )
}

isovhigh2 <- function(p, delta = 5, v = 10){
  (sqrt(p^2 - 4 * delta * v) - p + 2*v)/( 2 * v )
}

isovlow2 <- function(p, delta = 5, v = 10){
  (-sqrt(p^2 - 4 * delta * v) - p + 2*v)/( 2 * v )
}

isovhigh3 <- function(p, delta = 5, v = 15){
  (sqrt(p^2 - 4 * delta * v) - p + 2*v)/( 2 * v )
}

isovlow3 <- function(p, delta = 5, v = 15){
  (-sqrt(p^2 - 4 * delta * v) - p + 2*v)/( 2 * v )
}

brfFn <- function(p, delta = 5) {
  1 - (2*delta) /p
}

#This is evaluated for p = 12; q = 12/(8*sqrt(12^2 - 4*delta1*v1)) - 1/8 for the slope
#At p = 12; q = (-sqrt(12^2 - 4*v1*delta1) - 12 + 2*v1)/(2*v1) = 0.799
tangencyLine <- function(p){
  (p*(0.05))
}


par(mar =  c(4, 4, .5, .5))

xlims <- c(0, 40)
ylims <- c(0, 1)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = "",
     ylab = "",
     xaxt = "n", yaxt = "n", cex.lab = axislabelsize, bty = "n")

mtext(expression(paste("Price, ", p)), side = 1, line = 2.5, cex = axislabelsize)
text(-4, ylims[2] - 0.5*(ylims[2] - ylims[1]), expression(paste("Quality, ", q)), xpd = TRUE, cex = axislabelsize, srt = 90) 

npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx0 <- seq(5, xlims[2], length.out = npts)
xx1 <- seq(10, xlims[2], length.out = npts)
xx2 <- seq(xlims[1], 15, length.out = npts)
xx3 <- seq(10, xlims[2], length.out = npts2)
xx4 <- seq(xlims[1], 10, length.out = npts2)
#For below, I solved sqrt(p^2 - 200) - p + 200/p = 0, getting p = 10*sqrt(2) = 14.14214
xx5 <- seq(14.14214, xlims[2], length.out = npts2)
xx6 <- seq(14.14214, 15, length.out = npts2)
xx7 <- seq(20, xlims[2], length.out = npts2)
xx8 <- seq(xlims[1], 25, length.out = npts2)

#Customize ticks and labels for the plot
ticksy <- c(0, 0.5, 1)
ylabels <- c(0, expression(paste(frac(1,2))), 1)
ticksx <- c(0, 5, 10, 20, 40)
xlabels <- c(0, expression(paste(underline("u"))), expression(paste(p[0] == 2*underline("u"))), expression(paste(p == 4*underline("u"))) , 40)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#Lines for the coordinates of the Nash equilbrium
segments(0, 0, 20, 0.5, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(10, 0, 10, 1, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(20, 0, 20, 1, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, 0.5, 20, 0.5, lty = 2, col = grays[20], lwd = segmentlinewidth)

#Draw the lines for the graphs
#lines(xx2, tangencyLine(xx2), col = grays[20], lwd = segmentlinewidth, lty = 2)
lines(xx0, isov(xx0, delta = 5), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfFn(xx1), col = COL[2], lwd = graphlinewidth)
#lines(xx3, isovhigh1(xx3, v = 5, delta = 5), col = COLA[4], lwd = graphlinewidth)
#lines(xx4, isovlow1(xx4, v = 5, delta = 5), col = COLA[4], lwd = graphlinewidth)
#lines(xx5, isovhigh2(xx5, v = 10, delta = 5), col = COLA[4], lwd = graphlinewidth)
#lines(xx6, isovlow2(xx6, v = 10, delta = 5), col = COLA[4], lwd = graphlinewidth)
lines(xx7, isovhigh3(xx7, v = 20, delta = 5), col = COLA[4], lwd = graphlinewidth)
lines(xx8, isovlow3(xx8, v = 20, delta = 5), col = COLA[4], lwd = graphlinewidth)

#Annotation of the three graphs and the NE
text(3.4, 0.05, expression(paste(v[0] == 0)), cex = annotatesize)
#text(9.0, 0.05, expression(paste(v[1])), cex = annotatesize)
#text(14, 0.05, expression(paste(v[2])), cex = annotatesize)
text(23.5, 0.05, expression(paste(v[4])), cex = annotatesize)
text(21, 0.48, expression(paste("n")), cex = annotatesize)
text(35, 0.63, expression(paste("Best-response")), cex = annotatesize)
text(35, 0.58, expression(paste("function (ICC)")), cex = annotatesize)
text(36, 0.9, expression(paste("Iso-v curves")), cex = annotatesize)
text(4.5, 0.4, expression(paste("Participation ")), cex = annotatesize)
text(4.5, 0.35, expression(paste("constraint")), cex = annotatesize)
text(4.5, 0.3, expression(paste("(PC)")), cex = annotatesize)

#Arrows and rent label
Arrows(15, 0.8, 19.3, 0.8,  col = "black", lty = 1, lwd = 2, arr.type = "triangle")
Arrows(15, 0.8, 10.8, 0.8,  col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(14.8, 0.9, expression(paste("Rent at")), cex = annotatesize)
text(15, 0.85, expression(paste("point n")), cex = annotatesize)

#Add a point for the NE
points(20, 0.5, pch = 16, col = "black", cex = 1.5)


#Add a point for b
points(10, 0.5, pch = 16, col = "black", cex = 1.5)
text(9.5, 0.53, expression(paste("c")), cex = annotatesize)

#Add a point for d
#Figure out q for p = 14.14214: q = 1 - 2delta/p = 1 - (2*5)/14.14214 =  0.2928934
#points(14.14214, 0.2928934, pch = 16, col = "black", cex = 1.5)
#text(15, 0.2928934, expression(paste("d")), cex = annotatesize)


brackets(19, 0.5, 19, 0.73, h = 0.5, ticks = 0.5, curvature = 0.5, type = 1, col = 1, lwd = 2, lty = 1, xpd = FALSE)
text(15.5, 0.65, expression(paste("Additional")), cex = annotatesize)
text(15.5, 0.6, expression(paste("quality")), cex = annotatesize)

dev.off()
