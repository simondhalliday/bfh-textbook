#' Graph Designer: Harriet Brookes-Gray
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(ggplot2)
require(shape)
pdf(file = "employment/employment_brf_rentv2.pdf", width = 9, height = 7)

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


par(mar =  c(4, 4, 4, 3.5))
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
ylabels <- c(0, expression(paste(e^N)), 1)
ticksx <- c(0, 5, 10, 20, 40)
xlabels <- c(0, expression(paste(underline("u"))),expression(paste(2*underline("u"))) , expression(paste(w^N)) , NA)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#Lines for the coordinates of the Nash equilbrium
segments(0, 0, 20, 0.5, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(10, 0, 10, 1, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(20, 0, 20, 1, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, 0.5, 20, 0.5, lty = 2, col = grays[20], lwd = segmentlinewidth)

#Draw the lines for the graphs
lines(xx2, tangencyLine(xx2), col = grays[20], lwd = segmentlinewidth, lty = 2)
lines(xx0, isov(xx0, delta = 5), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfFn(xx1), col = COL[2], lwd = graphlinewidth)
#lines(xx3, isovhigh1(xx3, v = 5, delta = 5), col = COLA[4], lwd = graphlinewidth)
#lines(xx4, isovlow1(xx4, v = 5, delta = 5), col = COLA[4], lwd = graphlinewidth)
#lines(xx5, isovhigh2(xx5, v = 10, delta = 5), col = COLA[4], lwd = graphlinewidth)
#lines(xx6, isovlow2(xx6, v = 10, delta = 5), col = COLA[4], lwd = graphlinewidth)
lines(xx7, isovhigh3(xx7, v = 20, delta = 5), col = COLA[4], lwd = graphlinewidth)
lines(xx8, isovlow3(xx8, v = 20, delta = 5), col = COLA[4], lwd = graphlinewidth)

text(3.4, 0.05, expression(paste(v[0] == 0)), cex = labelsize)
#text(22, 0.05, expression(paste(v[1])), cex = labelsize)
text(25, 0.05, expression(paste(v[4])),cex = labelsize)
text(37, 0.62, expression(paste("Employee's best-response")), cex = labelsize, xpd =TRUE)
text(37, 0.57, expression(paste("function (ICC)")),cex = labelsize, xpd =TRUE)
#text(35, 0.58, expression(paste(e(Delta, a))))
text(36, 0.94, expression(paste("Employee's")),cex = labelsize)
text(36, 0.9, expression(paste("iso-v curves")),cex = labelsize)

text(19.3, 0.52, expression(n),cex = labelsize)
text(26.5, 0.48, expression(paste("Incomplete contract")),cex = labelsize)
text(26.5, 0.43, expression(paste("Nash equilibrium")),cex = labelsize)

text(15, 0.89, expression(paste("Rent at incomplete contract")),cex = labelsize)
text(15, 0.84, expression(paste("Nash equilibrium")),cex = labelsize)
Arrows(13, 0.8, 19.3, 0.8,  col = "black", lty = 1, lwd = 2, arr.type = "triangle")
Arrows(14, 0.8, 11, 0.8,  col = "black", lty = 1, lwd = 2, arr.type = "triangle")


Arrows(28, 0.15, 24, 0.15,  col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(32, 0.2, expression(paste("Slope of iso-v")),cex = labelsize)
text(32.3, 0.15, expression(paste(phantom() == "-mrs(w,e) ")),cex = labelsize)
text(32, 0.06, expression(paste(phantom() == -frac(v[w], v[e]))),cex = labelsize)



#Add a point for the NE
points(20, 0.5, pch = 16, col = "black", cex = 1.5)

#segments(12.5, 0, 12.5, ylims[2], lty = 2, col = grays[20], lwd = 2)
#segments(0, brfFn(w = 18.4), 18.4, brfFn(w = 18.4), lty = 2, col = "darkgray", lwd = 2)
points(10, 0.5, pch = 16, col = "black", cex = 1.5)
text(9.2, 0.53, expression(c),cex = labelsize)



dev.off()
