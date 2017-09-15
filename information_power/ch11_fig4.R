require(ggplot2)
require(shape)
pdf(file = "information_power/ch11_fig4.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
segmentlinewidth <- 2

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



#COL <- c("#bae4b3", "#74c476", "#238b45")
#COL <- c("#f7fcf0", "#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe", "#08589e")
COL <- c("#f7fcf5", "#e5f5e0", "#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#005a32")
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

#Draw the lines for the graphs
lines(xx0, isov(xx0, delta = 5), col = COL[3], lwd = 4)
lines(xx1, brfFn(xx1), col = "#beaed4", lwd = 4)
lines(xx2, tangencyLine(xx2), col = "darkgrey", lwd = segmentlinewidth, lty = 2)
lines(xx3, isovhigh1(xx3, v = 5, delta = 5), col = COL[4], lwd = 4)
lines(xx4, isovlow1(xx4, v = 5, delta = 5), col = COL[4], lwd = 4)
lines(xx5, isovhigh2(xx5, v = 10, delta = 5), col = COL[5], lwd = 4)
lines(xx6, isovlow2(xx6, v = 10, delta = 5), col = COL[5], lwd = 4)
lines(xx7, isovhigh3(xx7, v = 20, delta = 5), col = COL[6], lwd = 4)
lines(xx8, isovlow3(xx8, v = 20, delta = 5), col = COL[6], lwd = 4)

#Customize ticks and labels for the plot
ticksy <- c(0, 0.5, 1)
ylabels <- c(0, expression(paste(frac(1,2))), 1)
ticksx <- c(0, 5, 10, 20, 40)
xlabels <- c(0, expression(paste(delta)), expression(paste(p[0] == 2*delta)), expression(paste(underline(p) == 4*delta)) , 40)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Annotation of the three graphs and the NE
#text(5, 0.3, expression(paste("Iso-profit: ", frac(q, p) ," = ", frac(1, 8*delta))))
#text(35, 0.62, expression(paste("BRF: q = ", 1 - frac(2*delta, p))))
text(3.9, 0.05, expression(paste(v[0],  " = z")))
text(9.5, 0.05, expression(paste(v[1])))
text(14, 0.05, expression(paste(v[2])))
text(23.5, 0.05, expression(paste(v[3])))
text(21, 0.48, expression(paste("a")))
text(35, 0.62, expression(paste("Best Response Function")))
text(36, 0.9, expression(paste("Iso-v curves")))

#Lines for the coordinates of the Nash equilbrium
#segments(5, 0, 5, 1, lty = 2, col = "darkgray", lwd = 3)
segments(10, 0, 10, 1, lty = 2, col = "darkgray", lwd = segmentlinewidth)
segments(20, 0, 20, 1, lty = 2, col = "darkgray", lwd = segmentlinewidth)
segments(0, 0.5, 20, 0.5, lty = 2, col = "darkgray", lwd = segmentlinewidth)
segments(14.14214, 0.15, 14.14214, 0.45, lty = 2, col = "darkgray", lwd = segmentlinewidth)

#Arrows and rent label
Arrows(15, 0.8, 19.3, 0.8,  col = "black", lty = 1, lwd = 2, arr.type = "triangle")
Arrows(15, 0.8, 10.8, 0.8,  col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(14.8, 0.9, expression(paste("Rent at")))
text(15, 0.85, expression(paste("point a.")))

#Arrows and slope of iso-v label
Arrows(29, 0.15, 24, 0.15,  col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(32, 0.2, expression(paste("Slope of iso-v")))
text(32.3, 0.15, expression(paste(" = MRS ")))
text(32, 0.08, expression(paste(" = " -frac(v[p], v[q]))))



#Add a point for the NE
points(20, 0.5, pch = 16, col = "black", cex = 1.5)


#Add a point for b
points(10, 0.5, pch = 16, col = "black", cex = 1.5)
text(9.5, 0.53, expression(paste("b")))

#Add a point for c
#Figure out q for p = 14.14214: q = 1 - 2delta/p = 1 - (2*5)/14.14214 =  0.2928934
points(14.14214, 0.2928934, pch = 16, col = "black", cex = 1.5)
text(15, 0.2928934, expression(paste("c")))

#Add a point for f. referred to in the text
#points(12, 0.82, pch = 16, col = "black", cex = 1.2)
#text(12, 0.85, expression(paste("NE")))

#Arrow to Pareto-improving Lens
#Arrows(20, 0.8, 23.8, 0.68, col = "black", lty = 1, lwd = 3)
#text(20, 0.82, expression(paste("Pareto-Improving Lens")))

dev.off()