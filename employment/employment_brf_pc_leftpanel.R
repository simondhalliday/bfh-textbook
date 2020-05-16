require(shape)
pdf(file = "employment/employment_brf_pc_leftpanel.pdf", width = 9, height = 7)

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
grays <- gray.colors(25, start = 1, end = 0)

#The equation is below when v = 0. See Wolfram Alpha output. 
isov <- function(w, delta = 5) {
}

isovhigh1 <- function(w, delta = 5, v = 5){
  (sqrt(w^2 - 4 * delta * v) - w + 2*v)/( 2 * v )
}

isovlow1 <- function(w, delta = 5, v = 5){
  (-sqrt(w^2 - 4 * delta * v) - w + 2*v)/( 2 * v )
}

isovhigh2 <- function(w, delta = 5, v = 10){
  (sqrt(w^2 - 4 * delta * v) - w + 2*v)/( 2 * v )
}

isovlow2 <- function(w, delta = 5, v = 10){
  (-sqrt(w^2 - 4 * delta * v) - w + 2*v)/( 2 * v )
}

isovhigh3 <- function(w, delta = 5, v = 15){
  (sqrt(w^2 - 4 * delta * v) - w + 2*v)/( 2 * v )
}

isovlow3 <- function(w, delta = 5, v = 15){
  (-sqrt(w^2 - 4 * delta * v) - w + 2*v)/( 2 * v )
}


isovhigh4 <- function(p, delta = 5, v = 15){
  (sqrt(p^2 - 4 * delta * v) - p + 2*v)/( 2 * v )
}

isovlow4 <- function(p, delta = 5, v = 15){
  (-sqrt(p^2 - 4 * delta * v) - p + 2*v)/( 2 * v )
}

brfFn <- function(w, delta = 5) {
  1 - (2*delta) /w
}

#This is evaluated for p = 12; q = 12/(8*sqrt(12^2 - 4*delta1*v1)) - 1/8 for the slope
#At p = 12; q = (-sqrt(12^2 - 4*v1*delta1) - 12 + 2*v1)/(2*v1) = 0.799
tangencyLine <- function(w){
  (w*(0.05))
}



COL <- c("#f7fcf5", "#e5f5e0", "#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#005a32")
grays <- gray.colors(25, start = 1, end = 0)
par(mar =  c(5, 7, 1, 5))
xlims <- c(0, 40)
ylims <- c(0, 1)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("Wage, ", w)),
     ylab = expression(paste("")),
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     lwd = 3,
     bty = "n", 
     xaxs="i", 
     yaxs="i")


npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx0 <- seq(5, xlims[2], length.out = npts)
xx1 <- seq(10, xlims[2], length.out = npts)
xx2 <- seq(5, 15, length.out = npts)
xx3 <- seq(10, xlims[2], length.out = npts2)
xx4 <- seq(xlims[1], 10, length.out = npts2)
#For below, I solved sqrt(p^2 - 200) - p + 200/p = 0, getting p = 10*sqrt(2) = 14.14214
xx5 <- seq( 18.43909, xlims[2], length.out = npts2)
xx6 <- seq( 18.43909, 25, length.out = npts2)
xx7 <- seq(20, xlims[2], length.out = npts2)
xx8 <- seq(xlims[1], 25, length.out = npts2)

text(-7,0.5*ylims[2], expression(paste("Effort, ", e)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Draw the lines for the graphs
#lines(xx0, isov(xx0, delta = 5), col = COL[3], lwd = graphlinewidth)
#lines(xx1, brfFn(xx1), col = "#beaed4", lwd = graphlinewidth)
#lines(xx2, tangencyLine(xx2), col = "darkgrey", lwd = 3, lty = 2)
lines(xx3, isovhigh1(xx3, v = 5, delta = 5), col = COLA[4], lwd = graphlinewidth)
lines(xx4, isovlow1(xx4, v = 5, delta = 5), col = COLA[4], lwd = graphlinewidth)
lines(xx5, isovhigh2(xx5, v = 17, delta = 5), col = COLA[4], lwd = graphlinewidth)
lines(xx6, isovlow2(xx6, v = 17, delta = 5), col = COLA[4], lwd = graphlinewidth)
lines(xx7, isovhigh3(xx7, v = 20, delta = 5), col = COLA[4], lwd = graphlinewidth)
lines(xx8, isovlow3(xx8, v = 20, delta = 5), col = COLA[4], lwd = graphlinewidth)

#Customize ticks and labels for the plot
ticksy <- c(0, NA, 0.5, 1)
#ylabels <- c(0, expression(paste(frac(1,2))), 1)
ylabels <- c(0, NA, expression(paste(e^N == frac(1,2))), 1)
ticksx <- c(0, 10, NA, 20, 40)
xlabels <- c(0, expression(paste(w == 2, underline(u))), NA, expression(paste(w^N)) , NA)
#ticksx <- c(0, 10, 18.4, 20, 40)
#xlabels <- c(0, expression(paste(w == 2, underline(u))), expression(paste(w^g)), expression(paste(w^N)) , NA)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1,cex.axis = labelsize)

#Annotation of the three graphs and the NE
text(9.25, 0.05, expression(paste(v[1])),cex = labelsize, xpd = TRUE)
text(22.5, 0.05, expression(paste(v[2])),cex = labelsize, xpd = TRUE)
text(25.3, 0.05, expression(paste(v[3])),cex = labelsize, xpd = TRUE)
#text(37.5, 0.66, expression(paste("Employee's ICC, or")),cex = labelsize,xpd = TRUE)
#text(37.5, 0.61, expression(paste("Best-response function")),cex = labelsize,xpd = TRUE)
#text(37.5, 0.56, expression(paste(e(w))),cex = labelsize,xpd = TRUE)

#Lines for the coordinates of the Nash equilbrium
#segments(5, 0, 5, 1, lty = 2, col = "darkgray", lwd = 3)
#segments(10, 0, 10, 0.2, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(20, 0, 20, 0.75, lty = 2, col = grays[20], lwd = segmentlinewidth)
#segments(0, 0.5, 20, 0.5, lty = 2, col = grays[20], lwd = segmentlinewidth)
#segments(14.14214, 0.15, 14.14214, 0.45, lty = 2, col = "darkgray", lwd = 3)
text(19.5, 0.52, expression(n), cex = labelsize)
#text(27, 0.48, expression(paste("Incomplete contract")), cex = labelsize)
#text(27, 0.43, expression(paste("Nash equilibrium")), cex = labelsize)


#Arrows and slope of iso-v label
Arrows(29, 0.15, 24, 0.15,  col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(32, 0.2, expression(paste("Slope of iso-v")), cex = labelsize)
text(32.3, 0.15, expression(paste(phantom() == "-mrs ")), cex = labelsize)
text(32, 0.08, expression(paste(phantom() == -frac(v[w], v[e]))),cex = labelsize)



#Add a point for the NE
points(20, 0.5, pch = 16, col = "black", cex = 1.5)


#segments(18.4, 0, 18.4, 0.75, lty = 2, col = grays[20], lwd = segmentlinewidth)
#segments(0, brfFn(w = 18.4), 18.4, brfFn(w = 18.4), lty = 2, col = grays[20], lwd = segmentlinewidth)
#points(18.4, brfFn(w = 18.4), pch = 16, col = "black", cex = 1.5)
#text(17.5, brfFn(18.4) + 0.02, expression(g), cex = labelsize)



#Add a point for h
#Figure out q for p = 14.14214: q = 1 - 2delta/p = 1 - (2*5)/14.14214 =  0.2928934
points(20, 0.1839422, pch = 16, col = "black", cex = 1.5)
text(20.8, 0.1839422, expression(h), cex = labelsize)

points(22, isovhigh3(22, v = 20, delta = 5), pch = 16, col = "black", cex = 1.5)
text(22 + 0.5, isovhigh3(22, v = 20, delta = 5) - 0.02, expression(f),cex = labelsize)
points(22, isovlow3(22, v = 20, delta = 5), pch = 16, col = "black", cex = 1.5)
text(22 + 0.75, isovlow3(22, v = 20, delta = 5) + 0.02, expression(b),cex = labelsize)


dev.off()