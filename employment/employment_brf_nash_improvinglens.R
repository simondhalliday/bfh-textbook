require(shape)
pdf(file = "employment/employment_brf_nash_improvinglens.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

#The equation is below when v = 0. See Wolfram Alpha output. 
isov <- function(w, delta = 5) {
  (w - delta)/w
}

isovhigh1 <- function(w, delta = 5, v = 30){
  (sqrt(w^2 - 4 * delta * v) - w + 2*v)/( 2 * v )
}

isovlow1 <- function(w, delta = 5, v = 30){
  (-sqrt(w^2 - 4 * delta * v) - w + 2*v)/( 2 * v )
}

isovhigh2 <- function(w, delta = 5, v = 15){
  (sqrt(w^2 - 4 * delta * v) - w + 2*v)/( 2 * v )
}

isovlow2 <- function(w, delta = 5, v = 15){
  (-sqrt(w^2 - 4 * delta * v) - w + 2*v)/( 2 * v )
}

isovhigh3 <- function(w, delta = 5, v = 20){
  (sqrt(w^2 - 4 * delta * v) - w + 2*v)/( 2 * v )
}

isovlow3 <- function(w, delta = 5, v = 20){
  (-sqrt(w^2 - 4 * delta * v) - w + 2*v)/( 2 * v )
}

brfFn <- function(w, delta = 5) {
  1 - (2*delta) /w
}

isovhigh <- function(w, delta = 5, v = 20){
  (sqrt(w^2 - 4 * delta * v) - w + 2*v)/(2 * v )
}

#This is evaluated for p = 12; q = 12/(8*sqrt(12^2 - 4*delta1*v1)) - 1/8 for the slope
#At p = 12; q = (-sqrt(12^2 - 4*v1*delta1) - 12 + 2*v1)/(2*v1) = 0.799
tangencyLine <- function(w){
  (w*(0.05))
}

solowCondition <- function(w, delta = 5){
  (w*(1/(8*delta)))
}


COL <- c("#f7fcf5", "#e5f5e0", "#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#005a32", "#ffff99")
#COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0)

grays <- gray.colors(25, start = 1, end = 0)
par(mar =  c(5, 5, 4, 5))
xlims <- c(0, 40)
ylims <- c(0, 1)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("Wage, ", w)),
     ylab = expression(paste("Effort, ", e)),
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     line = 3,
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
xx5 <- seq(18.43909, xlims[2], length.out = npts2)
xx6 <- seq(18.43909, 41, length.out = npts2)
xx7 <- seq(20, xlims[2], length.out = npts2)
xx8 <- seq(xlims[1], 25, length.out = npts2)
xx9 <- seq(xlims[1] + 0.5, xlims[2], length.out = npts2)
yy1 <- isovhigh(xx7, delta = 5, v = 20)


polygon(c(xx7, xx7[340]), c(yy1, yy1[340]), col = COL[9], density = NULL, border = NA)

#Draw the lines for the graphs
lines(xx1, brfFn(xx1), col = "#beaed4", lwd = graphlinewidth)
lines(xx0, isov(xx0, delta = 5), col = COL[6], lwd = graphlinewidth)

lines(xx9, solowCondition(xx9, delta = 5), col = COLB[4], lwd = graphlinewidth)
#lines(xx3, isovhigh1(xx3, v = 5, delta = 5), col = COL[4], lwd = graphlinewidth)
#lines(xx4, isovlow1(xx4, v = 5, delta = 5), col = COL[4], lwd = graphlinewidth)
#lines(xx5, isovhigh2(xx5, v = 17, delta = 5), col = COL[5], lwd = graphlinewidth)
#lines(xx6, isovlow2(xx6, v = 17, delta = 5), col = COL[5], lwd = graphlinewidth)
lines(xx7, isovhigh3(xx7, v = 20, delta = 5), col = COL[6], lwd = graphlinewidth)
lines(xx8, isovlow3(xx8, v = 20, delta = 5), col = COL[6], lwd = graphlinewidth)
lines(xx5, isovhigh1(xx5, v = 30, delta = 5), col = COL[6], lwd = graphlinewidth, lty = 2)
lines(xx6, isovlow1(xx6, v = 30, delta = 5), col = COL[6], lwd = graphlinewidth, lty = 2)

#Customize ticks and labels for the plot
ticksy <- c(0,  0.5, 1)
#ylabels <- c(0, expression(paste(frac(1,2))), 1)
ylabels <- c(0,  expression(paste(e^N)), 1)
ticksx <- c(0, 5, 10, 20, 40)
xlabels <- c(0, expression(paste(underline(u))) ,expression(paste(w == 2*underline(u))),  expression(paste(w^N)) , NA)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#Annotation of  the three graphs and the NE
text(3.5, 0.42, expression(paste("Slope of ")), cex = labelsize)
text(3.5, 0.38, expression(paste("isocost:")), cex = labelsize)
text(3.5, 0.3, expression(paste( frac(e, w) == frac(Delta*e, Delta*w))), cex = labelsize)
Arrows(6.3, 0.34, 12.6, 0.34,  col = "black", lty = 1, lwd = 2, arr.type = "triangle")

text(25.5, 0.05, expression(paste(v[N])), cex = labelsize)
text(38, 0.66, expression(paste("Best-response")), cex = labelsize, xpd = TRUE)
text(38, 0.6, expression(paste("function (ICC)")), cex = labelsize, xpd = TRUE)
text(38, 0.53, expression(paste(e(w))), cex = labelsize, xpd = TRUE)
text(35, 0.05, expression(paste(v[5])), cex = annotatesize)
text(37, 0.98, expression(paste(c[2])), cex = annotatesize)
text(7.8, 0.05, expression(paste(v[0] == u[0])), cex = labelsize)
text(8.2, 0.02, expression(paste(phantom() == 0)), cex = labelsize)




#Lines for the coordinates of the Nash equilbrium
#segments(5, 0, 5, 1, lty = 2, col = "darkgray", lwd = 3)
#segments(10, 0, 10, 0.2, lty = 2, col = "darkgray", lwd = 2)
segments(20, 0, 20, 0.75, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, 0.5, 20, 0.5, lty = 2, col = grays[20], lwd = segmentlinewidth)
#segments(14.14214, 0.15, 14.14214, 0.45, lty = 2, col = "darkgray", lwd = 3)
text(21, 0.48, expression(n), cex = labelsize)
#text(13.8, 0.57, expression(paste("Incomplete contract")),cex = labelsize)
#text(14.2, 0.52, expression(paste("Nash equilibrium")),cex = labelsize)

#Arrows and rent label
# 
# Arrows(15, 0.8, 10.8, 0.8,  col = "black", lty = 1, lwd = 2, arr.type = "triangle")
# text(14.8, 0.95, expression(paste("Rent at")))
# text(14.8, 0.9, expression(paste("Incomplete Contract")))
# text(14.8, 0.85, expression(paste("Nash Equilibrium")))

#Arrows and slope of iso-v label
Arrows(34, 0.3, 30, 0.3,  col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(37, 0.35, expression(paste("Slope of iso-v")), cex = labelsize, xpd =TRUE)
text(37, 0.3, expression(paste(phantom() == "-mrs ")), cex = labelsize)
text(37, 0.22, expression(paste(phantom() == -frac(v[w], v[e]))), cex = labelsize)



#Add a point for the NE
points(20, 0.5, pch = 16, col = "black", cex = 1.5)

#segments(18.4, 0, 18.4, 0.75, lty = 2, col = "darkgray", lwd = 2)
#segments(0, brfFn(w = 18.4), 18.4, brfFn(w = 18.4), lty = 2, col = "darkgray", lwd = 2)
#points(18.4, brfFn(w = 18.4), pch = 16, col = "black", cex = 1.5)

#Add a point for b & complete contract NE label
#points(10, 0.5, pch = 16, col = "black", cex = 1.5)
#text(10.5, 0.47, expression(b))
# text(5, 0.58, expression(paste("Complete Contract")))
# text(5, 0.53, expression(paste("Nash equilibrium")))

#Add a point for c
#Figure out q for p = 14.14214: q = 1 - 2delta/p = 1 - (2*5)/14.14214 =  0.2928934
#points(20, 0.1839422, pch = 16, col = "black", cex = 1.5)
#text(20.8, 0.1839422, expression(c))

points(26.2, 0.72, pch = 16, col = "black", cex = 1.5)
text(26.2 + 0.5, 0.72 - 0.02, expression(f), cex = labelsize)
points(22, isovlow3(22, v = 20, delta = 5), pch = 16, col = "black", cex = 1.5)
text(22 + 0.8, isovlow3(22, v = 20, delta = 5) + 0.02, expression(b), cex = labelsize)

#Add a point for f. referred to in the text
#points(12, 0.82, pch = 16, col = "black", cex = 1.2)
#text(12, 0.85, expression(paste("NE")))

#Arrow to Pareto-improving Lens
#Arrows(20, 0.8, 23.8, 0.68, col = "black", lty = 1, lwd = 3)
#text(20, 0.82, expression(paste("Pareto-Improving Lens")))

dev.off()

