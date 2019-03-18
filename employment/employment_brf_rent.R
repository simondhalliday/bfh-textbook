require(shape)
pdf(file = "employment/employment_brf_rent.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 2
segmentlinewidth <- 1.5

#The equation is below when v = 0. See Wolfram Alpha output. 
isov <- function(w, delta = 5) {
}

isovhigh1 <- function(w, delta = 5, v = 5){
  (sqrt(w^2 - 4 * delta * v) - w + 2*v)/( 2 * v )
}

isovlow1 <- function(w, delta = 5, v = 5){
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

#This is evaluated for p = 12; q = 12/(8*sqrt(12^2 - 4*delta1*v1)) - 1/8 for the slope
#At p = 12; q = (-sqrt(12^2 - 4*v1*delta1) - 12 + 2*v1)/(2*v1) = 0.799
tangencyLine <- function(w){
  (w*(0.05))
}



COL <- c("#f7fcf5", "#e5f5e0", "#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#005a32")
par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 40)
ylims <- c(0, 1)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("Wage, ", w)),
     ylab = expression(paste("Effort, ", e)),
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     line = 2.5,
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

#Draw the lines for the graphs
#lines(xx0, isov(xx0, delta = 5), col = COL[3], lwd = graphlinewidth)
lines(xx1, brfFn(xx1), col = COLA[4], lwd = graphlinewidth)
#lines(xx2, tangencyLine(xx2), col = "darkgrey", lwd = 3, lty = 2)
lines(xx3, isovhigh1(xx3, v = 5, delta = 5), col = COLA[3], lwd = graphlinewidth)
lines(xx4, isovlow1(xx4, v = 5, delta = 5), col = COLA[3], lwd = graphlinewidth)
lines(xx5, isovhigh2(xx5, v = 17, delta = 5), col = COLA[3], lwd = graphlinewidth)
lines(xx6, isovlow2(xx6, v = 17, delta = 5), col = COLA[3], lwd = graphlinewidth)
lines(xx7, isovhigh3(xx7, v = 20, delta = 5), col = COLA[3], lwd = graphlinewidth)
lines(xx8, isovlow3(xx8, v = 20, delta = 5), col = COLA[3], lwd = graphlinewidth)

#Customize ticks and labels for the plot
ticksy <- c(0, 0.5, 1)
#ylabels <- c(0, expression(paste(frac(1,2))), 1)
ylabels <- c(0, expression(paste(e^N)), 1)
ticksx <- c(0, 10, 20, 40)
xlabels <- c(0, expression(paste(w == B/s)), expression(paste(w^N)) , NA)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Annotation of the three graphs and the NE
#text(5, 0.3, expression(paste("Iso-profit: ", frac(q, p) ," = ", frac(1, 8*delta))))
#text(35, 0.62, expression(paste("BRF: q = ", 1 - frac(2*delta, p))))
#text(3.9, 0.05, expression(paste(v[0],  " = z")))
text(9.5, 0.05, expression(paste(v[0])))
text(22, 0.05, expression(paste(v[1])))
text(24.8, 0.05, expression(paste(v[2])))
text(35, 0.66, expression(paste("Employee's")))
text(35, 0.62, expression(paste("Best Response Function")))
#text(35, 0.58, expression(paste(e(Delta, a))))
text(36, 0.94, expression(paste("Employee's")))
text(36, 0.9, expression(paste("Iso-v curves")))


#Lines for the coordinates of the Nash equilbrium
#segments(5, 0, 5, 1, lty = 2, col = "darkgray", lwd = 3)
segments(10, 0, 10, 0.2, lty = 2, col = "darkgray", lwd = 2)
segments(20, 0, 20, ylims[2], lty = 2, col = "darkgray", lwd = 2)
segments(0, 0.5, 20, 0.5, lty = 2, col = "darkgray", lwd = 2)
#segments(14.14214, 0.15, 14.14214, 0.45, lty = 2, col = "darkgray", lwd = 3)
text(19.5, 0.52, expression(n))
text(24.5, 0.48, expression(paste("Incomplete Contract")))
text(24.5, 0.43, expression(paste("Nash equilibrium")))

#Arrows and rent label
Arrows(15, 0.8, 19.3, 0.8,  col = "black", lty = 1, lwd = 2, arr.type = "triangle")
Arrows(15, 0.8, 13.2, 0.8,  col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(16.25, 0.98, expression(paste("Rent at")))
text(16.25, 0.93, expression(paste("Incomplete")))
text(16.25, 0.88, expression(paste("Contract")))
text(16.25, 0.83, expression(paste("Nash Equilibrium")))

#Arrows and slope of iso-v label
Arrows(29.5, 0.15, 24, 0.15,  col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(32, 0.2, expression(paste("Slope of iso-v")))
text(32.3, 0.15, expression(paste(" = -mrs(w,e) ")))
text(32, 0.08, expression(paste(" = " -frac(v[w], v[e]))))



#Add a point for the NE
points(20, 0.5, pch = 16, col = "black", cex = 1.5)

segments(12.5, 0, 12.5, ylims[2], lty = 2, col = "darkgray", lwd = 2)
#segments(0, brfFn(w = 18.4), 18.4, brfFn(w = 18.4), lty = 2, col = "darkgray", lwd = 2)
points(12.5, isovhigh1(w = 12.5), pch = 16, col = "black", cex = 1.5)
text(12.5 - 0.5, isovhigh1(w = 12.5) + 0.025, expression(a))

#Add a point for b & complete contract NE label
#points(10, 0.5, pch = 16, col = "black", cex = 1.5)
#text(10.5, 0.47, expression(b))
# text(5, 0.58, expression(paste("Complete Contract")))
# text(5, 0.53, expression(paste("Nash equilibrium")))

#Add a point for c
#Figure out q for p = 14.14214: q = 1 - 2delta/p = 1 - (2*5)/14.14214 =  0.2928934
#points(20, 0.1839422, pch = 16, col = "black", cex = 1.5)
#text(20.8, 0.1839422, expression(c))

#points(22, isovhigh3(22, v = 20, delta = 5), pch = 16, col = "black", cex = 1.5)
#text(22 + 0.5, isovhigh3(22, v = 20, delta = 5) - 0.02, expression(f))
#points(22, isovlow3(22, v = 20, delta = 5), pch = 16, col = "black", cex = 1.5)
#text(22 + 0.5, isovlow3(22, v = 20, delta = 5) + 0.02, expression(b))

#Add a point for f. referred to in the text
#points(12, 0.82, pch = 16, col = "black", cex = 1.2)
#text(12, 0.85, expression(paste("NE")))

#Arrow to Pareto-improving Lens
#Arrows(20, 0.8, 23.8, 0.68, col = "black", lty = 1, lwd = 3)
#text(20, 0.82, expression(paste("Pareto-Improving Lens")))

dev.off()