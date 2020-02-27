require(shape)
pdf(file = "specprodexch/production_drs_stacked.pdf", width = 9, height = 12)

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 7, 3, 3), mfrow = c(2, 1))


#Change this to make it log of l 

prodFn <- function(l, k = 0.5) {
  k*log(l + 1)
}

MprodFn <- function(l, k = 0.5) {
  k*(1/(l + 1))
}

AprodFn <- function(l, k = 0.5) {
  (k*log(l + 1))/l
}

Mpline <- function(l, constant = 0.3181472, slope = 0.125){
  constant + slope*l
}

xlims <- c(0, 10)
ylims <- c(0, 1.3)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(13, 15, 17)

plot(1, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = 0, to = ylims[2], by = 0.25)
# ylabels <- seq(from = 0, to = ylims[2], by = 0.25)
# ticksx <- seq(from = 0, to = xlims[2], by = 1)
# xlabels <- seq(from = 0, to = xlims[2], by = 1)
ticksy <- c(0, prodFn(l = 2, k = 0.5), prodFn(l = 6, k = 0.5), ylims[2])
ylabels <- c(NA, expression(paste(0.55)), expression(paste(0.97)), NA)
ticksx <- c(0, 2, 6, xlims[2])
xlabels <- c(NA, 2, 6, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = axislabelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = axislabelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(1, 3, length.out = npts)
xx4 <- seq(5, 7, length.out = npts)

lines(xx1, prodFn(xx1, k = 0.5), col = COLB[4], lwd = graphlinewidth)
#lines(xx2, MprodFn(xx1, k = 0.5), col = COLA[1], lwd = graphlinewidth)
#lines(xx1, AprodFn(xx1, k = 0.5), col = COLA[2], lwd = graphlinewidth)
lines(xx3, Mpline(xx3, constant = prodFn(l = 2, k = 0.5) - 2*MprodFn(l = 2, k = 0.5), slope = MprodFn(l = 2, k = 0.5)), col = "gray", lty = 2, lwd = graphlinewidth)
lines(xx4, Mpline(xx4, constant = prodFn(l = 6, k = 0.5) - 6*MprodFn(l = 6, k = 0.5), slope = MprodFn(l = 6, k = 0.5)), col = "gray", lty = 2, lwd = graphlinewidth)
#lines(xx1, indiffA1(xx1, uA = 20, rmax = 2.5, xmax = 10), col = COLB[4], lwd = graphlinewidth)

#mtext(expression(paste("Hours of labor, ", l)), side = 1, line = 2.5, cex = axislabelsize)
text(-1.5, 0.75, expression(paste("Total product, ", x)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
#text(7.3, 3, expression("Pareto Efficient"))
#text(7.3, 2.5, expression("Curve"))
#Arrows(7.3, 3.5, 7.3, 6.1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

segments(0, prodFn(l = 6, k = 0.5), 6, prodFn(l = 6, k = 0.5), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(6, 0, 6, prodFn(l = 6, k = 0.5), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 0, 6, prodFn(l = 6, k = 0.5), lty = 2, col = COLB[4] , lwd = segmentlinewidth)
points(6, prodFn(l = 6, k = 0.5), pch = 16, col = "black", cex = 1.5)

segments(0, prodFn(l = 2, k = 0.5), 2, prodFn(l = 2, k = 0.5), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(2, 0, 2, prodFn(l = 2, k = 0.5), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 0, 2, prodFn(l = 2, k = 0.5), lty = 2, col = COLB[4] , lwd = segmentlinewidth)
points(2, prodFn(l = 2, k = 0.5), pch = 16, col = "black", cex = 1.5)

#Label the iso-welfare functions for the HG, Aisha
text(8.7, 1, expression(paste("Total product")), cex = labelsize)
text(8.7, 0.9, expression(x == frac(1,2)*ln(1 + l)), cex = labelsize)


#Average Product
text(4.25, 0.27, expression(paste("Slope of ray from")), cex = labelsize)
text(4.25, 0.22, expression(paste("origin equals")), cex = labelsize)
text(4.25, 0.17, expression(paste("Average Product")), cex = labelsize)
Arrows(2.7, 0.3, 1.3, 0.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
Arrows(4, 0.33, 4, 0.58, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Marginal Product
text(2, prodFn(l = 6, k = 0.5) + 0.02, expression(paste("Slope of tangent line")), cex = labelsize)
text(2, prodFn(l = 6, k = 0.5) - 0.03, expression(paste("equals Marginal Product")), cex = labelsize)
Arrows(2, prodFn(l = 6, k = 0.5) - 0.075, 2, prodFn(l = 2, k = 0.5) + 0.05 , col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
Arrows(4, prodFn(l = 6, k = 0.5), 5.7, prodFn(l = 6, k = 0.5), col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


prodFn <- function(l, k = 0.5) {
  k*log(l + 1)
}

MprodFn <- function(l, k = 0.5) {
  k*(1/(l + 1))
}

AprodFn <- function(l, k = 0.5) {
  (k*log(l + 1))/l
}

Mpline <- function(l, constant = 0.3181472, slope = 0.125){
  constant + slope*l
}
xlims <- c(0, 10)
ylims <- c(0, 0.52)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(13, 15, 17)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     bty = "n",
     xaxs="i", 
     yaxs="i")


ticksy <- c(0, AprodFn(l = 2, k = 0.5), MprodFn(l = 2, k = 0.5), AprodFn(l = 6, k = 0.5), MprodFn(l = 6, k = 0.5), ylims[2])
ylabels <- c(NA, expression(paste(0.27)), expression(paste(0.17)), expression(paste(0.16)), expression(paste(0.07)), NA)
ticksx <- c(0, 2, 6, xlims[2])
xlabels <- c(NA, 2, 6, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

xx3 <- seq(1, 3, length.out = npts)
xx4 <- seq(5, 7, length.out = npts)

lines(xx1, MprodFn(xx1, k = 0.5), col = COLA[3], lwd = graphlinewidth)
lines(xx1, AprodFn(xx1, k = 0.5), col = COLA[4], lwd = graphlinewidth)

#Axis Labels
mtext(expression(paste("Hours of labor, ", l)), side = 1, line = 2.5, cex = axislabelsize)
text(-1.5, 0.5*ylims[2], expression(paste("Average & marginal product, ", list(ap,mp) )), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Margin
segments(6, 0, 6, ylims[2], lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, MprodFn(l = 6, k = 0.5), 8, MprodFn(l = 6, k = 0.5), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, AprodFn(l = 6, k = 0.5), 8, AprodFn(l = 6, k = 0.5), lty = 2, col = "gray" , lwd = segmentlinewidth)
points(6, MprodFn(l = 6, k = 0.5), pch = 16, col = "black", cex = 1.2)
points(6, AprodFn(l = 6, k = 0.5), pch = 16, col = "black", cex = 1.2)
segments(2, 0, 2, ylims[2], lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, MprodFn(l = 2, k = 0.5), 2, MprodFn(l = 2, k = 0.5), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, AprodFn(l = 2, k = 0.5), 2, AprodFn(l = 2, k = 0.5), lty = 2, col = "gray" , lwd = segmentlinewidth)
points(2, MprodFn(l = 2, k = 0.5), pch = 16, col = "black", cex = 1.2)
points(2, AprodFn(l = 2, k = 0.5), pch = 16, col = "black", cex = 1.2)

#Label the iso-welfare functions for the HG, Aisha
text(8.2, 1.1, expression(paste("Total Product")))
text(8.2, 1, expression(x == frac(1,50)*(l)^2))


#Marginal Product
#text(6, 0.1, expression(paste("Slope of tangent line")))
text(9.5, MprodFn(l = 9.5, k = 0.5) + 0.055, expression(paste("Marginal product")), cex = annotatesize, xpd = TRUE)
text(9.8, MprodFn(l = 9.5, k = 0.5) + 0.015, expression(paste(mp(l) == frac(1,'2l'+2))), cex = annotatesize, xpd = TRUE)

#Average Product
text(8, AprodFn(l = 9.5, k = 0.5) + 0.1, expression(paste("Average product")), cex = annotatesize)
text(8, AprodFn(l = 9.5, k = 0.5) + 0.05, expression(paste(ap(l) == frac(1,'2l')*(ln('l'+1)) )), cex = annotatesize)


dev.off()
