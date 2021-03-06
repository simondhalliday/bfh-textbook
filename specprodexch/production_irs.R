require(shape)
pdf(file = "specprodexch/production_irs.pdf", width = 9, height = 7)

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
par(mar =  c(4, 6, 1, 1))


prodFn <- function(l, alpha = 2, k = 0.02) {
  k*l^alpha
}

MprodFn <- function(l, alpha = 2, k = 0.02) {
  k*alpha*l^(alpha - 1)
}

AprodFn <- function(l, alpha = 2, k = 0.02) {
  k*l^(alpha - 1)
}

Mpline <- function(l, constant = 0.3181472, slope = 0.125){
  constant + slope*l
}

xlims <- c(0, 10)
ylims <- c(0, 1.4)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(13, 15, 17)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
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
ticksy <- c(0, prodFn(l = 2, k = 0.02, alpha = 2), prodFn(l = 6, k = 0.02, alpha = 2), ylims[2])
ylabels <- c(NA, expression(paste(2)), expression(paste(6)), NA)
ticksx <- c(0, 2, 6, xlims[2])
xlabels <- c(NA, 2, 6, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

xx3 <- seq(1, 3, length.out = npts)
xx4 <- seq(5, 7, length.out = npts)

lines(xx1, prodFn(xx1, alpha = 2, k = 0.02), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, MprodFn(xx1, alpha = 2, k = 0.02), col = COLA[1], lwd = graphlinewidth)
#lines(xx1, AprodFn(xx1, alpha = 2, k = 0.02), col = COLA[2], lwd = graphlinewidth)
lines(xx3, Mpline(xx3, constant = prodFn(l = 2, k = 0.02, alpha = 2) - 2*MprodFn(l = 2, k = 0.02, alpha = 2), slope = MprodFn(l = 2, k = 0.02, alpha = 2)), col = "gray", lty = 2, lwd = graphlinewidth)
lines(xx4, Mpline(xx4, constant = prodFn(l = 6, k = 0.02, alpha = 2) - 6*MprodFn(l = 6, k = 0.02, alpha = 2), slope = MprodFn(l = 6, k = 0.02)), col = "gray", lty = 2, lwd = graphlinewidth)

#Axis Labels
mtext(expression(paste("Hours of labor, ", l)), side = 1, line = 2.5, cex = axislabelsize)
text(-1, 0.75, expression(paste("Total product, ", x)), xpd = TRUE, cex = axislabelsize, srt = 90) 

segments(0, prodFn(l = 6, k = 0.02, alpha = 2), 6, prodFn(l = 6, k = 0.02, alpha = 2), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(6, 0, 6, prodFn(l = 6, k = 0.02, alpha = 2), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 0, 6, prodFn(l = 6, k = 0.02, alpha = 2), lty = 2, col = COLB[4] , lwd = segmentlinewidth)
points(6, prodFn(l = 6, k = 0.02), pch = 16, col = "black", cex = 1.5)

segments(0, prodFn(l = 2, k = 0.02, alpha = 2), 2, prodFn(l = 2, k = 0.02, alpha = 2), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(2, 0, 2, prodFn(l = 2, k = 0.02, alpha = 2), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 0, 2, prodFn(l = 2, k = 0.02, alpha = 2), lty = 2, col = COLB[4] , lwd = segmentlinewidth)
points(2, prodFn(l = 2, k = 0.02, alpha = 2), pch = 16, col = "black", cex = 1.5)


#Label the iso-welfare functions for the HG, Aisha
text(8.6, 1.1, expression(paste("Total Product")), cex = annotatesize)
text(8.6, 1, expression(x == frac(1,50)*(l)^2), cex = annotatesize)


#Marginal Product
text(7, 0.12, expression(paste("Slope of tangent line")), cex = labelsize)
text(7, 0.05, expression(paste("equals Marginal Product")), cex = labelsize)
Arrows(4.7, 0.075, 2.3, 0.075, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
Arrows(6, 0.16, 6, 0.67, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Average Product
text(2, prodFn(l = 6, k = 0.02, alpha = 2) - 0.1, expression(paste("Slope of ray from origin")), cex = labelsize)
text(2, prodFn(l = 6, k = 0.02, alpha = 2) - 0.15, expression(paste("equals Average Product")), cex = labelsize)
Arrows(1.5, prodFn(l = 6, k = 0.02, alpha = 2) - 0.175, 1.5, prodFn(l = 2, k = 0.02, alpha = 2) + 0.01 , col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
Arrows(4, prodFn(l = 6, k = 0.02, alpha = 2) - 0.125, 4.75, prodFn(l = 6, k = 0.02, alpha = 2) - 0.125, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)



dev.off()
