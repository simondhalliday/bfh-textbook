require(shape)
pdf(file = "specprodexch/production_irs.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 5, 4, 4))


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
ylims <- c(0, 1.3)

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
ticksy <- c(0, prodFn(l = 2, k = 0.02, alpha = 2), prodFn(l = 4, k = 0.02, alpha = 2), prodFn(l = 8, k = 0.02, alpha = 2), ylims[2])
ylabels <- c(NA, expression(paste(x(l == 2))), expression(paste(x(l==4))), expression(paste(x(l==8))), NA)
ticksx <- c(0, 2, 4, 8, xlims[2])
xlabels <- c(NA, 2, 4, 8, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

xx3 <- seq(1, 3, length.out = npts)
xx4 <- seq(3, 5, length.out = npts)
xx5 <- seq(7, 9, length.out = npts)

lines(xx1, prodFn(xx1, alpha = 2, k = 0.02), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, MprodFn(xx1, alpha = 2, k = 0.02), col = COLA[1], lwd = graphlinewidth)
#lines(xx1, AprodFn(xx1, alpha = 2, k = 0.02), col = COLA[2], lwd = graphlinewidth)
lines(xx3, Mpline(xx3, constant = prodFn(l = 2, k = 0.02, alpha = 2) - 2*MprodFn(l = 2, k = 0.02, alpha = 2), slope = MprodFn(l = 2, k = 0.02, alpha = 2)), col = "gray", lty = 2, lwd = graphlinewidth)
lines(xx4, Mpline(xx4, constant = prodFn(l = 4, k = 0.02, alpha = 2) - 4*MprodFn(l = 4, k = 0.02, alpha = 2), slope = MprodFn(l = 4, k = 0.02)), col = "gray", lty = 2, lwd = graphlinewidth)
lines(xx5, Mpline(xx5, constant = prodFn(l = 8, k = 0.02, alpha = 2) - 8*MprodFn(l = 8, k = 0.02, alpha = 2), slope = MprodFn(l = 8, k = 0.02)), col = "gray", lty = 2, lwd = graphlinewidth)

#Axis Labels
mtext(expression(paste("Hours of Labor, ", l)), side = 1, line = 2.5, cex = axislabelsize)
text(-1.2, 0.75, expression(paste("Total Product, ", x)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Points and segments for l = 8
segments(0, prodFn(l = 8, k = 0.02, alpha = 2), 8, prodFn(l = 8, k = 0.02, alpha = 2), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(8, 0, 8, prodFn(l = 8, k = 0.02, alpha = 2), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 0, 8, prodFn(l = 8, k = 0.02, alpha = 2), lty = 2, col = COLB[4] , lwd = segmentlinewidth)
points(8, prodFn(l = 8, k = 0.02), pch = 16, col = "black", cex = 1.5)

#Points and segments for l = 4
segments(0, prodFn(l = 4, k = 0.02, alpha = 2), 4, prodFn(l = 4, k = 0.02, alpha = 2), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(4, 0, 4, prodFn(l = 4, k = 0.02, alpha = 2), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 0, 4, prodFn(l = 4, k = 0.02, alpha = 2), lty = 2, col = COLB[4] , lwd = segmentlinewidth)
points(4, prodFn(l = 4, k = 0.02), pch = 16, col = "black", cex = 1.5)

#Points and segments for l = 2
segments(0, prodFn(l = 2, k = 0.02, alpha = 2), 2, prodFn(l = 2, k = 0.02, alpha = 2), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(2, 0, 2, prodFn(l = 2, k = 0.02, alpha = 2), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 0, 2, prodFn(l = 2, k = 0.02, alpha = 2), lty = 2, col = COLB[4] , lwd = segmentlinewidth)
points(2, prodFn(l = 2, k = 0.02, alpha = 2), pch = 16, col = "black", cex = 1.5)


#Label the iso-welfare functions for the HG, Aisha
text(6.3, 0.6, expression(paste("Total Product")))
text(6.3, 0.5, expression(x == frac(1,50)*(l)^2))


#Marginal Product
text(6, 0.1, expression(paste("Slope of tangent line")))
text(6, 0.05, expression(paste("equals Marginal Product")))
Arrows(4.7, 0.075, 2.3, 0.075, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
#Arrows(8, 0.13, 8, 0.67, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Average Product
text(1.5, prodFn(l = 7, k = 0.02, alpha = 2) - 0.1, expression(paste("Slope of ray from origin")))
text(1.5, prodFn(l = 7, k = 0.02, alpha = 2) - 0.15, expression(paste("equals Average Product")))
#Arrows(1.5, prodFn(l = 6, k = 0.02, alpha = 2) - 0.175, 1.5, prodFn(l = 2, k = 0.02, alpha = 2) + 0.01 , col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
Arrows(2.8, prodFn(l = 7, k = 0.02, alpha = 2) - 0.125, 4.95, prodFn(l = 7, k = 0.02, alpha = 2) - 0.125, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)



dev.off()
