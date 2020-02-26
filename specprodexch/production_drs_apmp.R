require(shape)
pdf(file = "specprodexch/production_drs_apmp.pdf", width = 9, height = 7)

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
par(mar =  c(4, 5, 1, 1))


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
xlims <- c(0, 11)
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


ticksy <- c(0, AprodFn(l = 2, k = 0.5), MprodFn(l = 2, k = 0.5), AprodFn(l = 8, k = 0.5), MprodFn(l = 8, k = 0.5), ylims[2])
ylabels <- c(NA, expression(paste(2)), expression(paste(2)), expression(paste(8)), expression(paste(8)), NA)
ticksx <- c(0, 2, 8, xlims[2])
xlabels <- c(NA, 2, 8, NA)

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
text(-1, 0.5*ylims[2], expression(paste("Average & marginal product, ", list(ap,mp) )), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Margin
segments(8, 0, 8, ylims[2], lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, MprodFn(l = 8, k = 0.5), 8, MprodFn(l = 8, k = 0.5), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, AprodFn(l = 8, k = 0.5), 8, AprodFn(l = 8, k = 0.5), lty = 2, col = "gray" , lwd = segmentlinewidth)
points(8, MprodFn(l = 8, k = 0.5), pch = 16, col = "black", cex = 1.2)
points(8, AprodFn(l = 8, k = 0.5), pch = 16, col = "black", cex = 1.2)
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
text(9.5, MprodFn(l = 9.5, k = 0.5) + 0.055, expression(paste("Marginal product")), cex = annotatesize)
text(9.5, MprodFn(l = 9.5, k = 0.5) + 0.025, expression(paste(mp(l) == frac(1,'2l'+2))), cex = annotatesize)

#Average Product
text(9.5, AprodFn(l = 9.5, k = 0.5) + 0.06, expression(paste("Average product")), cex = annotatesize)
text(9.5, AprodFn(l = 9.5, k = 0.5) + 0.03, expression(paste(ap(l) == frac(1,'2l')*(ln('l'+1)) )), cex = annotatesize)


dev.off()
