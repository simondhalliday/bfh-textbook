require(shape)
pdf(file = "bfh-textbook/indmarketdemand/production_irs_apmp.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlabelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 7, 4, 4))


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

xlims <- c(0, 11)
ylims <- c(0, 0.55)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(13, 15, 17)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     #cex.lab = 1.5, 
     #cex.axis = 0.8, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = 0, to = ylims[2], by = 0.25)
# ylabels <- seq(from = 0, to = ylims[2], by = 0.25)
# ticksx <- seq(from = 0, to = xlims[2], by = 1)
# xlabels <- seq(from = 0, to = xlims[2], by = 1)
ticksy <- c(0, AprodFn(l = 2, k = 0.02, alpha = 2), MprodFn(l = 2, k = 0.02, alpha = 2), AprodFn(l = 8, k = 0.02, alpha = 2), MprodFn(l = 8, k = 0.02, alpha = 2), ylims[2])
ylabels <- c(NA, expression(paste(ap(l==2))), expression(paste(mp(l==2))), expression(paste(ap(l==8))), expression(paste(mp(l==8))), NA)
ticksx <- c(0, 2, 8, xlims[2])
xlabels <- c(NA, 2, 8, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = graphlabelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = graphlabelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

xx3 <- seq(1, 3, length.out = npts)
xx4 <- seq(5, 7, length.out = npts)

#lines(xx1, prodFn(xx1, alpha = 2, k = 0.02), col = COLB[4], lwd = graphlinewidth)
lines(xx1, MprodFn(xx1, alpha = 2, k = 0.02), col = COLA[3], lwd = graphlinewidth)
lines(xx1, AprodFn(xx1, alpha = 2, k = 0.02), col = COLA[4], lwd = graphlinewidth)
#lines(xx3, Mpline(xx3, constant = prodFn(l = 2, k = 0.02, alpha = 2) - 2*MprodFn(l = 2, k = 0.02, alpha = 2), slope = MprodFn(l = 2, k = 0.02, alpha = 2)), col = "gray", lty = 2, lwd = graphlinewidth)
#lines(xx4, Mpline(xx4, constant = prodFn(l = 8, k = 0.02, alpha = 2) - 6*MprodFn(l = 8, k = 0.02, alpha = 2), slope = MprodFn(l = 8, k = 0.02)), col = "gray", lty = 2, lwd = graphlinewidth)

#Axis Labels
mtext(expression(paste("Hours of Labor, ", l)), side = 1, line = 2.5, cex = axislabelsize)
text(-1.8, 0.5*ylims[2], expression(paste("Average & Marginal Product, ", x)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Margin
#segments(0, MprodFn(l = 8, k = 0.02, alpha = 2), 6, MprodFn(l = 8, k = 0.02, alpha = 2), lty = 2, col = "gray" , lwd = segmentlinewidth)
#segments(6, 0, 6, MprodFn(l = 8, k = 0.02, alpha = 2), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(8, 0, 8, ylims[2], lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, MprodFn(l = 8, k = 0.02), 8, MprodFn(l = 8, k = 0.02), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, AprodFn(l = 8, k = 0.02), 8, AprodFn(l = 8, k = 0.02), lty = 2, col = "gray" , lwd = segmentlinewidth)
points(8, MprodFn(l = 8, k = 0.02), pch = 16, col = "black", cex = 1.2)
points(8, AprodFn(l = 8, k = 0.02), pch = 16, col = "black", cex = 1.2)
segments(2, 0, 2, ylims[2], lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, MprodFn(l = 2, k = 0.02), 2, MprodFn(l = 2, k = 0.02), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, AprodFn(l = 2, k = 0.02), 2, AprodFn(l = 2, k = 0.02), lty = 2, col = "gray" , lwd = segmentlinewidth)
points(2, MprodFn(l = 2, k = 0.02), pch = 16, col = "black", cex = 1.2)
points(2, AprodFn(l = 2, k = 0.02), pch = 16, col = "black", cex = 1.2)

#Average
#segments(0, AprodFn(l = 2, k = 0.02, alpha = 2), 2, AprodFn(l = 2, k = 0.02, alpha = 2), lty = 2, col = "gray" , lwd = segmentlinewidth)
#segments(2, 0, 2, AprodFn(l = 2, k = 0.02, alpha = 2), lty = 2, col = "gray" , lwd = segmentlinewidth)
#segments(0, 0, 2, AprodFn(l = 2, k = 0.02, alpha = 2), lty = 2, col = COLB[4] , lwd = segmentlinewidth)
#points(2, AprodFn(l = 2, k = 0.02, alpha = 2), pch = 16, col = "black", cex = 1.5)


#Label the iso-welfare functions for the HG, Aisha
text(8.2, 1.1, expression(paste("Total Product")))
text(8.2, 1, expression(x == frac(1,50)*(l)^2))


#Marginal Product
#text(6, 0.1, expression(paste("Slope of tangent line")))
text(9.5, MprodFn(l = 9.5, k = 0.02, alpha = 2) + 0.09, expression(paste("Marginal Product")), cex = graphlabelsize)
text(9.5, MprodFn(l = 9.5, k = 0.02, alpha = 2) + 0.05, expression(paste(mp(l) == frac(1,25)*(l) )), cex = graphlabelsize)
#Arrows(4.7, 0.075, 2.3, 0.075, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
#Arrows(6, 0.13, 6, 0.67, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Average Product
text(9.5, AprodFn(l = 9.5, k = 0.02, alpha = 2) - 0.04, expression(paste("Average Product")), cex = graphlabelsize)
text(9.5, AprodFn(l = 9.5, k = 0.02, alpha = 2) - 0.08, expression(paste(ap(l) == frac(1,50)*(l) )), cex = graphlabelsize)
#Arrows(1.5, prodFn(l = 8, k = 0.02, alpha = 2) - 0.175, 1.5, prodFn(l = 2, k = 0.02, alpha = 2) + 0.01 , col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
#Arrows(2.8, prodFn(l = 8, k = 0.02, alpha = 2) - 0.125, 4.75, prodFn(l = 8, k = 0.02, alpha = 2) - 0.125, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)



dev.off()
