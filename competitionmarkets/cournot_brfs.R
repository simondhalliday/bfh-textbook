require(shape)
pdf(file = "competitionmarkets/cournot_brfs.pdf", width = 9, height = 7)

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

par(mar =  c(6, 8.5, 0.5, 0.5))

piA <- function(xa, xb, s = 0.5, pmax = 20, c1 = 2) {
  (pmax - s*xb)*xa - s*(xa)^2 - c1*xa
}

piB <- function(xa, xb, s = 0.5, pmax = 20, c1 = 2) {
  (pmax - s*xa)*xb - s*(xb)^2 - c1*xb
}

brfB <- function(xa, s = 0.5, pmax = 20, c1 = 2) {
 (pmax - c1)/(2*s) - (1/2)*xa
}

brfA <- function(xa, s = 0.5, pmax = 20, c1 = 2) {
  (pmax - c1)/s - 2*xa
}


xlims <- c(0, 42)
ylims <- c(0, 42)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(46.08, 55, 64)
b <- c(46.08, 55, 64)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs = "i", 
     yaxs = "i")


ticksy <- c(0, 12, 18, 36, ylims[2])
ylabels <- c(NA, expression(paste(x^{BN} == 12)), expression(paste( frac(bar(p) - c,2*beta) == 18 )), expression(paste( frac(bar(p) - c,beta) == 36 )), NA)
ticksx <- c(0, 12, 18, 36, xlims[2])
#xlabels <- c(NA, expression(paste(x^{AN} == 12)), expression(paste(frac(bar(p) - c[1],2*beta) == 18)), expression(paste( frac(bar(p) - c[1],beta) ==36 )), NA)
xlabelsnum <- c(NA, NA, NA, expression(paste( frac(bar(p) - c,beta) == 36 )), NA)

text(11, -2.3, expression(paste(x^{AN} == 12)),xpd = TRUE, cex = labelsize)
text(19.2, -3, expression(paste(frac(bar(p) - c,2*beta) == 18)),xpd = TRUE, cex = labelsize)

axis(1, at = ticksx, pos = 0, labels = FALSE)
text(x = c(0, 12, 18, 36, xlims[2]), par("usr")[3] - 0.4, labels = xlabelsnum, srt = 0, pos = 1, xpd = TRUE, cex = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, brfA(xx1, s = 0.5, pmax = 20, c1 = 2), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfB(xx1, s = 0.5, pmax = 20, c1 = 2), col = COLB[4], lwd = graphlinewidth)

segments(0, 12, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(12, 0, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)

text(0.5*(xlims[2]), -7, expression(paste("A's output, ", x^A)), xpd = TRUE, cex = axislabelsize) 
text(-8.5, 0.5*ylims[2], expression(paste("B's output, ", x^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label point i. 
points(12, 12, pch = 16, col = "black", cex = 1.5)
text(18.5, 12.5, expression(paste("Nash Equilibrium")), cex = annotatesize)
text(11.3, 11.3, expression(paste("n")), cex = labelsize)

#B's brf
text(35.5, 8, expression(paste("B's best-response")), cex = annotatesize)
text(35.5, 6, expression(paste("function")), cex = annotatesize)
Arrows(28, 6.75, 24, 6.75, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#A's brf
text(8, 35.5, expression(paste("A's best-response")), cex = annotatesize)
text(8, 33.5, expression(paste("function")), cex = annotatesize)
Arrows(6, 32, 6, 26, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


dev.off()