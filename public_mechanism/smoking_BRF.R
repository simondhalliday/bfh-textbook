require(shape)
library(pBrackets)

pdf(file = "smoking_BRF.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(5, 5, 1, 1))

BRFXi <- function(Xj, P, alpha = 10, beta = 2){
  (alpha+beta*Xj)/P
}

BRFXj <- function(Xj, P, alpha = 10, beta = 2){
  (P*Xj-alpha)/beta
}

xlims <- c(-1, 10)
ylims <- c(0, 10)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

ticksy <- c(0, 1.25, 2, 5, ylims[2])
ylabels <- c(NA, NA, NA,  NA, NA)
ticksx <- c(0, 1, 1.25, 2.5, xlims[2])
xlabels <- c(NA, NA, NA, NA, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

text(1, -0.49, expression(paste(frac(alpha,P[t]))), xpd = TRUE, cex = .8)
text(1.25, -0.45,  expression(paste(x[jt]^N)), xpd=TRUE, cex=.8)
text(2.5, -0.49, expression(paste(frac(alpha,P[0]))), xpd=TRUE, cex=.8 )
text(-0.45, 1.15, expression(paste(x[it]^N)), xpd = TRUE, cex = .8)
text(-0.45, 2.1, expression(paste(x[it]^{direct})) , xpd=TRUE, cex=.8)
text(-0.45, 5, expression(paste(x[i0])), xpd=TRUE, cex=.8 )
text(0.5*xlims[2], -.7, expression(paste("j's smoking")), xpd = TRUE, cex = axislabelsize) 
text(-1.3, 0.5*ylims[2], expression(paste("i's smoking ")), xpd = TRUE, cex = axislabelsize, srt = 90) 

npts <- 500 
xx1 <- seq(0, xlims[2], length.out = npts)

lines(xx1, BRFXj(xx1,P=4), col = COLB[4], lwd = graphlinewidth)
lines(xx1, BRFXi(xx1,P=4), col = COLA[4], lwd = graphlinewidth)
lines(xx1, BRFXj(xx1,P=10), col = COLB[4], lwd = graphlinewidth)
lines(xx1, BRFXi(xx1,P=10), col = COLA[4], lwd = graphlinewidth)

segments(5, 0, 5, 5, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 5, 5, 5, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(5, 5, pch = 16, col = "black", cex = 1.5)
text(5.25, 4.75, expression(paste(a)), cex = labelsize)

segments(1.25, 0, 1.25, 1.25, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 1.25, 1.25, 1.25, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(1.25, 1.25, pch = 16, col = "black", cex = 1.5)
text(1.5, 1, expression(paste(b)), cex = labelsize)

segments(0, 2, 5, 2, lty = 2, col = "gray" , lwd = segmentlinewidth)


text(7, 5.3, expression(paste("i's best response")), cex = labelsize)
text(7, 5.0, expression(paste("before tax")), cex = labelsize)
text(7, 2, expression(paste("i's best response")), cex = labelsize)
text(7, 1.75, expression(paste("after tax")), cex = labelsize)

brackets(0, 1.25, 0, 2, h = .1,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = segmentlinewidth, lty = 1)

brackets(0, 2, 0, 5, h = .1,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd =segmentlinewidth, lty = 1)


text(-.3, 1.55, expression(paste("Indirect \n effect")), cex = .5)
text(-.3, 3.5, expression(paste("Direct \n effect")), cex = .5)

dev.off()
