require(shape)
library(pBrackets)

pdf(file = "public_mechanism/smoking_MU_MC.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(4, 5, 1, 1))

MU <- function(Xi, Xj, alpha = 10, beta = 2){
  ((alpha+beta*Xj)/Xi) 
}

xlims <- c(0, 6)
ylims <- c(-1.1, 12)

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

ticksy <- c(0, 4, 10, ylims[2])
ylabels <- c(NA, expression(paste(P[0])),  expression(paste(P[t])), NA)
ticksx <- c(0, 1.25, 2, 5, xlims[2])
xlabels <- c(NA, expression(paste(x[At])), expression(paste(x[At]^{direct})), expression(paste(x[A0])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = axislabelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = axislabelsize)

text(0.5*xlims[2], -2.5, expression(paste("A's smoking")), xpd = TRUE, cex = axislabelsize) 
text(-.6, 0.5*ylims[2], expression(paste("Benefits and costs, $")), xpd = TRUE, cex = axislabelsize, srt = 90) 

text(0.75, 11.5, expression(paste(u[A],"(",x[A],",",x[Bt],")")), xpd = TRUE, cex = labelsize) 
text(2.1, 11.5, expression(paste(u[A],"(",x[A],",",x[B0],")")), xpd = TRUE, cex = labelsize) 


npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

segments(1.25, 0, 1.25, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(2, 0, 2, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(5, 0, 5, 4, lty = 2, col = "gray" , lwd = segmentlinewidth)


lines(xx1, MU(xx1, Xj=5), col = COLA[4], lwd = graphlinewidth)
lines(xx1, MU(xx1, Xj=1.25), col = COLA[4], lwd = graphlinewidth)

segments(0, 4, xlims[2], 4, lty = 1, col = COLB[3] , lwd = segmentlinewidth)
segments(0, 10, xlims[2], 10, lty = 1, col = COLB[3] , lwd = segmentlinewidth)

text(5.6, 10.7, expression(paste("Price")), cex = labelsize)
text(5.6, 10.3, expression(paste("after tax")), cex = labelsize)

text(5.6, 4.7, expression(paste("Price")), cex = labelsize)
text(5.6, 4.3, expression(paste("before tax")), cex = labelsize)

brackets(2, -1, 1.25, -1, h = .25,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = segmentlinewidth, lty = 1, xpd = TRUE)

brackets(5, -1, 2, -1, h = .3,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = segmentlinewidth, lty = 1, xpd = TRUE)

text(1.65, -1.95, expression(paste("Indirect \n effect")), cex = labelsize, xpd = TRUE)
text(3.5, -1.95, expression(paste("Direct \n effect")), cex = labelsize, xpd = TRUE)

dev.off()
