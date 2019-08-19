require(shape)
library(pBrackets)

pdf(file = "public_mechanism/smoking_MU_MC.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(5, 5, 1, 1))

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
xlabels <- c(NA, expression(paste(x[it])), expression(paste(x[it]^{direct})), expression(paste(x[i0])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

text(0.5*xlims[2], -1.5, expression(paste("i's smoking")), xpd = TRUE, cex = axislabelsize) 
text(-.4, 12, expression(paste("$")), xpd = TRUE, cex = axislabelsize) 

text(1.37, 11.5, expression(paste(u[i],"(",x[i],",",x[jt],")")), xpd = TRUE, cex = 1) 
text(2.05, 11.5, expression(paste(u[i],"(",x[i],",",x[j0],")")), xpd = TRUE, cex = 1) 


npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

segments(1.25, 0, 1.25, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(2, 0, 2, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(5, 0, 5, 4, lty = 2, col = "gray" , lwd = segmentlinewidth)


lines(xx1, MU(xx1, Xj=5), col = COLA[4], lwd = graphlinewidth)
lines(xx1, MU(xx1, Xj=1.25), col = COLA[4], lwd = graphlinewidth)

segments(0, 4, xlims[2], 4, lty = 1, col = COLB[3] , lwd = segmentlinewidth)
segments(0, 10, xlims[2], 10, lty = 1, col = COLB[3] , lwd = segmentlinewidth)


text(5.5, 10.3, expression(paste("Price after tax")), cex = 1)
text(5.5, 4.3, expression(paste("Price before tax")), cex = 1)

brackets(2, 0, 1.25, 0, h = .25,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = segmentlinewidth, lty = 1)

brackets(5, 0, 2, 0, h = .3,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = segmentlinewidth, lty = 1)

text(1.65, -.85, expression(paste("Indirect \n effect")), cex = 0.85)
text(3.5, -.85, expression(paste("Direct \n effect")), cex = 0.85)

dev.off()
