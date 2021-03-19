require(shape)
require(plotrix)
require(ggplot2)
require(Cairo)
pdf(file = "indmarketdemand/elasticity_fig.pdf", width = 9, height = 7)
#jpeg(file = "indmarketdemand/elasticity_fig.jpg", width = 640, height = 480, units = "px",)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 6, 2, 2))
xlims <- c(0, 3)
ylims <- c(2, 8)
xlims2 <- c(10, 0)
ylims2 <- c(15, 0)

elasticity1 <- function(x, A = 5, b = 1) {
  A*x^(1/b)
}

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = "Quantity of the good, x",
     ylab = "Price per unit of x, p", 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

ticksy <- seq(from = 0, to = 15, by = 1)
ylabels <- seq(from = 0, to = 15, by = 1)
ticksx <- seq(from = 0, to = 10, by = 1)
xlabels <- seq(from = 0, to = 10, by = 1)
axis(1, at = ticksx, pos = 2, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0, cex.axis = labelsize)

axis.break(axis = 2, breakpos = 2.2, pos = 0,
           bgcol = "white", breakcol = "black",
           style = "slash", brw = 0.02)

npts <- 500 
npts2 <- 501

#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
yy1 <- elasticity1(xx1, A = 5, b = -0.2)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
yy2 <- elasticity1(xx2, A = 5, b = -1.5)
xx3 <- seq(xlims[1], xlims[2], length.out = npts)
yy3 <- elasticity1(xx3, A = 5, b = -3.79)
xx4 <- seq(xlims[1], xlims[2], length.out = npts)
yy4 <- elasticity1(xx4, A = 5, b = -4.74)

lines(xx1,yy1, col = COL[3], lwd = graphlinewidth)
lines(xx2,yy2, col = COLB[2], lwd = graphlinewidth)
lines(xx3,yy3, col = COLA[3], lwd = graphlinewidth)
lines(xx4,yy4, col = COLB[4], lwd = graphlinewidth)

points(1, 5, pch = 16, col = "black", cex = 1.5)

legend(1.15, 8, 
       legend=c(expression(paste("Rice in Japan, ", abs(eta) == 0.2)), expression(paste("Fish, ", abs(eta) == 1.5)),expression(paste("Coca-cola, ", abs(eta) == 3.8)), expression(paste("Expensive alcoholic drinks, ", abs(eta) == 4.7))),
       col=c(COL[3], COLB[2],COLA[3],COLB[4]), 
       lty=1, cex=labelsize, xpd =TRUE)


dev.off()

