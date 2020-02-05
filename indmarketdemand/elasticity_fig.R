require(shape)
require(ggplot2)
#pdf(file = "property_psp1_offerSTEP1.pdf", width = 9, height = 7)
#pdf(file = "property_psp1_offerSTEP2.pdf", width = 9, height = 7)
#pdf(file = "property_psp1_offerSTEP3.pdf", width = 9, height = 7)
#pdf(file = "property_psp1_offerSTEP4.pdf", width = 9, height = 7)
pdf(file = "indmarketdemand/elasticity_fig.pdf", width = 9, height = 7)

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

par(mar =  c(4, 4, 4, 4))
xlims <- c(0, 10)
ylims <- c(0, 15)
xlims2 <- c(10, 0)
ylims2 <- c(15, 0)

elasticity1 <- function(x, A, b ) {
  A*x^(1/b)
}

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = "",
     ylab = "", 
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
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0)


npts <- 500 
npts2 <- 501

#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
yy1 <- elasticity1(xx1, A = 5, b = -0.2)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
yy2 <- elasticity1(xx2, A = 5, b = -1.5)
xx3 <- seq(xlims[1], xlims[2], length.out = npts)
yy3 <- elasticity1(xx3, A = 5, b = -3.4)
xx4 <- seq(xlims[1], xlims[2], length.out = npts)
yy4 <- elasticity1(xx4, A = 5, b = -4.74)


lines(xx1,yy1, col = COL[3], lwd = graphlinewidth)
lines(xx2,yy2, col = COLB[2], lwd = graphlinewidth)
lines(xx3,yy3, col = COLA[3], lwd = graphlinewidth)
lines(xx4,yy4, col = COLB[4], lwd = graphlinewidth)

dev.off()

