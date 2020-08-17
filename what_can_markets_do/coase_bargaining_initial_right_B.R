require(ggplot2)
require(shape)
require(plotrix)

pdf(file = "what_can_markets_do/coase_bargaining_initial_right_B.pdf", width = 9, height = 7)

#Set parameters for graphics
namesize <- 1.3
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5



#Colors
COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")
grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")

upf <- function(ub) {
  8 - ub
}


uA <- function(xA, yA, alpha = 1/2){
  ((xA)^alpha)*((yA)^(1 - alpha))
}


par(mar =  c(4, 4, 1, 1))
xlims <- c(0, 8)
ylims <- c(0, 8)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = "",
     ylab = "",
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(xlims[1], xlims[2], length.out = npts2)

text(0.5*xlims[2], -1.3, expression(paste("Payoffs to A")), xpd = TRUE, cex = axislabelsize) 
text(-0.9, 0.5*ylims[2], expression(paste("Payoffs to B")), xpd = TRUE, cex = axislabelsize, srt = 90) 


#polygons
xpoly1 <- c(1,3,1)
ypoly1 <- c(5,5,7)
polygon(x = xpoly1, y = ypoly1, col=COL[4], density=NULL, border = NA)


#Draw the lines for the graphs
lines(xx1, upf(xx1), col = CBCols[1], lwd = graphlinewidth)

#Customize ticks and labels for the plot
ticksy <- seq(from = ylims[1], to = ylims[2], by = 1)
ylabels <- seq(from = ylims[1], to = ylims[2], by = 1)
ticksx <- seq(from =  xlims[1], to = xlims[2], by = 1)
xlabels <- seq(from = xlims[1], to = xlims[2], by = 1)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)


#segments:
#segments(4,  0, 4, 4, lty = 2, col = "darkgray", lwd = 2)
#segments(0, 4, 4, 4, lty = 2, col = "darkgray", lwd = 2)

segments(3,  0, 3, 5, lty = 2, col = "darkgray", lwd = 2)
segments(0, 5, 3, 5, lty = 2, col = "darkgray", lwd = 2)

segments(1,  0, 1, 7, lty = 2, col = "darkgray", lwd = 2)
segments(0, 7, 1, 7, lty = 2, col = "darkgray", lwd = 2)

segments(1,  5, 3.5, 5, lty = 2, col = "black", lwd = 2)
segments(1, 5, 1, 7.5, lty = 2, col = "black", lwd = 2)

#points:
#points(4,  4, pch = 16, col = "black", cex = 1.5)
#text(4-0.3,  4-0.3, expression(Z[a]), cex = annotatesize)

points(1, 5, pch = 16, col = "black", cex = 1.5)
text(1 - 0.3, 5 - 0.3, expression(Z[b]), cex = annotatesize)

points(2, 6, pch = 16, col = "black", cex = 1.5)
text(2+0.2, 6+0.2, expression(N), cex = annotatesize)

#label
text(5.4, 1, expression("Feasible payoffs if restrict"), cex = annotatesize)

dev.off()

