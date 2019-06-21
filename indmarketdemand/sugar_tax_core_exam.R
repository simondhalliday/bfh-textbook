require(shape)
pdf(file = "indmarketdemand/sugar_tax_core_exam.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(6, 6, 4, 4))

mrsA <- function(x, rmax = 20, xmax = 10) {
  rmax - (rmax/xmax)*x
}

uA <- function(x, y, rmax = 20, xmax = 10) {
  y + rmax*x - (1/2)(rmax/xmax)*x^2
}

Qs <- function(x, tax = 0){
  x + 5 + tax
}

Qs_tax<- function(x, tax = 3){
  x + 5 + tax
}

xlims <- c(0, 11)
ylims <- c(0, 22)

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
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
# ticksx <- seq(from = 0, to = xlims[2], by = 2)
# xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(0, 9, 10, 12, 20, ylims[2])
ylabels <- c(NA, expression(paste(p[d] == 9)), expression(paste(p[b] == 10)), expression(paste(p[a] == 12)), expression(paste(20)), NA)
ticksx <- c(0, 4, 5, 10, xlims[2])
xlabels <- c(0, expression(paste(x[a]) == 4), expression(paste(x[b]) == 5), expression(paste(10)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

# Tax Rev Q
xpoly <- c(4, 4, 5)
ypoly <- c(9, 10, 10)
polygon(x = xpoly, y = ypoly, col = COL[2], density=NULL, border = NA)

# Tax Rev P 
xpoly1 <- c(0, 4, 4, 0)
ypoly1 <- c(9, 9, 12, 12)
polygon(x = xpoly1, y = ypoly1, col = COL[3], density=NULL, border = NA)

# DWL
xpoly2 <- c(4, 5, 4) 
ypoly2 <- c(10, 10, 12) # Change y1 to 10 to meet gray line
polygon(x = xpoly2, y = ypoly2, col = COL[4], density=NULL, border = NA)

# CS 
xpoly3 <- c(0, 0, 4)
ypoly3 <- c(12, 20, 12)
polygon(x = xpoly3, y = ypoly3, col = COLA[1], density=NULL, border = NA)

# PS
xpoly4 <- c(0, 0, 4, 4, 0)
ypoly4 <- c(9, Qs(0), Qs(4), 9, 9)
polygon(x = xpoly4, y = ypoly4, col = COLB[1], density=NULL, border = NA)


#Lines for mrs graph
lines(xx1, mrsA(xx1, rmax = 20, xmax = 10), col = COLA[4], lwd = graphlinewidth)

lines(xx1, Qs(xx1), col = COLB[4], lwd = graphlinewidth)
lines(xx1, Qs_tax(xx1), col = COLB[5], lwd = graphlinewidth)

# Market Price
segments(0, 12, 4, 12, lty = 2, col = "gray" , lwd = segmentlinewidth) 
segments(0, 10, 5, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)

# Vert Seg from Q 
segments(5, 0, 5, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(4, 0, 4, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)

# Q*
segments(2, 0, 2, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)

#Label axes
mtext(expression(paste("Quantity of sugary drinks (thousands of six-packs), ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-1.4, 0.5*ylims[2], expression(paste("Price per six-pack, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label a
points(4, 12, pch = 16, col = "black", cex = 1.5)
text(4, 12.75, expression(a))

#Label b
points(5, 10, pch = 16, col = "black", cex = 1.5)
text(5, 10.75, expression(b))

#Label c
points(4, 10, pch = 16, col = "black", cex = 1.5)
text(4 - 0.2, 10 - 0.25, expression(c))

#Label d
points(4, Qs(4) , pch = 16, col = "black", cex = 1.5)
text(4 + 0.2, Qs(4) - 0.25, expression(d))

#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
text(8.5, 19, expression(paste("Supply with tax, ", p(x) == 8 + x)), cex = labelsize)
text(7.75, 10.5, expression(paste("Supply, ", p(x) == 5 + x)), cex = labelsize)

#Label mrs function
text(8.05, 5.5, expression(paste("Demand")), cex = labelsize)
Arrows(6, 11.6, 6, 13.4, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.2, code = 3)
text(6.6, 12.7, expression(paste("Tax" == 3)), cex = labelsize)

# Label CS
#text(1.2, 14, expression(paste("Consumer Surplus")))


dev.off()