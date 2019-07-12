pdf(file = "indmarketdemand/sugar_bev_demand.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(6, 6, 4, 4))

indiffA <- function(x, alpha = 0.5, uA = 5) {
  (uA / x^alpha)^(1/(1 - alpha))
}

cef <- function(x, e = 1.4, k){
  k*(x^(-1/e))
}


xlims <- c(0, 300)
ylims <- c(0, 3)

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

ticksy <- c(0, 1.25, 1.375, ylims[2])
ylabels <- c(NA, expression(paste(p[0])), expression(paste(p + tp[1])), NA)
ticksx <- c(0, 129, 150, xlims[2])
xlabels <- c(0, expression(paste(x[1])), expression(paste(x[0])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Label axes
mtext(expression(paste("Quantity of sugary drinks (liters), ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-40, 0.5*ylims[2], expression(paste("Price per liter, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 


# Segments
segments(150, 0, 150, 1.25, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 1.25, 150, 1.25, lty = 2, col = "gray" , lwd = segmentlinewidth)

segments(0, 1.375, 129, 1.375, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(129, 0, 129, 1.375, lty = 2, col = "gray" , lwd = segmentlinewidth)


# Demand Curve
#lines(xx1, indiffA(xx1, alpha = 0.38723, uA = 7.98053), col = COLA[5], lwd = graphlinewidth)
lines(xx1, cef(xx1, k = 44.7982), col = COLA[5], lwd = graphlinewidth)
text(xlims[2] - 0.05*xlims[2], 0.7, expression(Demand))

# Points
points(150, 1.25, pch = 16, col = "black", cex = 1.5)
text(155, 1.3, expression(a))

points(129, 1.375, pch = 16, col = "black", cex = 1.5)
text(134, 1.425, expression(b))

# Label Regions
text(140, 1.3, expression(A))
text(50, 1, expression(B))
text(50, 1.3, expression(C))
text(50, 1.75, expression(D))




dev.off()
