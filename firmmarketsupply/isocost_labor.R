#require(shape)
pdf(file = "firmmarketsupply/isocost_labor.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

xlims <- c(0, 18)
ylims <- c(0, 18)

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

ticksy <- c(0, 2, 8, ylims[2])
ylabels <- c(NA, expression(paste(a[k]^g)), expression(paste(a[k]^f)), NA)
ticksx <- c(0, 2, 8, xlims[2])
xlabels <- c(0, expression(paste(a[L]^f)), expression(paste(a[L]^g)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

# c_1^H
segments(0, 18, 9, 0, lty = 1, col = COLB[4] , lwd = graphlinewidth) 
text(1.5, 10, expression(paste(C[1]^H)), cex = labelsize)
# c_2^H
segments(0, 12, 6, 0, lty = 1, col = COLB[4] , lwd = graphlinewidth) 
text(1.5, 16, expression(paste(C[2]^H)), cex = labelsize)
# c_1^L
segments(0, 9, 18, 0, lty = 1, col = COLA[4] , lwd = graphlinewidth)
text(10, 1.5, expression(paste(C[1]^L)), cex = labelsize)
# c_2^L
segments(0, 6, 12, 0, lty = 1, col = COLA[4] , lwd = graphlinewidth) 
text(16, 1.5, expression(paste(C[2]^L)), cex = labelsize)
# Label Lines

# Segment af_k to 2
segments(0, 8, 2, 8, lty = 2, col = "gray" , lwd = segmentlinewidth)

# Segment af_L to 2
segments(2, 0, 2, 8, lty = 2, col = "gray" , lwd = segmentlinewidth)

# Segment ag_k to 1
segments(0, 2, 8, 2, lty = 2, col = "gray" , lwd = segmentlinewidth)

# Segment ag_L to 1
segments(8, 0, 8, 2, lty = 2, col = "gray" , lwd = segmentlinewidth)

# Label x,y axis

mtext(expression(paste("Labor Input (Hours)")), side=1, line = 2.5, cex = axislabelsize)
text(-1.4, 0.5*ylims[2], expression(paste("Capital Goods Input (Horsepower)")), xpd = TRUE, cex = axislabelsize, srt = 90) 


# Label Isocost H
text(12, 6, expression(paste("Isocost Curves")), cex = labelsize)
text(12, 5, expression(paste("(Low Wage)")), cex = labelsize)
# Label Isocost L
text(6, 12, expression(paste("Isocost Curves")), cex = labelsize)
text(6, 11, expression(paste("(High Wage)")), cex = labelsize)
# Label Point 1 + 2
points(2, 8, pch = 16, col = "black", cex = 1.5)
text(2.5, 8.5, expression(f))

points(8, 2, pch = 16, col = "black", cex = 1.5)
text(8.5, 2.5, expression(g))



dev.off()