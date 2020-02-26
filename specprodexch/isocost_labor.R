#require(shape)
pdf(file = "specprodexch/isocost_labor.pdf", width = 7, height = 7)

#Set parameters for graphics
axislabelsize <- 1.3
axistitlesize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

xlims <- c(0, 18)
ylims <- c(0, 18)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(46.08, 55, 64)
b <- c(46.08, 55, 64)

#Margins
par(mar =  c(4, 4, 1, 1))


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
ylabels <- c(NA, expression(paste(k[g])), expression(paste(k[f])), NA)
ticksx <- c(0, 2, 8, xlims[2])
xlabels <- c(NA, expression(paste(l[f])), expression(paste(l[g])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = axislabelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = axislabelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Isoquants
segments(2, ylims[2], 2, 8, lty = 1, col = "grey22" , lwd = graphlinewidth)
segments(2, 8, xlims[2], 8, lty = 1, col = "grey22" , lwd = graphlinewidth)

#Isoquants
segments(8, 2, 8, ylims[2], lty = 1, col = "grey22" , lwd = graphlinewidth)
segments(8, 2, xlims[2], 2, lty = 1, col = "grey22" , lwd = graphlinewidth)



# c_1^H
segments(0, 18, 9, 0, lty = 1, col = COLB[4] , lwd = graphlinewidth) 
text(0.75, 11.5, expression(paste(c[1]^H)), cex = labelsize)
# c_2^H
segments(0, 12, 6, 0, lty = 1, col = COLB[4] , lwd = graphlinewidth) 
text(0.75, 17.5, expression(paste(c[2]^H)), cex = labelsize)
# c_1^L
segments(0, 9, 18, 0, lty = 1, col = COLA[4] , lwd = graphlinewidth)
text(11.5, 0.75, expression(paste(c[1]^L)), cex = labelsize)
# c_2^L
segments(0, 6, 12, 0, lty = 1, col = COLA[4] , lwd = graphlinewidth) 
text(17.5, 0.75, expression(paste(c[2]^L)), cex = labelsize)


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
mtext(expression(paste("Hours of labor, l")), side=1, line = 2.5, cex = axistitlesize)
text(-1.5, 0.5*ylims[2], expression(paste("Quantity of capital goods, k")), xpd = TRUE, cex = axistitlesize, srt = 90) 

#Label isoquants
text(3.5, 17, expression(paste("Isoquant")), cex = labelsize)
text(3.5, 16.2, expression(paste(x[f](l[f],k[f]) == underline(x))), cex = labelsize)
text(9.5, 17, expression(paste("Isoquant")), cex = labelsize)
text(9.5, 16.2, expression(paste(x[g](l[f],k[f]) == underline(x))), cex = labelsize)

# Label Isocost H
text(12, 4.8, expression(paste("Isocost curves")), cex = labelsize)
text(12, 4, expression(paste("(low wage)")), cex = labelsize, col = COLA[4])

# Label Isocost L
text(5, 12.4, expression(paste("Isocost curves")), cex = labelsize)
text(5, 11.6, expression(paste("(high wage)")), cex = labelsize, col = COLB[5])
# Label Point 1 + 2
points(2, 8, pch = 16, col = "black", cex = 1.5)
text(1.7, 7.7, expression(f))

points(8, 2, pch = 16, col = "black", cex = 1.5)
text(7.7, 1.7, expression(g))



dev.off()