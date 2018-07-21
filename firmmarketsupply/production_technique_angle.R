require(shape)
pdf(file = "firmmarketsupply/production_technique_angle.pdf", width = 7, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(4, 4, 2, 2))

xlims <- c(0, 12)
ylims <- c(0, 12)

npts <- 501 


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
ticksy <- c(0, 4.5, 6, 7.5, ylims[2])
ylabels <- c(NA, expression(paste(k[1])), expression(paste(k[2])), expression(paste(k[3])), NA)
ticksx <- c(0, 4.5, 6, 7.5, xlims[2])
xlabels <- c(NA, expression(paste(l[1])), expression(paste(l[2])), expression(paste(l[3])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Infeasible area
xpoly <- c(0, 4.5, 4.5, 7.5, xlims[2], xlims[2], 0, 0)
ypoly <- c(ylims[2], ylims[2], 7.5, 4.5, 4.5, 0, 0, ylims[2])
polygon(x = xpoly, y = ypoly, col = COLB[1], density=NULL, border = NA)

#Feasible area
xpoly1 <- c(xlims[2], 7.5, 4.5, 4.5,  xlims[2], xlims[2])
ypoly1 <- c(4.5, 4.5, 7.5, ylims[2], ylims[2], 4.5)
#ypoly1 <- c(ylims[2], ylims[2], 7.5, 4.5, ylims[2])
polygon(x = xpoly1, y = ypoly1, col = COLA[1], density=NULL, border = NA)

#lines(xx1, mrsA(xx1, rmax = 20, xmax = 10), col = COLA[4], lwd = graphlinewidth)
#lines(xx1, brfB(xx1, alpha = 16, beta = 1/24), col = COLB[4], lwd = graphlinewidth)

#Production set
segments(7.5, 4.5, xlims[2], 4.5, lty = 1, col = COLA[3], lwd = graphlinewidth)
segments(4.5, 7.5, 4.5, ylims[2], lty = 1, col = COLA[3],  lwd = graphlinewidth)
segments(4.5, 7.5, 7.5, 4.5, lty = 1, col = COLA[3],  lwd = graphlinewidth)

#Segments to other points
segments(0, 7.5, 4.5, 7.5, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(4.5, 0, 4.5, 7.5, lty = 2, col = "gray", lwd = segmentlinewidth)

#For k2
segments(0, 4.5, 7.5, 4.5, lty = 2, col = "gray", lwd = segmentlinewidth)

#For l2
segments(7.5, 0, 7.5, 4.5, lty = 2, col = "gray", lwd = segmentlinewidth)

mtext(expression(paste("Hours of labor, ", l)), side=1, line = 2.5, cex = axislabelsize)
text(-1.1, 0.5*ylims[2], expression(paste("Amount of capital, ", k)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#For good production technique i
#points(4, 6, pch = 16, col = "black", cex = 1.5)
#text(4.25, 6.25, expression(i))


#Rays from Origin
segments(0, 0, 7.5, 4.5, lty = 3, col = "darkgray", lwd = segmentlinewidth)
segments(0, 0, 4.5, 7.5, lty = 3, col = "darkgray", lwd = segmentlinewidth)
segments(0, 0, 6, 6, lty = 3, col = "darkgray", lwd = segmentlinewidth)

#For production technique a
points(4.5, 7.5, pch = 16, col = "black", cex = 1.5)
text(4.25, 7.75, expression(a))

#For production technique b
points(7.5, 4.5, pch = 16, col = "black", cex = 1.5)
text(7.75, 4.25, expression(b))

#For production technique c
points(6, 6, pch = 16, col = "black", cex = 1.5)
text(6, 5.7, expression(c))



#Label 
text(8, 8, expression(paste("Choice among")), cex = labelsize)
text(8, 7.5, expression(paste("production")), cex = labelsize)
text(8, 7, expression(paste("techniques")), cex = labelsize)

Arrows(7.5, 6.75, 7.5, 4.9, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
Arrows(7, 7.5, 4.9, 7.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
Arrows(7, 7, 6.25, 6.25, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(10, 11, expression(paste("Feasible")), cex = labelsize)
text(10, 10.5, expression(paste("production")), cex = labelsize)
text(10, 10, expression(paste("techniques")), cex = labelsize)

text(10, 3, expression(paste("Infeasible")), cex = labelsize)
text(10, 2.5, expression(paste("production")), cex = labelsize)
text(10, 2, expression(paste("techniques")), cex = labelsize)


text(1.6, 7, expression(paste("Steeper ray:")), cex = labelsize)
text(1.6, 6.5, expression(paste("more k-intensive")), cex = labelsize)
text(1.6, 6, expression(paste("less l-intensive")), cex = labelsize)
Arrows(2.8, 7, 3.95, 7, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


text(6, 2.5, expression(paste("Flatter ray:")), cex = labelsize)
text(6, 2, expression(paste("more l-intensive")), cex = labelsize)
text(6, 1.5, expression(paste("less k-intensive")), cex = labelsize)
Arrows(6, 2.7, 6, 3.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(10, 4.75, expression(paste(bar(x) == 1,", unit output")), cex = labelsize)

dev.off()
