require(shape)
require(plotrix)
pdf(file = "employment/employment_unions.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 3

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

WageFn <- function(H, delta = 5) {
  delta /(1 - H)
}

#COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")
par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 1.2)
ylims <- c(0, 40)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("Total hours of employment as a proportion, ", H)),
     ylab = expression(paste("Wage, ", w)),
     xaxt = "n", 
     yaxt = "n",
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i")

npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], 0.9, length.out = npts)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
xx4 <- seq(xlims[1], 25, length.out = npts2)

#Draw the lines for the graphs
lines(xx1, WageFn(xx1), col = COL[1], lwd = 4)
#lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 4)
#lines(xx2, solowInfeas(xx2, delta = 5), col = COL[1], lwd = 4, lty = 2)

#Customize ticks and labels for the plot
ticksy <- c(0, 2.5, 5, 20, 25,  40)
ylabels <- c(0, expression(paste(B)), expression(paste(B+a/t)), expression(paste(w[0])), expression(paste(w[2])), NA)
ticksx <- c(0, 0.75, 0.8, 1, xlims[2])
xlabels <- c(0, expression(paste(H[0],"*")), expression(paste(H[2],"*")), 1.0, NA)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Annotation of the  graphs
text(0.72, 35, expression(paste("Wage Curve ", w^N*(H))))

#segments(1, 0, 1, 42, lty = 2, lwd = 3, col = "darkgray")
segments(0.75, 0, 0.75, 20, lty = 2, lwd = 2, col = "darkgray")

#Arrows(0.85, 15, 0.85, 24, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#Arrows(0.85, 15, 0.85, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#text(0.98, 14.5, expression(paste("Employment rent")))
#text(0.98, 12.5, expression(paste("increases with union")))

#Original Zero profit condition 
segments(0, 20, 0.75, 20, lty = 1, lwd = graphlinewidth, col = COLB[3])
segments(0.75, 20, 1.2, 20, lty = 2, lwd = segmentlinewidth, col = COLB[3])

points(0.75, 20, pch = 16, col = "black", cex = 1.5)
text(0.74, 21, expression(paste(n[0])))


#Union Zero profit condition 
segments(0, 25, 0.8, 25, lty = 1, lwd = graphlinewidth, col = COLB[3])
segments(0.8, 25, 1.2, 25, lty = 2, lwd = segmentlinewidth, col = COLB[3])


Arrows(0.2, 20.5, 0.2, 23.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(0.36, 22.5, expression(paste("Union raises productivity")))

segments(0.8, 0, 0.8, 25, lty = 2, lwd = 2, col = "darkgray")
points(0.8, 25, pch = 16, col = "black", cex = 1.5)
text(0.79, 26, expression(paste(n[2])))
text(1.02, 26, expression(paste(zpc[2], ", ", w == w[2])))


#Unemployment benefits & a
#segments(0, 5, 1.2, 5, lty = 2, lwd = 2, col = "darkgray")
segments(0, 2.5, 1.2, 2.5, lty = 2, lwd = 2, col = "darkgray")

#Zero profit condition
text(1.02, 21, expression(paste(zpc[0], ", ", w == w[0])))
#text(0.97, 6, expression(paste(B + a)))
#text(0.97, 3.5, expression(paste(B, " (unemployment benefits)")))
#text(1.08, 36, expression(paste("level of")))
#text(1.08, 34, expression(paste("employment, ", bar(H))))


dev.off()

