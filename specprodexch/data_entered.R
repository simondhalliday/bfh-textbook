#Graph Designer: Simon Halliday + Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "specprodexch/data_entered.pdf", width = 8, height = 8)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 4, 4))

xlims <- c(0, 17)
ylims <- c(0, 17)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(3.25, 5.25, 7.25)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i"
)


# ticksy <- seq(from = 0, to = ylims[2], by = 1)
# ylabels <- seq(from = 0, to = ylims[2], by = 1)
# ticksx <- seq(from = 0, to = xlims[2], by = 1)
# xlabels <- seq(from = 0, to = xlims[2], by = 1)
ticksy <- c(ylims[1], 8, 10, 15, ylims[2])
ylabels <- c(NA, 8, 10, 15, NA)
ticksx <- c(xlims[1], 8.571, 10, 12.857, xlims[2])
xlabels <- c(NA, 8.571, 10, 12.857, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Draw the graphs
segments(0, 0, 13, 13, lty = 1, col = "dark grey",  lwd = graphlinewidth)

segments(0, 10, 8.571, 0, lty = 2, col = COLB[3],  lwd = graphlinewidth)
segments(0, 15, 12.857, 0, lty = 2, col = COLB[3],  lwd = graphlinewidth)

segments(0, 8, 8.571, 0, lty = 1, col = COLA[3],  lwd = graphlinewidth)
segments(0, 15, 10, 0, lty = 1, col = COLA[3],  lwd = graphlinewidth)

segments(0, 4.14, 4.14, 4.14, lty = 2, col = "grey",  lwd = segmentlinewidth)
segments(4.14, 0, 4.14, 4.14, lty = 2, col = "grey",  lwd = segmentlinewidth)

#Label Points
points(4.14, 4.14, pch = 16, col = "black", cex = 1.5)
text(4.14 - 0.5, 4.14 - 0.25, expression(a))

#Axis labels
mtext(expression(paste("Data entered ('000's), ", x)), side = 1, line = 2.5, cex = axislabelsize)
text(-1.5, 8.5, expression(paste("Graphs made, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Text
text(13, 14.25, expression(paste("Each graph requires")))
text(13, 13.75, expression(paste("1250 keystrokes of data")))

#Label the curves
text(1, 10, expression(p[1]), cex = labelsize)
text(1, 15, expression(p[2]), cex = labelsize)

text(1, 12, expression(PPF[A]), cex = labelsize)
text(1, 6, expression(PPF[B]), cex = labelsize)

dev.off()
