
pdf(file = "specprodexch/leontief_comparison.pdf", width = 7, height = 7)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5


COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(5, 5, 1, 1))

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

ticksy <- c(0, 2, 4, 6, ylims[2])
ylabels <- c(NA, expression(paste(k[1])), expression(paste(k[2])), expression(paste(k[3])), NA)
ticksx <- c(0, 2, 4, 6, xlims[2])
xlabels <- c(NA, expression(paste(l[1])), expression(paste(l[2])), expression(paste(l[3])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels,cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
abline(0,1)

segments(2, 2, xlims[2], 2, lty = 1, col = COLA[4], lwd = graphlinewidth)
segments(2, 2, 2, ylims[2], lty = 1, col = COLA[4],  lwd = graphlinewidth)

segments(4, 4, xlims[2], 4, lty = 1, col = COLA[4], lwd = graphlinewidth)
segments(4, 4, 4, ylims[2], lty = 1, col = COLA[4],  lwd = graphlinewidth)

segments(6, 6, xlims[2], 6, lty = 1, col = COLA[4], lwd = graphlinewidth)
segments(6, 6, 6, ylims[2], lty = 1, col = COLA[4],  lwd = graphlinewidth)


mtext(expression(paste("Hours of labor, ", l)), side=1, line = 3, cex = axislabelsize)
text(-1.7, 0.5*ylims[2], expression(paste("Quantity of capital goods, ", k)), xpd = TRUE, cex = axislabelsize, srt = 90) 

# points(4, 6, pch = 16, col = "black", cex = 1.5)
text(11, 2.6, expression(paste(underline(x)[1]^L)), cex = labelsize)
text(11, 4.6, expression(paste(underline(x)[2]^L)), cex = labelsize)
text(11, 6.6, expression(paste(underline(x)[3]^L)), cex = labelsize)


# text(10, 11, expression(paste("Feasible")), cex = labelsize)
# text(10, 10.5, expression(paste("production")), cex = labelsize)
# text(10, 10, expression(paste("techniques")), cex = labelsize)
# text(2, 4, expression(paste("Infeasible")), cex = labelsize)
# text(2, 3.5, expression(paste("production")), cex = labelsize)
# text(2, 3, expression(paste("techniques")), cex = labelsize)


#Label mrs function
text(18, 2.5, expression(paste(mrs(x,y) == r[max] - bgroup("(",frac(r[max], x[max]),")")*x)), cex = labelsize)

dev.off()