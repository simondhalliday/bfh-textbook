
pdf(file = "firmmarketsupply/leontief.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 5, 4, 4))

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

ticksy <- c(0, 6, ylims[2])
ylabels <- c(NA, expression(paste(k[1])), NA)
ticksx <- c(0, 4, xlims[2])
xlabels <- c(NA, expression(paste(l[1])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

segments(4, 6, xlims[2], 6, lty = 1, col = COLA[4], lwd = graphlinewidth)
segments(4, 6, 4, ylims[2], lty = 1, col = COLA[4],  lwd = graphlinewidth)

mtext(expression(paste("Hours of labor, ", l)), side=1, line = 2.5, cex = axislabelsize)
text(-1, 0.5*ylims[2], expression(paste("Amount of Capital, ", k)), xpd = TRUE, cex = axislabelsize, srt = 90) 

points(4, 6, pch = 16, col = "black", cex = 1.5)
text(4.25, 6.25, expression(i))


text(10, 11, expression(paste("Feasible")), cex = labelsize)
text(10, 10.5, expression(paste("production")), cex = labelsize)
text(10, 10, expression(paste("techniques")), cex = labelsize)
text(2, 4, expression(paste("Infeasible")), cex = labelsize)
text(2, 3.5, expression(paste("production")), cex = labelsize)
text(2, 3, expression(paste("techniques")), cex = labelsize)


#Label mrs function
text(18, 2.5, expression(paste(mrs(x,y) == r[max] - bgroup("(",frac(r[max], x[max]),")")*x)))

dev.off()