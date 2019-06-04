pdf(file = "indmarketdemand/elasticity.pdf", width = 9, height = 7)

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

xlims <- c(0, 12)
ylims <- c(0, 12)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

ticksx <- c(0, xlims[2])
ticksy <- c(0, ylims[2])
ylabels <- c(NA, "p")
xlabels <- c(0, "x")

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Label axes
mtext(expression(paste("Quantity Demanded, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-1, 0.5*ylims[2], expression(paste("Price, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 

# Demand
segments(0, 10, 5, 5, lty = 1, col = COLB[4] , lwd = graphlinewidth) 
segments(5, 5, 10, 0, lty = 1, col = COLA[4] , lwd = graphlinewidth) 



#Label 
text(5.5, 5.5, expression(paste("Unit Elastic")))
text(6, 5, expression(paste("n = 1")))
text(3, 8, expression(paste("Elastic")))
text(3.5, 7.5, expression(paste("n > 1")))
text(8, 3, expression(paste("Inelastic")))
text(8.5, 2.5, expression(paste("n < 1")))

text(1.25, 10, expression(paste("Perfectly Elastic")))
text(1.25, 9.5, expression(paste("n  = ", infinity)))
text(10.3, 1, expression(paste("Perfectly Inelastic")))
text(10.3, 0.5, expression(paste("n  = 0")))
     
# Points
points(5, 5, pch = 16, col = "black", cex = 1.5)
points(2.5, 7.5, pch = 16, col = "black", cex = 1.5)
points(7.5, 2.5, pch = 16, col = "black", cex = 1.5)

dev.off()