require(shape)
pdf(file = "people/sequential_fishermen_player1.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 2
actionlabelsize <- 1.5
graphlinewidth <- 3
segmentlinewidth <- 2
nodesize <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(0, 0, 0, 0))

xlims <- c(0, 10)
ylims <- c(0, 10)

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

ticksy <- seq(from = 0, to = ylims[2], by = 2)
ylabels <- seq(from = 0, to = ylims[2], by = 2)
ticksx <- seq(from = 0, to = xlims[2], by = 2)
xlabels <- seq(from = 0, to = xlims[2], by = 2)

#Branches for Alfredo
#Fish 10
segments(5, 9, 2.5, 5, lty = 1, col = rgb(0, 0, 1, 0.5), lwd = graphlinewidth)
#Fish 12
segments(5, 9, 7.5, 5, lty = 1, col = rgb(0, 0, 1, 0.5), lwd = graphlinewidth)
#Initial node
points(5, 9, pch = 16, col = "black", cex = nodesize)

#P1 Nodes
points(2.5, 5, pch = 16, col = "black", cex = nodesize)
points(7.5, 5, pch = 16, col = "black", cex = nodesize)


text(5, 9.3, expression(paste("Alfredo")), cex = labelsize)

text(3, 7.5, expression(paste("Fish")), cex = actionlabelsize)
text(3, 7, expression(paste("10 Hours")), cex = actionlabelsize)
text(7, 7.5, expression(paste("Fish")), cex = actionlabelsize)
text(7, 7, expression(paste("12 Hours")), cex = actionlabelsize)

#Payoffs 
text(2.5, 4.6, expression("(1, 4)"), cex = actionlabelsize)
text(7.5, 4.6, expression("(2, 2)"), cex = actionlabelsize)

dev.off()
