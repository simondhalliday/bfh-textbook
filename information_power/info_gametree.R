require(shape)
pdf(file = "information_power/info_gametree.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 2
actionlabelsize <- 1.5
graphlinewidth <- 3
segmentlinewidth <- 2
nodesize <- 1.5
fadelevel <- 1

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
     xaxs = "i", 
     yaxs  ="i")

ticksy <- seq(from = 0, to = ylims[2], by = 2)
ylabels <- seq(from = 0, to = ylims[2], by = 2)
ticksx <- seq(from = 0, to = xlims[2], by = 2)
xlabels <- seq(from = 0, to = xlims[2], by = 2)

# Agent
segments(5, 9, 3, 5, col = COLB[4], lty = 1, lwd = 3)
segments(5, 9, 9, 1, lty = 1, col = COLB[4], lwd = graphlinewidth)


# Principal
segments(3, 5, 1, 1, lty = 1, col = COLA[4], lwd = graphlinewidth)
segments(3, 5, 5, 1, lty = 1, col = COLA[4], lwd = graphlinewidth)

#Initial node
points(5, 9, pch = 16, cex = nodesize)

#P1 Nodes
points(3, 5, pch = 16, cex = nodesize)

#P2 Terminal Nodes
points(1, 1, pch = 16, cex = nodesize)
points(5, 1, pch = 16, cex = nodesize)
points(9, 1, pch = 16, cex = nodesize)

text(5, 9.3, expression(paste("Agent")), cex = labelsize)
text(3, 7.5, expression(paste("Low quality")), cex = actionlabelsize)
text(3, 7.1, expression(paste("(cost = ", underline(c), ")")), cex = actionlabelsize)
text(6.9, 7.5, expression(paste("High quality")), cex = actionlabelsize)
text(6.9, 7.1, expression(paste("(cost = ", bar(c), ")")), cex = actionlabelsize)


text(1.8, 5, expression(paste("Principal")), cex = labelsize)
text(1.3, 3.5, expression(paste("Terminate")), cex = actionlabelsize)
text(5, 3.5, expression(paste("Don't terminate")), cex = actionlabelsize)

#Payoffs 
text(1, .6, expression(z - underline(c)), cex = actionlabelsize)
text(5, .6, expression(p - underline(c)), cex = actionlabelsize)
text(9, .6, expression(p - bar(c)), cex = actionlabelsize)



dev.off()