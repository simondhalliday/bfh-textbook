require(shape)
pdf(file = "what_can_markets_do/sequential_coase_restricted.pdf", width = 9, height = 7)

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
     xaxs="i", 
     yaxs="i")

ticksy <- seq(from = 0, to = ylims[2], by = 2)
ylabels <- seq(from = 0, to = ylims[2], by = 2)
ticksx <- seq(from = 0, to = xlims[2], by = 2)
xlabels <- seq(from = 0, to = xlims[2], by = 2)

#segments(5, 9, 2.5, 5, lty = 1, col = rgb(0, 0, 1, 0.5), lwd = graphlinewidth)
#Plant late
segments(5, 9, 7.5, 5, lty = 1, col = rgb(1, 0, 0, 0.5), lwd = graphlinewidth)

#Branches for Bianca
#From Anders planting early
#Bianca plants early
#Arrows(2.5, 5, 1, 1, col = rgb(1, 0, 0, 0.5), lty = 1, lwd = 3, arr.type = "triangle")
segments(2.5, 5, 1, 1, lty = 1, col = rgb(0, 0, 1, 0.5), lwd = graphlinewidth)
#Bianca plants late
segments(2.5, 5, 4, 1, lty = 1, col = rgb(0, 0, 1, 0.5), lwd = graphlinewidth)

#Branches for Anders
#Plant early
#Arrows(5, 9, 2.5, 5, col = rgb(0, 0, 1, 0.5), lty = 1, lwd = 3, arr.type = "triangle")
segments(5, 9, 2.5, 5, col = rgb(1, 0, 0, 0.5), lty = 1, lwd = 3)

#From Anders plants late
#Bianca plants early
#segments(7.5, 5, 10, 1, lty = 1, col = rgb(1, 0, 0, 0.5), lwd = graphlinewidth)


#Bianca plants late
#segments(7.5, 5, 9, 1, lty = 1, col = rgb(0, 0, 1, 0.5), lwd = graphlinewidth)

#Initial node
points(5, 9, pch = 16, col = "black", cex = nodesize)

#P1 Nodes
points(2.5, 5, pch = 16, col = "black", cex = nodesize)
points(7.5, 5, pch = 16, col = rgb(0,0,0,fadelevel), cex = nodesize)

#P2 Terminal Nodes
points(1, 1, pch = 16, col = "black", cex = nodesize)
points(4, 1, pch = 16, col = rgb(0,0,0,fadelevel), cex = nodesize)
#points(6, 1, pch = 16, col = rgb(0,0,0,fadelevel), cex = nodesize)
#points(9, 1, pch = 16, col = rgb(0,0,0,fadelevel), cex = nodesize)
#points(10, 1, pch = 16, col = rgb(0,0,0,fadelevel), cex = nodesize)

text(5, 9.3, expression(paste("Bianca")), cex = labelsize)
text(3, 7.5, expression(paste("Bargain")), cex = actionlabelsize)
text(3, 7.1, expression(paste("(to use)")), cex = actionlabelsize)
text(7.1, 7.5, expression(paste("Don't")), cex = actionlabelsize, col = rgb(0, 0, 0, fadelevel))
text(7.1, 7.1, expression(paste("bargain")), cex = actionlabelsize, col = rgb(0, 0, 0, fadelevel))
text(7.1, 6.7, expression(paste("(restrict)")), cex = actionlabelsize, col = rgb(0, 0, 0, fadelevel))


text(1.7, 5, expression(paste("Anders")), cex = labelsize)
text(1.3, 3.5, expression(paste("Accept")), cex = actionlabelsize)
#text(1.3, 3.1, expression(paste("early")), cex = actionlabelsize)
text(3.8, 3.5, expression(paste("Reject")), cex = actionlabelsize, col = rgb(0,0,0,fadelevel))
#text(3.8, 3.1, expression(paste("late")), cex = actionlabelsize, col = rgb(0,0,0,fadelevel))


#text(8.2, 5, expression(paste("Anders")), cex = labelsize, col = rgb(0,0,0,fadelevel))
#text(9.1, 3.5, expression(paste("Restrict")), cex = actionlabelsize, col = rgb(0,0,0,fadelevel))
#text(6.1, 3.1, expression(paste("early")), cex = actionlabelsize, col = rgb(0,0,0,fadelevel))
#text(9.1, 3.5, expression(paste("Use")), cex = actionlabelsize, col = rgb(0,0,0,fadelevel))
#text(9.1, 3.1, expression(paste("unrestricted")), cex = actionlabelsize, col = rgb(0,0,0,fadelevel))

#Payoffs 
text(1, .6, expression("(" * phantom("2") * "," * phantom("4") * ")"), cex = actionlabelsize)
text(1, .6, expression(phantom("(") * "2" * phantom(",") * phantom("4") * phantom(")")), col = rgb(1, 0, 0, 0.5),  cex = actionlabelsize)
text(1, .6, expression(phantom("(") * phantom("2") * phantom(",") * "4" * phantom(")")), col = rgb(0, 0, 1, 0.5), cex = actionlabelsize)

text(4, .6, expression("(" * phantom("4") * "," * phantom("4") * ")"), cex = actionlabelsize)
text(4, .6, expression(phantom("(") * "4" * phantom(",") * phantom("4") * phantom(")")), col = rgb(1, 0, 0, 0.5),  cex = actionlabelsize)
text(4, .6, expression(phantom("(") * phantom("4") * phantom(",") * "4" * phantom(")")), col = rgb(0, 0, 1, 0.5), cex = actionlabelsize)

text(7.5, 4.6, expression("(" * phantom("4") * "," * phantom("4") * ")"), cex = actionlabelsize)
text(7.5, 4.6, expression(phantom("(") * "4" * phantom(",") * phantom("4") * phantom(")")), col = rgb(1, 0, 0, 0.5),  cex = actionlabelsize)
text(7.5, 4.6, expression(phantom("(") * phantom("4") * phantom(",") * "4" * phantom(")")), col = rgb(0, 0, 1, 0.5), cex = actionlabelsize)

#text(6, .6, expression("(4, 4)"), cex = actionlabelsize, col = rgb(0,0,0,fadelevel))
#text(9, .6, expression("(1, 0)"), cex = actionlabelsize, col = rgb(0,0,0,fadelevel))

#Cutting branch plant late when A plants early
# segments(2.6, 3.9, 3.1, 4.4, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)
# segments(2.8, 3.25, 3.3, 3.75, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)
# segments(3, 2.65, 3.5, 3.15, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)
# segments(3.2, 2.05, 3.7, 2.55, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)
# segments(3.4, 1.45, 3.9, 1.95, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)


#Cutting branch plant late when A plants
# segments(7.4, 3.9, 6.9, 4.4, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)
# segments(7.2, 3.25, 6.7, 3.75, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)
# segments(7, 2.65, 6.5, 3.15, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)
# segments(6.8, 2.05, 6.3, 2.55, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)
# segments(6.6, 1.45, 6.1, 1.95, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)

#Cutting branch plant late for A given B's actions
# segments(5.25, 7.9, 5.75, 8.4, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)
# segments(5.65, 7.3, 6.15, 7.8, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)
# segments(6.05, 6.7, 6.55, 7.2, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)
# segments(6.45, 6.1, 6.95, 6.6, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)
# segments(6.85, 5.5, 7.35, 6, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)


dev.off()
