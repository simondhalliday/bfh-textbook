require(shape)
pdf(file = "people/sequential_palanpur_player1.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 2
actionlabelsize <- 1.5
graphlinewidth <- 3
segmentlinewidth <- 2
nodesize <- 1.5
fadelevel <- 0.3

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

#Branches for Aram
#Plant early
segments(5, 9, 2.5, 5, lty = 1, col = rgb(0, 0, 1, 0.5), lwd = graphlinewidth)
#Plant late
segments(5, 9, 7.5, 5, lty = 1, col = rgb(0, 0, 1, 0.5), lwd = graphlinewidth)

#Branches for Bina
#From Aram planting early
#Bina plants early
segments(2.5, 5, 1, 1, lty = 1, col = rgb(1, 0, 0, 0.5), lwd = graphlinewidth)
#Bina plants late
segments(2.5, 5, 4, 1, lty = 1, col = rgb(1, 0, 0, fadelevel), lwd = graphlinewidth)

#From Aram plants late
#Bina plants early
segments(7.5, 5, 6, 1, lty = 1, col = rgb(1, 0, 0, 0.5), lwd = graphlinewidth)


#Bina plants late
segments(7.5, 5, 9, 1, lty = 1, col = rgb(1, 0, 0, 0.5), lwd = graphlinewidth)

#Initial node
points(5, 9, pch = 16, col = "black", cex = nodesize)

#P1 Nodes
points(2.5, 5, pch = 16, col = "black", cex = nodesize)
points(7.5, 5, pch = 16, col = "black", cex = nodesize)

#P2 Terminal Nodes
points(1, 1, pch = 16, col = "black", cex = nodesize)
points(4, 1, pch = 16, col = rgb(0,0,0,fadelevel), cex = nodesize)
points(6, 1, pch = 16, col = rgb(0,0,0,fadelevel), cex = nodesize)
points(9, 1, pch = 16, col = "black", cex = nodesize)

text(5, 9.3, expression(paste("Aram")), cex = labelsize)

text(3.3, 7.5, expression(paste("Plant")), cex = actionlabelsize)
text(3.3, 7.1, expression(paste("early")), cex = actionlabelsize)
text(6.7, 7.5, expression(paste("Plant")), cex = actionlabelsize)
text(6.7, 7.1, expression(paste("late")), cex = actionlabelsize)


text(2, 5, expression(paste("Bina")), cex = labelsize)
text(1.3, 3.5, expression(paste("Plant")), cex = actionlabelsize)
text(1.3, 3.1, expression(paste("early")), cex = actionlabelsize)
text(3.8, 3.5, expression(paste("Plant")), cex = actionlabelsize, col = rgb(0,0,0,fadelevel))
text(3.8, 3.1, expression(paste("late")), cex = actionlabelsize, col = rgb(0,0,0,fadelevel))


text(8, 5, expression(paste("Bina")), cex = labelsize)
text(6.1, 3.5, expression(paste("Plant")), cex = actionlabelsize, col = rgb(0,0,0,fadelevel))
text(6.1, 3.1, expression(paste("early")), cex = actionlabelsize, col = rgb(0,0,0,fadelevel))
text(8.7, 3.5, expression(paste("Plant")), cex = actionlabelsize)
text(8.7, 3.1, expression(paste("late")), cex = actionlabelsize)

#Payoffs 

text(1, .6, expression("(" * phantom("4") * "," * phantom("4") * ")"), cex = actionlabelsize)
text(1, .6, expression(phantom("(") * "4" * phantom(",") * phantom("4") * phantom(")")), col = rgb(0, 0, 1, 0.5),  cex = actionlabelsize)
text(1, .6, expression(phantom("(") * phantom("4") * phantom(",") * "4" * phantom(")")), col = rgb(1, 0, 0, 0.5), cex = actionlabelsize)

text(4, .6, expression("(" * phantom("0") * "," * phantom("3") * ")"), cex = actionlabelsize,col = rgb(0,0,0,fadelevel))
text(4, .6, expression(phantom("(") * "0" * phantom(",") * phantom("3") * phantom(")")), col = rgb(0, 0, 1, 0.5,fadelevel),  cex = actionlabelsize)
text(4, .6, expression(phantom("(") * phantom("0") * phantom(",") * "3" * phantom(")")), col = rgb(1, 0, 0, 0.5,fadelevel), cex = actionlabelsize)

text(6, .6, expression("(" * phantom("3") * "," * phantom("0") * ")"), cex = actionlabelsize,col = rgb(0,0,0,fadelevel))
text(6, .6, expression(phantom("(") * "3" * phantom(",") * phantom("0") * phantom(")")), col = rgb(0, 0, 1, 0.5,fadelevel),  cex = actionlabelsize)
text(6, .6, expression(phantom("(") * phantom("3") * phantom(",") * "0" * phantom(")")), col = rgb(1, 0, 0, 0.5,fadelevel), cex = actionlabelsize)

text(9, .6, expression("(" * phantom("2") * "," * phantom("2") * ")"), cex = actionlabelsize)
text(9, .6, expression(phantom("(") * "2" * phantom(",") * phantom("2") * phantom(")")), col = rgb(0, 0, 1, 0.5),  cex = actionlabelsize)
text(9, .6, expression(phantom("(") * phantom("2") * phantom(",") * "2" * phantom(")")), col = rgb(1, 0, 0, 0.5), cex = actionlabelsize)

#Cutting branch plant late when A plants early
segments(2.6, 3.9, 3.1, 4.4, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)
segments(2.8, 3.25, 3.3, 3.75, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)
segments(3, 2.65, 3.5, 3.15, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)
segments(3.2, 2.05, 3.7, 2.55, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)
segments(3.4, 1.45, 3.9, 1.95, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)


#Cutting branch plant late when A plants
segments(7.4, 3.9, 6.9, 4.4, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)
segments(7.2, 3.25, 6.7, 3.75, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)
segments(7, 2.65, 6.5, 3.15, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)
segments(6.8, 2.05, 6.3, 2.55, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)
segments(6.6, 1.45, 6.1, 1.95, lty = 1, col = rgb(0,0,0,fadelevel), lwd = graphlinewidth)


dev.off()
