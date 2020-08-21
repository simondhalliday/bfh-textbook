require(shape)
pdf(file = "people/sequential_disagreement.pdf", width = 9, height = 7)

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

#Branches for Aisha
#Swahili
segments(5, 9, 2.5, 5, lty = 1, col = rgb(0, 0, 1, 0.5), lwd = graphlinewidth)
#English
segments(5, 9, 7.5, 5, lty = 1, col = rgb(0, 0, 1, 0.5), lwd = graphlinewidth)

#Branches for Ben
#From Aisha speaking Swahili
#Ben speaking Swahili
segments(2.5, 5, 1, 1, lty = 1, col = rgb(1, 0, 0, 0.5), lwd = graphlinewidth)
#Ben speaking English
segments(2.5, 5, 4, 1, lty = 1, col = rgb(1, 0, 0, 0.5), lwd = graphlinewidth)

#From Aisha Speakign English
#Ben speaking Swahili
segments(7.5, 5, 6, 1, lty = 1, col = rgb(1, 0, 0, 0.5), lwd = graphlinewidth)

#Ben Speaks English
segments(7.5, 5, 9, 1, lty = 1, col = rgb(1, 0, 0, 0.5), lwd = graphlinewidth)

#Initial node
points(5, 9, pch = 16, col = "black", cex = nodesize)

#P1 Nodes
points(2.5, 5, pch = 16, col = "black", cex = nodesize)
points(7.5, 5, pch = 16, col = "black", cex = nodesize)

#P2 Terminal Nodes
points(1, 1, pch = 16, col = "black", cex = nodesize)
points(4, 1, pch = 16, col = "black", cex = nodesize)
points(6, 1, pch = 16, col = "black", cex = nodesize)
points(9, 1, pch = 16, col = "black", cex = nodesize)

text(5, 9.3, expression(paste("Aisha")), cex = labelsize)

text(3.2, 7.5, expression(paste("Stick to")), cex = actionlabelsize)
text(3.2, 7.1, expression(paste("Swahili")), cex = actionlabelsize)
text(6.8, 7.5, expression(paste("Improve")), cex = actionlabelsize)
text(6.8, 7.1, expression(paste("English")), cex = actionlabelsize)


text(2, 5, expression(paste("Ben")), cex = labelsize)
text(1.2, 3.5, expression(paste("Improve")), cex = actionlabelsize)
text(1.2, 3.1, expression(paste("Swahili")), cex = actionlabelsize)
text(3.8, 3.5, expression(paste("Stick to")), cex = actionlabelsize)
text(3.8, 3.1, expression(paste("English")), cex = actionlabelsize)


text(8, 5, expression(paste("Ben")), cex = labelsize)
text(6.2, 3.5, expression(paste("Improve")), cex = actionlabelsize)
text(6.2, 3.1, expression(paste("Swahili")), cex = actionlabelsize)
text(8.8, 3.5, expression(paste("Stick to")), cex = actionlabelsize)
text(8.8, 3.1, expression(paste("English")), cex = actionlabelsize)

#Payoffs 

text(1, .6, expression("(" * phantom("4") * "," * phantom("2") * ")"), cex = actionlabelsize)
text(1, .6, expression(phantom("(") * "4" * phantom(",") * phantom("2") * phantom(")")), col = rgb(0, 0, 1, 0.5),  cex = actionlabelsize)
text(1, .6, expression(phantom("(") * phantom("4") * phantom(",") * "2" * phantom(")")), col = rgb(1, 0, 0, 0.5), cex = actionlabelsize)

text(4, .6, expression("(" * phantom("0") * "," * phantom("0") * ")"), cex = actionlabelsize)
text(4, .6, expression(phantom("(") * "0" * phantom(",") * phantom("0") * phantom(")")), col = rgb(0, 0, 1, 0.5),  cex = actionlabelsize)
text(4, .6, expression(phantom("(") * phantom("0") * phantom(",") * "0" * phantom(")")), col = rgb(1, 0, 0, 0.5), cex = actionlabelsize)

text(6, .6, expression("(" * phantom("0") * "," * phantom("0") * ")"), cex = actionlabelsize)
text(6, .6, expression(phantom("(") * "0" * phantom(",") * phantom("0") * phantom(")")), col = rgb(0, 0, 1, 0.5),  cex = actionlabelsize)
text(6, .6, expression(phantom("(") * phantom("0") * phantom(",") * "0" * phantom(")")), col = rgb(1, 0, 0, 0.5), cex = actionlabelsize)

text(9, .6, expression("(" * phantom("2") * "," * phantom("4") * ")"), cex = actionlabelsize)
text(9, .6, expression(phantom("(") * "2" * phantom(",") * phantom("4") * phantom(")")), col = rgb(0, 0, 1, 0.5),  cex = actionlabelsize)
text(9, .6, expression(phantom("(") * phantom("2") * phantom(",") * "4" * phantom(")")), col = rgb(1, 0, 0, 0.5), cex = actionlabelsize)

dev.off()
