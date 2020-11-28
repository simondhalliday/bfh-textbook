require(shape)
pdf(file = "capitalism/sequential_no_shirking_dual.pdf", width = 9, height = 7)

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

par(mar =  c(0, 5, 4, 0))

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

#Branches for Agent
segments(5, 9, 5, 10, lty = 1, col = rgb(0, 0, 1, 0.5), lwd = graphlinewidth, xpd = TRUE)
#Fish 10
segments(5, 9, 1, 1, lty = 1, col = rgb(0, 0, 1, 0.5), lwd = graphlinewidth)
#Fish 12
segments(5, 9, 9, 1, lty = 1, col = rgb(0, 0, 1, 0.5), lwd = graphlinewidth)

#Branches for Agent
#From Aram Fishing 10
#Bina fishes 10
segments(3.5, 6, 6, 1, lty = 1, col = rgb(1, 0, 0, 0.5), lwd = graphlinewidth)
#Bina fishes 12
segments(2.5, 4, 4, 1, lty = 1, col = rgb(1, 0, 0, 0.5), lwd = graphlinewidth)

#From Aram fishing 12
#Bina fishes 10
#segments(7.5, 5, 6, 1, lty = 1, col = rgb(1, 0, 0, 0.5), lwd = graphlinewidth)


#Bina fishes 12
#segments(7.5, 5, 9, 1, lty = 1, col = rgb(1, 0, 0, 0.5), lwd = graphlinewidth)

#Initial node
points(5, 10, pch = 16, col = "black", cex = nodesize, xpd = TRUE)
points(5, 9, pch = 16, col = "black", cex = nodesize)

#P1 Nodes
points(2.5, 4, pch = 16, col = "black", cex = nodesize)
points(3.5, 6, pch = 16, col = "black", cex = nodesize) 

#P2 Terminal Nodes
points(1, 1, pch = 16, col = "black", cex = nodesize)
points(4, 1, pch = 16, col = "black", cex = nodesize)
points(6, 1, pch = 16, col = "black", cex = nodesize)
points(9, 1, pch = 16, col = "black", cex = nodesize)

text(-0.45, 11, expression(paste("Employer:")), cex = actionlabelsize, xpd = TRUE)
text(5, 11, expression(paste("Sets no-shirking effort, ", underline(e), ", termination probability, ", t)), cex = actionlabelsize, xpd = TRUE)
text(5, 10.55, expression(paste("and wage, ", w)), cex = actionlabelsize, xpd = TRUE)

text(0, 8.5, expression(paste("Capitalist sector")), cex = actionlabelsize, xpd = TRUE)
text(0, 8.2, expression(paste("worker's choice:")), cex = actionlabelsize, xpd = TRUE)


text(3.6, 8.5, expression(paste("Does not work")), cex = actionlabelsize)
text(3.6, 8.1, expression(paste(e == 0)), cex = actionlabelsize)


text(6.1, 8.5, expression(paste("Works")), cex = actionlabelsize)
text(6.1, 8.1, expression(paste(e == underline(e))), cex = actionlabelsize)

text(-0.575, 6.15, expression(paste("Chance:")), cex = actionlabelsize, xpd = TRUE)

text(2.2, 6.15-0.5, expression(paste("Worker is")), cex = actionlabelsize)
text(2.2, 5.7-0.5, expression(paste("terminated")), cex = actionlabelsize)
text(2.2, 5.25-0.5, expression(paste((t))), cex = actionlabelsize)

text(4.7, 6.15-0.5, expression(paste("Worker")), cex = actionlabelsize)
text(4.7, 5.7-0.5, expression(paste("keeps job")), cex = actionlabelsize)
text(4.7, 5.25-0.5, expression(paste( (1-t) )), cex = actionlabelsize)

text(-0.575, 3.55, expression(paste("Chance:")), cex = actionlabelsize, xpd = TRUE)

text(1, 3.55, expression(paste("Moves to")), cex = actionlabelsize)
text(1, 3.1, expression(paste("informal sector")), cex = actionlabelsize)


text(3.7, 3.55, expression(paste("Gets a")), cex = actionlabelsize)
text(3.7, 3.1, expression(paste("new job")), cex = actionlabelsize)

text(1.2, 2.7, expression(paste( (j) )), cex = actionlabelsize)
text(3.7, 2.7, expression(paste( (1-j) )), cex = actionlabelsize)


#Payoffs 
text(1, .6, expression(gamma^I- underline(u)), cex = actionlabelsize)
text(4, .6, expression(w - underline(u)), cex = actionlabelsize)
text(6, .6, expression(w), cex = actionlabelsize)
text(9, .6, expression(paste(w - underline(u))), cex = actionlabelsize)

dev.off()
