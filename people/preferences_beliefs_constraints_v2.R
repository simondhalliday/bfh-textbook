require(shape)
library(plotrix)
pdf(file = "people/preferences_beliefs_constraints_v2.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.8
actionlabelsize <- 1.7
graphlinewidth <- 3
segmentlinewidth <- 2
fadelevel <- 0.3
elwidth = 4

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(0, 0, 0, 0))

xlims <- c(0, 10)
ylims <- c(1.5, 8.5)

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


draw.ellipse(1.5, 5, a = 1.4, b = 1.4, border = COL[1], angle=c(0), lty=1, lwd = elwidth)
text(1.5, 5.4, expression(paste("Constraints")), cex = actionlabelsize)
text(1.5, 5, expression(paste("(Feasible set")), cex = actionlabelsize)
text(1.5, 4.6, expression(paste("of actions)")), cex = actionlabelsize)
text(2, 8, expression(paste("Beliefs")), cex = labelsize, col = COLB[4])
text(2.5, 7.5, expression(paste("(which actions result")), cex = labelsize)
text(2.5, 7, expression(paste("in which outcomes)")), cex = labelsize)
#text(3.4, 7.5, expression(paste("Beliefs")), cex = labelsize, col = COLB[4])
#text(5, 7.5, expression(paste(phantom()==phantom(), "Feasible set")), cex = labelsize)

Arrows(3, 5, 3.4, 5, col = COLB[3], lty = 1, lwd = 3, arr.type = "triangle")

draw.ellipse(5, 5, a = 1.4, b = 1.4, border = COL[1], angle=c(0), lty=1, lwd = elwidth)
text(5, 5.4, expression(paste("Set of")), cex = actionlabelsize)
text(5, 5, expression(paste("outcomes to")), cex = actionlabelsize)
text(5, 4.6, expression(paste("be feasible")), cex = actionlabelsize)


Arrows(6.5, 5, 6.9, 5, col = COLB[3], lty = 1, lwd = 3, arr.type = "triangle")

draw.ellipse(8.5, 5, a = 1.4, b = 1.4, border = COL[1], angle=c(0), lty=1, lwd = elwidth)
text(8.5, 5.2, expression(paste("Choice of")), cex = actionlabelsize)
text(8.5, 4.8, expression(paste("an action")), cex = actionlabelsize)

#text(4.6, 2.9, expression(paste("Feasible set + ")), cex = labelsize)
text(8, 8, expression(paste("Preferences")), cex = labelsize, col = COLB[4])
#text(8.45, 2.9, expression(paste(phantom()==phantom(), "Ranking of")), cex = labelsize)
text(8.3, 7.5, expression(paste("(evaluation of")), cex = labelsize)
text(8.3, 7, expression(paste("outcomes)")), cex = labelsize)

dev.off()
