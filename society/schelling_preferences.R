require(shape)
library(plotrix)
pdf(file = "society/schelling_preferences_2.pdf", width = 6, height = 2)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.8
actionlabelsize <- 1.7
graphlinewidth <- 2
segmentlinewidth <- 1.5
fadelevel <- 0.3
elwidth <- 4
circlelettersize <- 4
circrad <- 1.15

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

SCol <- c("#377EB8", "#4DAF4A")
par(mar =  c(0.75, 0.75, 0.75, 0.75))

xlims <- c(0, 10)
ylims <- c(3, 5)

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


draw.circle(1.75, 4, radius = circrad, 
            nv = 100, border = NA, 
            col = SCol[1], lty = 1, density = NULL,
            angle = 45, lwd = elwidth)

text(1.75, 4, expression(paste("A")), 
     cex = circlelettersize, col = "black")

draw.circle(5, 4, radius = circrad, 
            nv = 100, border = NA, 
            col = SCol[2], lty = 1, density = NULL,
            angle = 45, lwd = elwidth)

text(5, 4, expression(paste("B")), 
     cex = circlelettersize, col = "black")

draw.circle(8.25, 4, radius = circrad, 
            nv = 100, border = NA, 
            col = SCol[2], lty = 1, density = NULL,
            angle = 45, lwd = elwidth)

text(8.25, 4, expression(paste("C")), 
     cex = circlelettersize, col = "black")


dev.off()

pdf(file = "society/schelling_preferences_1.pdf", width = 6, height = 2)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.8
actionlabelsize <- 1.7
graphlinewidth <- 2
segmentlinewidth <- 1.5
fadelevel <- 0.3
elwidth = 4

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

SCol <- c("#377EB8", "#4DAF4A")
par(mar =  c(0, 0, 0, 0))

xlims <- c(0, 10)
ylims <- c(3, 5)

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


draw.circle(1.75, 4, radius = circrad, 
            nv = 100, border = NA, 
            col = SCol[1], lty = 1, density = NULL,
            angle = 45, lwd = elwidth)

text(1.75, 4, expression(paste("A")), 
     cex = circlelettersize, col = "black")

draw.circle(5, 4, radius = circrad, 
            nv = 100, border = NA, 
            col = SCol[2], lty = 1, density = NULL,
            angle = 45, lwd = elwidth)

text(5, 4, expression(paste("B")), 
     cex = circlelettersize, col = "black")

draw.circle(8.25, 4, radius = circrad, 
            nv = 100, border = NA, 
            col = SCol[1], lty = 1, density = NULL,
            angle = 45, lwd = elwidth)

text(8.25, 4, expression(paste("C")), 
     cex = circlelettersize, col = "black")


dev.off()

pdf(file = "society/schelling_preferences_3.pdf", width = 6, height = 2)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.8
actionlabelsize <- 1.7
graphlinewidth <- 2
segmentlinewidth <- 1.5
fadelevel <- 0.3
elwidth = 4

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

SCol <- c("#377EB8", "#4DAF4A")
par(mar =  c(0, 0, 0, 0))

xlims <- c(0, 10)
ylims <- c(3, 5)

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


draw.circle(1.75, 4, radius = circrad, 
            nv = 100, border = NA, 
            col = SCol[2], lty = 1, density = NULL,
            angle = 45, lwd = elwidth)

text(1.75, 4, expression(paste("A")), 
     cex = circlelettersize, col = "black")

draw.circle(5, 4, radius = circrad, 
            nv = 100, border = NA, 
            col = SCol[2], lty = 1, density = NULL,
            angle = 45, lwd = elwidth)

text(5, 4, expression(paste("B")), 
     cex = circlelettersize, col = "black")

draw.circle(8.25, 4, radius = circrad, 
            nv = 100, border = NA, 
            col = SCol[2], lty = 1, density = NULL,
            angle = 45, lwd = elwidth)

text(8.25, 4, expression(paste("C")), 
     cex = circlelettersize, col = "black")


dev.off()
