library(pBrackets)
library(shape)

pdf(file = "indmarketdemand/elasticity.pdf", width = 9, height = 7)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(4, 4, 1, 1))

xlims <- c(0, 11)
ylims <- c(0, 11)

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

ticksx <- c(0, 10, xlims[2])
ticksy <- c(0, 10, ylims[2])
ylabels <- c(NA, expression(bar(p)), NA)
xlabels <- c(0, expression(bar(X)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Label axes
mtext(expression(paste("Quantity of the good, ", X)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.7, 0.5*ylims[2], expression(paste("Price per unit of the good, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 

# Demand
segments(0, 10, 10, 0, lty = 1, col = COLB[4] , lwd = graphlinewidth) 



#Label 
text(6.5, 6.6, expression(paste("Unit elastic")), cex = annotatesize)
text(6.5, 6, expression(paste(abs(eta), " = 1")), cex = annotatesize)
text(4, 8.76, expression(paste("Elastic")), cex = annotatesize)
text(4, 8.25, expression(paste(abs(eta), " > 1")), cex = annotatesize)
text(8.5, 4.1, expression(paste("Inelastic")), cex = annotatesize)
text(8.5, 3.5, expression(paste(abs(eta) , " < 1")), cex = annotatesize)

# Points
points(5, 5, pch = 16, col = "black", cex = 1.5)

# Braces
brackets(1, 9.5, 4.75, 5.75, h = NULL,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = segmentlinewidth, lty = 1, xpd = FALSE)
brackets(5.5, 5, 9.5, 1, h = NULL,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = segmentlinewidth, lty = 1, xpd = FALSE)

# Arrows
Arrows(6, 6, x1 = 5.35, y1 = 5.35,
       code = 2, lty = 1,
       lwd = segmentlinewidth,
       arr.type = "triangle", arr.lwd = 0.5)
dev.off()