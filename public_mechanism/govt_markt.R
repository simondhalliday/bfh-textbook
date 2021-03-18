#' Graph Designer: Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)

pdf(file = "public_mechanism/govt_markt.pdf", width = 9, height = 3)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.5
labelsize <- 1.2
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442","#D55E00")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(0.2, 3, 0, 3))


#Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 50)
xlims <- c(0, 50)

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
     yaxs = "i"
)


# Axes --------------------------------------------------------------------


# ticksy <- c(NA)
# ylabels <- c(NA)
# ticksx <- c(NA)
# xlabels <- c(NA)
# 
# axis(1,at = ticksx,  pos = 0, labels = xlabels, cex.axis = labelsize)
# axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Axis labels and draw linear utility function
# text(-0.2, 0.5*ylims[2], expression(paste("y axis")), xpd = TRUE, cex = axislabelsize, srt = 90) 
# text(0.5*xlims[2], -0.15, expression(paste("x axis")), xpd = TRUE, cex = axislabelsize) 

# Arrows ------------------------------------------------------------------

# points(12, 32.5, cex = 1.5, pch = 16)
# points(38, 32.5, cex = 1.5, pch = 16)
# points(25, 10, cex = 1.5, pch = 16)

# Top
Arrows(13, 40, 37, 40, code = 3, lty = 1, lwd = graphlinewidth, arr.type = "triangle", col = CBCols[2])
# Left
#Arrows(12.5, 31.25, 24.5, 11.25, code = 3, lty = 1, lwd = graphlinewidth, arr.type = "triangle", col = CBCols[1])
# Right
#Arrows(25.5, 11.25, 37.5, 31.25, code = 3, lty = 1, lwd = graphlinewidth, arr.type = "triangle", col = CBCols[3])

# Text --------------------------------------------------------------------
text(7, 40, expression(bold("Government")), cex = axislabelsize)
text(4.85, 36, expression(italic("Compliance with state authority")), cex = labelsize, xpd = TRUE)
text(5.3, 33, expression(italic("implemented by fiat and elections")), cex = labelsize, xpd = TRUE)

text(41.5, 40, expression(bold("Markets")), cex = axislabelsize)
text(42.85, 36, expression(italic("Material incentives")), cex = labelsize, xpd = TRUE)
text(43, 33, expression(italic("implemented by prices")), cex = labelsize, xpd = TRUE)

# Annotations (CW) --------------------------------------------------------

# Top
text(16, 20, expression("Nationalization of railways"), cex = labelsize, xpd = TRUE)
text(16, 17, expression("energy distribution"), cex = labelsize, xpd = TRUE)
Arrows(16, 38, 16, 21, code = 1, lty = 1, lwd = graphlinewidth, arr.type = "triangle", arr.length = 0.2, col = "black")

text(25, 10, expression("Carbon tax and dividend"), cex = labelsize, xpd = TRUE)
Arrows(25, 38, 25, 11, code = 1, lty = 1, lwd = graphlinewidth, arr.type = "triangle", arr.length = 0.2, col = "black")


text(31, 16, expression("'Cap and trade'"), cex = labelsize, xpd = TRUE)
Arrows(31, 38, 31, 18, code = 1, lty = 1, lwd = graphlinewidth, arr.type = "triangle", arr.length = 0.2, col = "black")

text(40, 22, expression("Deregulation of product and"), cex = labelsize, xpd = TRUE)
text(40, 19, expression("labor markets"), cex = labelsize, xpd = TRUE)
Arrows(33.5, 38, 33.5, 24, code = 1, lty = 1, lwd = graphlinewidth, arr.type = "triangle", arr.length = 0.2, col = "black")


dev.off()