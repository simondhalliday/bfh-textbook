#' Graph Designer: Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)

pdf(file = "public_mechanism/govt_markt_civ_general.pdf", width = 9, height = 6)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.5
labelsize <- 1.0
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(0, 1, 0, 1))


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
Arrows(15, 34.3, 39, 34.3, code = 3, lty = 1, lwd = graphlinewidth, arr.type = "triangle", arr.length = 0.3, col = CBCols[2])
# Left
Arrows(14.65, 33.35, 26.5, 13.35, code = 3, lty = 1, lwd = graphlinewidth, arr.type = "triangle", arr.length = 0.3, col = CBCols[1])
# Right
Arrows(27.4, 13.35, 39.35, 33.35, code = 3, lty = 1, lwd = graphlinewidth, arr.type = "triangle", arr.length = 0.3, col = CBCols[3])

# Text --------------------------------------------------------------------
text(9, 33.5, expression(bold("Government")), cex = namesize)
text(5.85, 31, expression(italic("Compliance with state authority")), cex = labelsize, xpd = TRUE)
text(6.3, 29.4, expression(italic("Implemented by fiat and elections")), cex = labelsize, xpd = TRUE)

text(43.5, 33.5, expression(bold("Markets")), cex = namesize)
text(44.85, 31, expression(italic("Material incentives")), cex = labelsize, xpd = TRUE)
text(45.7, 29.4, expression(italic("Implemented by prices")), cex = labelsize, xpd = TRUE)

text(27, 11, expression(bold("Civil Society")), cex = namesize)
text(27, 8.5, expression(italic("Reciprocity, altruism, fairness, sustainability, identity (including in-group)")), cex = labelsize, xpd = TRUE)
text(27, 7, expression(italic("Implemented by social norms and the exercise of private power")), cex = labelsize, xpd = TRUE)

# Annotations (CW) --------------------------------------------------------

# Top
text(0.35*xlims[2], 41, expression("Carbon tax and dividend"), cex = labelsize, xpd = TRUE)
Arrows(0.45*xlims[2], 35.5, 0.45*xlims[2], 39.5, code = 1, lty = 1, lwd = segmentlinewidth, arr.type = "triangle", arr.length = 0.15, col = "black")

text(0.65*xlims[2], 41, expression("'Cap and trade'"), cex = labelsize, xpd = TRUE)
Arrows(0.6*xlims[2], 35.5, 0.6*xlims[2], 39.5, code = 1, lty = 1, lwd = segmentlinewidth, arr.type = "triangle", arr.length = 0.15, col = "black")

# Right
#text(41, 25, expression("Relationships within"), cex = labelsize, xpd = TRUE)
text(42, 26, expression("Conventional firms"), cex = labelsize, xpd = TRUE)
Arrows(30, 23, 36.5, 26, code = 1, lty = 1, lwd = segmentlinewidth, arr.type = "triangle", arr.length = 0.15, col = "black")

text(42, 23, expression("Worker-owned co-op"), cex = labelsize, xpd = TRUE)
Arrows(30, 22, 35.5, 23, code = 1, lty = 1, lwd = segmentlinewidth, arr.type = "triangle", arr.length = 0.15, col = "black")

text(40, 19.5, expression("Open-sourced software"), cex = labelsize, xpd = TRUE)
Arrows(30, 20, 33, 19.5, code = 1, lty = 1, lwd = segmentlinewidth, arr.type = "triangle", arr.length = 0.15, col = "black")

text(38, 17, expression("Civil-society-led zero"), cex = labelsize, xpd = TRUE)
text(38, 15.4, expression("net carbon consumption"), cex = labelsize, xpd = TRUE)
Arrows(26, 20, 31.5, 17, code = 1, lty = 1, lwd = segmentlinewidth, arr.type = "triangle", arr.length = 0.15, col = "black")

# text(36, 13, expression("German healthcare system"), cex = labelsize, xpd = TRUE)
# Arrows(22, 25, 28, 13, code = 1, lty = 1, lwd = graphlinewidth, arr.type = "triangle", arr.length = 0.2, col = "black")

# Left

text(16, 14, expression("Care work at home"), cex = labelsize, xpd = TRUE)
Arrows(27, 16.5, 22, 14, code = 1, lty = 1, lwd = segmentlinewidth, arr.type = "triangle", arr.length = 0.15, col = "black")

text(14, 21.5, expression("Kidney exchanges"), cex = labelsize, xpd = TRUE)
text(14, 20, expression("(kidney sales prohibited)"), cex = labelsize, xpd = TRUE)
Arrows(24, 19, 20, 21.5, code = 1, lty = 1, lwd = segmentlinewidth, arr.type = "triangle", arr.length = 0.15, col = "black")

# text(8, 22, expression("NHS call for volunteers"), cex = labelsize, xpd = TRUE)
# Arrows(17.5, 22, 15, 22, code = 1, lty = 1, lwd = graphlinewidth, arr.type = "triangle", arr.length = 0.2, col = "black")


dev.off()