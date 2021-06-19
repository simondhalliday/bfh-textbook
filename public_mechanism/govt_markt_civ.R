#' Graph Designer: Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)

pdf(file = "public_mechanism/govt_markt_civ.pdf", width = 9, height = 6)

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
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442","#D55E00")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(0, 2, 0, 2))


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
Arrows(13, 37.2, 37, 37.2, code = 3, lty = 1, lwd = graphlinewidth, arr.type = "", col = CBCols[2])
# Left
Arrows(13, 37.25, 25.5, 11.25, code = 3, lty = 1, lwd = graphlinewidth, arr.type = "", col = CBCols[1])
# Right
Arrows(25.5, 11.25, 37, 37.25, code = 3, lty = 1, lwd = graphlinewidth, arr.type = "", col = CBCols[3])

# Text --------------------------------------------------------------------
text(7.5, 37.5, expression(bold("Government")), cex = namesize)
text(5.85, 35.5, expression(italic("Compliance with state authority")), cex = labelsize, xpd = TRUE)
text(6.3, 34, expression(italic("Implemented by fiat and elections")), cex = labelsize, xpd = TRUE)

text(40, 37.5, expression(bold("Markets")), cex = axislabelsize)
text(43.8, 35.5, expression(italic("Material incentives implemented")), cex = labelsize, xpd = TRUE)
text(43, 34.3, expression(italic("by prices and competition")), cex = labelsize, xpd = TRUE)
text(43, 33, expression(italic("with complete contracts")), cex = labelsize, xpd = TRUE)

text(26, 9.5, expression(bold("Civil Society")), cex = namesize)
text(27, 7.5, expression(italic("Reciprocity, altruism, fairness, sustainability, identity (including in-group)")), cex = labelsize, xpd = TRUE)
text(27, 6.2, expression(italic("Implemented by social norms and the exercise of private power")), cex = labelsize, xpd = TRUE)

# Annotations (CW) --------------------------------------------------------

# Top
text(8, 41, expression("Mandatory risk-sharing (taxes and transfers)"), cex = labelsize, xpd = TRUE)

Arrows(14.5, 36, 13, 40, code = 1, lty = 1, lwd = graphlinewidth, arr.type = "triangle", arr.length = 0.2, col = "black")

#text(20, 43, expression("Free childcare"), cex = labelsize, xpd = TRUE)
Arrows(17, 37, 20, 44, code = 1, lty = 1, lwd = graphlinewidth, arr.type = "triangle", arr.length = 0.2, col = "black")

text(20, 47, expression("Reallocation of labor - 20k Qantas workers"), cex = labelsize, xpd = TRUE)
text(20, 45, expression("hired by government as contact tracers"), cex = labelsize, xpd = TRUE)
#Arrows(25, 33.25, 30, 35.5, code = 1, lty = 1, lwd = graphlinewidth, arr.type = "triangle", arr.length = 0.2, col = "black")

text(35, 42, expression("Monetary rewards for getting vaccinated"), cex = labelsize, xpd = TRUE)
Arrows(29, 36, 34, 41, code = 1, lty = 1, lwd = graphlinewidth, arr.type = "triangle", arr.length = 0.2, col = "black")

# Right
text(40, 29, expression("Reallocation of labor -"), cex = labelsize, xpd = TRUE)
text(40, 27.5, expression("Amazon hires 100k"), cex = labelsize, xpd = TRUE)
Arrows(35, 35, 37, 30, code = 1, lty = 1, lwd = graphlinewidth, arr.type = "triangle", arr.length = 0.2, col = "black")

text(41.5, 23, expression("Fast-track approval for private sector"), cex = labelsize, xpd = TRUE)
text(41.5, 21.7, expression("developed virus tests"), cex = labelsize, xpd = TRUE)
Arrows(25, 32, 32.5, 24, code = 1, lty = 1, lwd = graphlinewidth, arr.type = "triangle", arr.length = 0.2, col = "black")

text(38, 19, expression("Research, production and"), cex = labelsize, xpd = TRUE)
text(38, 17.8, expression("distribution of vaccine"), cex = labelsize, xpd = TRUE)
Arrows(24, 30, 33, 20, code = 1, lty = 1, lwd = graphlinewidth, arr.type = "triangle", arr.length = 0.2, col = "black")

text(34.5, 14, expression("German healthcare system"), cex = labelsize, xpd = TRUE)
Arrows(22, 29, 29, 15, code = 1, lty = 1, lwd = graphlinewidth, arr.type = "triangle", arr.length = 0.2, col = "black")

# Left

text(13, 13, expression("Social distancing and wearing masks"), cex = labelsize, xpd = TRUE)
Arrows(25, 14, 22, 13, code = 1, lty = 1, lwd = graphlinewidth, arr.type = "triangle", arr.length = 0.2, col = "black")

# text(15, 15.5, expression("Care work at home"), cex = labelsize, xpd = TRUE)
# Arrows(24, 16, 21, 15.5, code = 1, lty = 1, lwd = graphlinewidth, arr.type = "triangle", arr.length = 0.2, col = "black")

# text(12, 19.5, expression("Kidney exchanges"), cex = labelsize, xpd = TRUE)
# text(12, 18, expression("(kidney sales prohibited)"), cex = labelsize, xpd = TRUE)
# Arrows(22, 17, 18, 19.5, code = 1, lty = 1, lwd = graphlinewidth, arr.type = "triangle", arr.length = 0.2, col = "black")

text(9.5, 25, expression("NHS call for volunteers"), cex = labelsize, xpd = TRUE)
Arrows(19.2, 25, 16, 25, code = 1, lty = 1, lwd = graphlinewidth, arr.type = "triangle", arr.length = 0.2, col = "black")

text(8, 18, expression("Virus-testing and tracing in South Korea"), cex = labelsize, xpd = TRUE)
Arrows(22.3, 18, 17.5, 18, code = 1, lty = 1, lwd = graphlinewidth, arr.type = "triangle", arr.length = 0.2, col = "black")




dev.off()