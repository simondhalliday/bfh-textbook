require(shape)
require(plotrix)
pdf(file = "capitalism/employment_unions_v2.pdf", width = 9, height = 7)

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
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)

CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")

WageFn <- function(h, ubar = 3, B = 2, t = 0.8) {
  B + ubar + (ubar*t*h )/(t*(1 - h))
}


WageFn#COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")
par(mar =  c(5, 6, 1.1, 1.1))
xlims <- c(0, 1)
ylims <- c(0, 50)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("Total hours of employment as a proportion, ", H)),
     ylab = expression(paste("Wage, ", w)),
     xaxt = "n", 
     yaxt = "n",
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs = "i", 
     yaxs = "i")

npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], 1, length.out = npts)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
xx4 <- seq(xlims[1], 25, length.out = npts2)

#Draw the lines for the graphs
lines(xx1, WageFn(xx1), col = CBCols[1], lwd = graphlinewidth)
#lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 4)
#lines(xx2, solowInfeas(xx2, delta = 5), col = COL[1], lwd = 4, lty = 2)

#Customize ticks and labels for the plot
# ticksy <- c(0, 2.5, 5, 20, 25,  40)
# ylabels <- c(0, expression(paste(B)), expression(paste(B+underline(u))), expression(paste(w[0])), expression(paste(w[2])), NA)

ticksy <- c(0, 20, 25, 40, ylims[2])
ylabels <- c(NA, expression(paste(w[0]^c)), expression(paste(w[2]^c)), expression(paste(gamma[0])), expression(paste(gamma[2])))

ticksx <- c(0, 0.832, 0.87, 1, xlims[2])
#xlabels <- c(0, expression(paste(H[0],"*")), expression(paste(H[2],"*")), 1.0, NA)
xlabels <- c(0, NA, NA, 1.0, NA)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#x-axis tick label 
text(0.883, -2.1, expression(paste(H[2]^N)), cex = labelsize,xpd = TRUE)
text(0.835, -2.1, expression(paste(H[0]^N)), cex = labelsize,xpd = TRUE)

#Annotation of the  graphs
text(0.74, 35, expression(paste("Wage curve, ", w^N*(H))), cex = labelsize)

#segments(1, 0, 1, 42, lty = 2, lwd = 3, col = "darkgray")
segments(0.832, 0, 0.832, 20, lty = 2, lwd = segmentlinewidth, col = grays[20], xpd = TRUE)


segments(0, 40, xlims[2], 40, lty = 2, lwd = segmentlinewidth, col = CBCols[3], xpd = TRUE)
segments(0, 50, xlims[2], 50, lty = 2, lwd = segmentlinewidth, col = CBCols[3], xpd = TRUE)


#Arrows(0.85, 15, 0.85, 24, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#Arrows(0.85, 15, 0.85, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#text(0.98, 14.5, expression(paste("Employment rent")))
#text(0.98, 12.5, expression(paste("increases with union")))

#Original Zero profit condition 
segments(0, 20, 0.832, 20, lty = 1, lwd = graphlinewidth, col = CBCols[2], xpd = TRUE)
segments(0.7925, 20, 1.2, 20, lty = 2, lwd = segmentlinewidth, col = CBCols[2], xpd = TRUE)

points(0.832, 20, pch = 16, col = "black", cex = 1.5)
text(0.815, 21.5, expression(paste(n[0])), cex = labelsize)


#Union Zero profit condition 
segments(0, 25, 0.87, 25, lty = 1, lwd = graphlinewidth, col = CBCols[2])
segments(0.837, 25, 1.2, 25, lty = 2, lwd = segmentlinewidth, col = CBCols[2], xpd = TRUE)


Arrows(0.2, 20.5, 0.2, 23.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle")


text(0.37, 45, expression(paste("Union raises productivity, ", gamma)), cex = labelsize)
Arrows(0.6, 41, 0.6, 48, col = "black", lty = 1, lwd = 2, arr.type = "triangle")

segments(0.87, 0, 0.87, 25, lty = 2, lwd = segmentlinewidth, col = grays[20])
points(0.87, 25, pch = 16, col = "black", cex = 1.5)
text(0.85, 26.5, expression(paste(n[2])), cex = labelsize)
text(0.2, 31.5, expression(paste("Competition")), cex = labelsize, xpd = TRUE)
text(0.2, 29.2, expression(paste("condition, ")), cex = labelsize, xpd = TRUE)
text(0.2, 26.7, expression(paste(w^c == w[2])), cex = labelsize, xpd = TRUE)



#Unemployment benefits & a
#segments(0, 5, 1.2, 5, lty = 2, lwd = 2, col = "darkgray")
#segments(0, 2.5, 1.2, 2.5, lty = 2, lwd = segmentlinewidth, col = grays[20])

#Zero profit condition
text(0.2, 18.3, expression(paste("Initial competition")), cex = labelsize, xpd = TRUE)
text(0.2, 16.3, expression(paste("condition")), cex = labelsize, xpd = TRUE)
text(0.2, 13.8, expression(paste(w^c == w[0])), cex = labelsize, xpd = TRUE)
#text(0.97, 6, expression(paste(B + a)))
#text(0.97, 3.5, expression(paste(B, " (unemployment benefits)")))
#text(1.08, 36, expression(paste("level of")))
#text(1.08, 34, expression(paste("employment, ", bar(H))))

dev.off()

