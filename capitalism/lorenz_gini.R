#' Graph Designer: Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)

pdf(file = "capitalism/lorenz_gini.pdf", width = 8, height = 8)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.7
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 7, 0.5, 0.5))

#Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 1.1)
xlims <- c(0, 1.1)

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

ticksy <- c(ylims[1], 0.1, 0.3, 0.5, 0.8, 1)
ylabels <- c(NA, expression(sigma[u]),  NA, expression(paste(sigma[u] + sigma[n])), NA,  1)
ticksx <- c(xlims[1], 0.3, 0.8, 1)
xlabels <- c(0, expression(u), expression(u + n), 1)


axis(1,at = ticksx,  pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xpoly1 <- c(0, 1, 1, 0.8, 0.3, 0)
ypoly1 <- c(0, 0, 1, 0.5, 0.1, 0)
polygon(x = xpoly1, y = ypoly1, col = COLA[1], density = NULL, border = NA)

xpoly2 <- c(0, 1, 0.8, 0.3, 0)
ypoly2 <- c(0, 1, 0.5, 0.1, 0)
polygon(x = xpoly2, y = ypoly2, col = COLB[1], density = NULL, border = NA)



#Axis labels and draw linear utility function
text(-0.2, 0.5*ylims[2], expression(paste("Cumulative income, %")) , xpd = TRUE, cex = axislabelsize, srt = 90) 
text(0.45*xlims[2], -0.15, expression(paste("Cumulative population, (%)")), xpd = TRUE, cex = axislabelsize) 

# Segments
segments(0, 1, 1, 1, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(1, 0, 1, 1, lty = 2, col = grays[20], lwd = segmentlinewidth)

#segments(0, 0.8, 0.8, 0.8, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0.8, 0, 0.8, 0.8, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, 0.5, 0.8, 0.5, lty = 2, col = grays[20], lwd = segmentlinewidth)

#segments(0, 0.3, 0.3, 0.3, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0.3, 0, 0.3, 0.3, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, 0.1, 0.3, 0.1, lty = 2, col = grays[20], lwd = segmentlinewidth)

#Line from B to C
segments(0.3, 0.3, 0.8, 0.5, lty = 2, col = grays[20], lwd = segmentlinewidth)


# Lorenz curve
segments(0, 0, 1, 1, lty = 1, col = COLB[4], lwd = graphlinewidth)
segments(0, 0, 0.3, 0.1, lty = 1, col = COLA[4], lwd = graphlinewidth)
segments(0.3, 0.1, 0.8, 0.5, lty = 1, col = COLA[4], lwd = graphlinewidth)
segments(0.8, 0.5, 1, 1, lty = 1, col = COLA[4], lwd = graphlinewidth)

#Segment to B
segments(0, 0.3, 0.3, 0.3, lty = 2, col = grays[20], lwd = segmentlinewidth)

#Segment to D
segments(0, 0.8, 0.8, 0.8, lty = 2, col = grays[20], lwd = segmentlinewidth)


# Points + labels

points(0.3, 0.1, pch = 16, col = "black", cex = 1.5)
text(0.325, 0.075, expression(A), cex = labelsize)

points(0.3, 0.3, pch = 16, col = "black", cex = 1.5)
text(0.275, 0.325, expression(B), cex = labelsize)

points(0.8, 0.5, pch = 16, col = "black", cex = 1.5)
text(0.825, 0.475, expression(C), cex = labelsize)

points(0.8, 0.8, pch = 16, col = "black", cex = 1.5)
text(0.775, 0.825, expression(D), cex = labelsize)

points(1, 1, pch = 16, col = "black", cex = 1.5)
text(1.025, 1.025, expression(E), cex = labelsize)



dev.off()
