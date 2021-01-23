require(shape)
pdf(file = "competitionmarkets/checkpoint_markupratio.pdf", width = 9, height = 7)

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
grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")

par(mar =  c(4, 4, 1, 1))

marketProfit <- function(n, s = 0.5, pmax = 20, c1 = 2) {
  n*(1/(n+1)^2)*(pmax - c1)^2/(s)
}

consumerSurplus <- function(n, s = 0.5, pmax = 20, c1 = 2) {
  (1/2)*(pmax - (c1 + ((1/(n+1))*(pmax -c1)  )) )*(n/(n+1))*( (pmax - c1)/(s))
}

markUp <- function(n, pmax = 20, c1 = 2) {
  c1 + (1/(n+1))*(pmax - c1)
}


xlims <- c(0, 44)
ylims <- c(0, 5)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(46.08, 55, 64)
b <- c(46.08, 55, 64)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
ticksx <- c(0,1,2,5,43, xlims[2])
xlabels <- c(NA,1,2,5,43, NA)
ticksy <- c(0, 4.5, 3, 1.5, 0.2, ylims[2])
ylabels <- c(NA,4.5, 3, 1.5, 0.2, NA)
# ticksx <- c(0, xlims[2])
# xlabels <- c(NA, NA)

axis(1, at = ticksx, pos = 0, labels = NA, cex.axis = labelsize)

text(x = c(0, 1, 2, 5, 43), par("usr")[3] - 0.1, labels = xlabels, srt = 0, pos = 1, xpd = TRUE, cex = labelsize)

axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

segments(1, 4.5, 2, 3, lty = 1, col = COLA[4] , lwd = segmentlinewidth)
segments(2, 3, 5, 1.5, lty = 1, col = COLA[4], lwd = segmentlinewidth)
segments(5, 1.5, 43, 0.2, lty = 1, col = COLA[4] , lwd = segmentlinewidth)

points(1, 4.5, pch = 16, col = "black", cex = 1.5)
points(2, 3, pch = 16, col = "black", cex = 1.5)
points(5, 1.5, pch = 16, col = "black", cex = 1.5)
points(43, 0.2, pch = 16, col = "black", cex = 1.5)

text(0.5*(xlims[2]), -0.5, expression(paste("Number of firms, ", n)), xpd = TRUE, cex = axislabelsize) 
text(-3, 0.5*ylims[2], expression(paste("Markup ratio")), xpd = TRUE, cex = axislabelsize, srt = 90) 


dev.off()
