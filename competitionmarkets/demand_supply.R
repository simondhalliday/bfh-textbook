require(shape)
pdf(file = "competitionmarkets/demand_supply.pdf", width = 9, height = 7)

#Set parameters for graphics
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

par(mar =  c(5, 5, 1, 1))

Demand <- function(x, rmax = 20, xmax = 12, n = 10) {
  rmax - (rmax/(n*xmax))*x
}

mrsA <- function(x, rmax = 20, xmax = 10) {
  rmax - (rmax/xmax)*x
}

uA <- function(x, y, rmax = 20, xmax = 10) {
  y + rmax*x - (1/2)(rmax/xmax)*x^2
}


Supply <- function(x, c1 = 0.5, c2 = 0.0465){
  c1 + 2*c2*x
}

xlims <- c(0, 130)
ylims <- c(0, 22)

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
     xaxs = "i", 
     yaxs = "i")


ticksy <- c(0, 7.5, 20, ylims[2])
ylabels <- c(NA, expression(paste(p,"*")), expression(paste(bar(p))), NA)
ticksx <- c(0, 75, 120, xlims[2])
xlabels <- c(NA, expression(paste(X,"*")), expression(paste(bar(X))), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Draw the polygon for producer surplus
xpoly1 <- c(0, 75, 0, 0)
ypoly1 <- c(0.5, 7.5, 7.5, 0.5)
polygon(x = xpoly1, y = ypoly1, col = COLB[1], density = NULL, border = NA)

#Draw the polygon for consumer surplus
xpoly2 <- c(0, 75, 0, 0)
ypoly2 <- c(7.5, 7.5, 20, 7.5)
polygon(x = xpoly2, y = ypoly2, col = COLA[1], density = NULL, border = NA)


text(25, 6, expression(paste("Producer surplus")), xpd = TRUE, cex = labelsize)
text(25, 5, expression(paste("(Economic profit)")), xpd = TRUE, cex = labelsize) 
text(25, 11.5, expression(paste("Consumer surplus")), xpd = TRUE, cex = labelsize) 

#Lines for mrs graph
lines(xx1, Demand(xx1), col = COLA[4], lwd = graphlinewidth)
lines(xx1, Supply(xx1), col = COLB[4], lwd = graphlinewidth)

segments(0, 7.5, 75, 7.5, lty = 2, "gray" , lwd = segmentlinewidth)
segments(75, 0, 75, 7.5, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(75, 7.5, pch = 16, col = "black", cex = 1.5)

#Label axes
mtext(expression(paste("Market output of the good, ", X)), side = 1, line = 2.5, cex = axislabelsize)
text(-12, 0.5*ylims[2], expression(paste("Price per unit of X, ", p[X])), xpd = TRUE, cex = axislabelsize, srt = 90) 

text(103, 12, expression("Sellers' Supply"), cex = labelsize)
text(103, 6, expression(paste("Buyers' demand")), cex = labelsize)

dev.off()
