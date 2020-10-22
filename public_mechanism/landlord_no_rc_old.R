require(shape)
pdf(file = "public_mechanism/landlord_no_rc.pdf", width = 9, height = 7)

# Set parameters for graphics
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

par(mar =  c(5, 5, 1, 1))

Demand <- function(x, rmax = 20, xmax = 12, n = 8) {
  rmax - (rmax/(n*xmax))*x
}


mrsA <- function(x, rmax = 20, xmax = 10) {
  rmax - (rmax/xmax)*x
}

#uA <- function(x, y, rmax, rmax = 20, xmax = 10) {
#  y + rmax*x - (1/2)(rmax/xmax)*x^2
#}


Supply <- function(x, c1 = 0.75, c2 = 4*0.0465){
  c1 + c2*x
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


ticksy <- c(0, 9.83, ylims[2])
ylabels <- c(NA, expression(paste(p[b])), NA)
ticksx <- c(0, 48.817, xlims[2])
xlabels <- c(NA, expression(paste(X[b])), NA)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Draw the polygon for landlord surplus
xpoly1 <- c(0, 28.226, 0, 0)
ypoly1 <- c(0.75, 6, 6, 0.75)
polygon(x = xpoly1, y = ypoly1, col=COLB[1], density=NULL, border = NA)

#Draw the polygon for renter surplus
xpoly2 <- c(0, 0, 28.226, 28.226)
ypoly2 <- c(6, 20, 14.1196, 6)
polygon(x = xpoly2, y = ypoly2, col=COLA[1], density=NULL, border = NA)

#Draw the polygon for renter loss
xpoly3 <- c(28.226, 28.226, 48.817)
ypoly3 <- c( 9.83, 14.1196, 9.83)
polygon(x = xpoly3, y = ypoly3, col=COLA[1], density=NULL, border = NA)

#Draw the polygon for LL loss
xpoly4 <- c(28.226, 28.226, 48.817)
ypoly4 <- c(6, 9.83, 9.83)
polygon(x = xpoly4, y = ypoly4, col=COLB[1], density=NULL, border = NA)

# CHANGE THIS COLOR

# Draw now renters surplus
xpoly5 <- c(0, 0, 28.226, 28.226)
ypoly5 <- c(9.83, 6, 6, 9.83)
polygon(x = xpoly5, y = ypoly5, col=COLB[1], density=NULL, border = NA)

#text(51, 5.5, expression(paste("Landlord Surplus Lost")), xpd = TRUE, cex = labelsize) 

#Segments
segments(0, 9.83, xlims[2], 9.83, lty = 2, grays[20] , lwd = segmentlinewidth)
segments(48.817, 0, 48.817, 9.83, lty = 2, col = grays[20] , lwd = segmentlinewidth)


# segments(0, 6, xlims[2], 6, lty = 2, grays[20] , lwd = segmentlinewidth)
# segments(28.226, 0, 28.226, 14.1196, lty = 2, grays[20] , lwd = segmentlinewidth)

#Lines for mrs graph
lines(xx1, Demand(xx1), col = COLA[4], lwd = graphlinewidth)
lines(xx1, Supply(xx1), col = COLB[4], lwd = graphlinewidth)

# To find the equilibrium X uncomment the following
# eq1 <- uniroot(function(x)  Demand(x) - Supply(x)  , c(.01,1000), tol=1e-8)   
# eq1[1]

# Points
points(48.817, 9.83, pch = 16, col = "black", cex = 1.5)
text(48.817, 10.75, expression(paste(b)), xpd = TRUE, cex = labelsize)

# segments(48.817 + 18.226, 0, 48.817 +18.226, 6, lty = 2, col = grays[20] , lwd = segmentlinewidth)


# segments(0, 6, xlims[2], 6, lty = 2, grays[20] , lwd = segmentlinewidth)
# segments(28.226, 0, 28.226, 14.1196, lty = 2, grays[20] , lwd = segmentlinewidth)

#Label axes
#mtext(expression(paste("Market output of housing, ", X)), side=1, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], -2.5, expression(paste("Market output of housing, ", X)), xpd = TRUE, cex = axislabelsize) 
text(-12, 0.5*ylims[2], expression(paste("Rental price per unit of housing, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 

# Labels
text(10.5, 6.5, expression(paste("Landlords'")), xpd = TRUE, cex = labelsize)
text(10.5, 5.5, expression(paste("economic")), xpd = TRUE, cex = labelsize)
text(10.5, 4.5, expression(paste("profit")), xpd = TRUE, cex = labelsize)
text(10.5, 14.16, expression(paste("Renters'")), xpd = TRUE, cex = labelsize) 
text(10.5, 13.16, expression(paste("surplus")), xpd = TRUE, cex = labelsize)

#text(100, 10.25, expression("Price, Before"), cex = labelsize)
#text(100, 5.5, expression(paste("Rent Control Price")), cex = labelsize)

text(100, 1.5, expression("Demand"), cex = labelsize)
text(100, 17.5, expression("Supply"), cex = labelsize)


axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

dev.off()
