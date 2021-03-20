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

par(mar =  c(5, 7, 1, 1))

# Demand <- function(x, rmax = 20, xmax = 12, n = 8) {
#   rmax - (rmax/(n*xmax))*x
# }

mrsA <- function(x, rmax = 20, xmax = 10) {
  rmax - (rmax/xmax)*x
}

#uA <- function(x, y, rmax, rmax = 20, xmax = 10) {
#  y + rmax*x - (1/2)(rmax/xmax)*x^2
#}


# Supply <- function(x, c1 = 0.75, c2 = 0.0465){
#   c1 + 4*c2*x
# }

Demand <- function(x, c2 = 3667, m = 10/3) {
  c2 - m*x
}

Supply <- function(x, c1 = 334, m = 10/3){
  c1 + m*x
}

# xlims <- c(0, 130)
# ylims <- c(0, 22)

xlims <- c(0, 1250)
ylims <- c(0, 4000)

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


ticksy <- c(0, 334, 2000, 3667, ylims[2])
ylabels <- c(NA, 334, "2,000", "3,667", NA)
ticksx <- c(0, 500, 1100.1, xlims[2])
xlabels <- c(NA, expression(paste(X[B]) == 500), "1,100.1", NA)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Draw the polygon for landlord surplus
xpoly1 <- c(0, 500, 0)
ypoly1 <- c(2000, 2000, 3667)
polygon(x = xpoly1, y = ypoly1, col=COLA[1], density=NULL, border = NA)


#Draw the polygon for renter loss
# xpoly3 <- c(28.226, 28.226, 48.817)
# ypoly3 <- c( 9.83, 14.1196, 9.83)
# polygon(x = xpoly3, y = ypoly3, col=COLA[1], density=NULL, border = NA)

#Draw the polygon for LL loss
# xpoly4 <- c(28.226, 28.226, 48.817)
# ypoly4 <- c(6, 9.83, 9.83)
# polygon(x = xpoly4, y = ypoly4, col=COLB[1], density=NULL, border = NA)


# Draw now renters surplus
xpoly5 <- c(0, 0, 500)
ypoly5 <- c(2000, 334, 2000)
polygon(x = xpoly5, y = ypoly5, col=COLB[1], density=NULL, border = NA)

#text(51, 5.5, expression(paste("Landlord Surplus Lost")), xpd = TRUE, cex = labelsize) 

#Segments
segments(0, 2000, xlims[2], 2000, lty = 2, grays[20] , lwd = segmentlinewidth)
segments(500, 0, 500, 2000, lty = 2, col = grays[20] , lwd = segmentlinewidth)
#segments(0, 1500, xlims[2], 1500, lty = 2, grays[20] , lwd = segmentlinewidth)

# segments(0, 6, xlims[2], 6, lty = 2, grays[20] , lwd = segmentlinewidth)
# segments(28.226, 0, 28.226, 14.1196, lty = 2, grays[20] , lwd = segmentlinewidth)

#Lines for mrs graph
lines(xx1, Demand(xx1), col = COLA[4], lwd = graphlinewidth)
lines(xx1, Supply(xx1), col = COLB[4], lwd = graphlinewidth)

# Points
points(500, 2000, pch = 16, col = "black", cex = 1.5)
text(500, 2200, expression(paste(B)), xpd = TRUE, cex = labelsize)


# text(1050, 2100, expression("Price before control"), cex = labelsize)
# text(1050, 1400, expression(paste("Rent control price")), cex = labelsize)


#Label axes
mtext(expression(paste("Number of housing units, ", X, " (thousands)")), side=1, line = 2.5, cex = axislabelsize)
text(-180, 0.5*ylims[2], expression(paste("Rental price, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 

# Labels
text(125, 1650, expression(paste("Landlords'")), xpd = TRUE, cex = labelsize)
text(125, 1450, expression(paste("economic")), xpd = TRUE, cex = labelsize)
text(125, 1250, expression(paste("rent")), xpd = TRUE, cex = labelsize)

text(125, 2600, expression(paste("Renters'")), xpd = TRUE, cex = labelsize) 
text(125, 2400, expression(paste("surplus")), xpd = TRUE, cex = labelsize)

#text(100, 10.25, expression("Price, Before"), cex = labelsize)
#text(100, 5.5, expression(paste("Rent Control Price")), cex = labelsize)
text(1025, 2100, expression(paste("Price before control, ", p[B])), cex = labelsize)


text(1150, 200, expression("Demand"), cex = labelsize)
text(1150, 3800, expression("Supply"), cex = labelsize)


axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

dev.off()
