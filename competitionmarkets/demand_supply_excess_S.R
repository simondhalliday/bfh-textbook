require(shape)
pdf(file = "competitionmarkets/demand_supply_excess_S.pdf", width = 9, height = 7)

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
Grays <- gray.colors(25, start = 1, end = 0)

par(mar =  c(6, 10, 1, 3))

Demand <- function(x, rmax = 20, xmax = 12, n = 10) {
  rmax - (rmax/(n*xmax))*x
}

mrsA <- function(x, rmax = 20, xmax = 10) {
  rmax - (rmax/xmax)*x
}

uA <- function(x, y, rmax = 20, xmax = 10) {
  y + rmax*x - (1/2)(rmax/xmax)*x^2
}


#Supply <- function(x, c1 = 0, c2 = 0.05){
#  c1 + 2*c2*x
#}

# new supply curve
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
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
# ticksx <- seq(from = 0, to = xlims[2], by = 2)
# xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(0, Supply(x = 54), 7.5, 11, 20, ylims[2])
#ylabels <- c(NA,  expression(paste(mc(X^{DH}))), expression(paste(p,"*")), expression(paste(p^H)), expression(paste(bar(p) )), NA)
ylabels <- c(NA,  expression(paste(mc(X^{DH}) == 5.5)), expression(paste(p^c) == 7.5), expression(paste(p^H) == 11), expression(paste(bar(p)==20 )), NA)
ticksx <- c(0, 54, 75, 110, 120, xlims[2])
#xlabels <- c(NA, expression(paste(X^{DH})), expression(paste(X,"*")), expression(paste(X^{SH})), expression(paste(bar(p)/beta )), NA)
xlabels <- c(NA, expression(paste(X^{DH} == 54)), expression(paste(X^{c}) == 75), expression(paste(X^{SH} == 110)), expression(paste(bar(p)/beta ==120 )), NA)

axis(1, at = ticksx, pos = 0, labels = FALSE, cex.axis = labelsize)
text(x = c(0, 52, 75, 103, 127, xlims[2]), par("usr")[3] - 0.5, labels = xlabels, srt = 0, pos = 1, xpd = TRUE, cex = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], 75, length.out = npts)
xx2 <- seq(75, xlims[2], length.out = npts)

#Lines for mrs graph
lines(xx1, Demand(xx1), col = COLA[4], lwd = graphlinewidth)
lines(xx2, Demand(xx2), col = COLA[4], lty = 2, lwd = segmentlinewidth)
lines(xx1, Supply(xx1), col = COLB[4], lwd = graphlinewidth)
lines(xx2, Supply(xx2), col = COLB[4], lty = 2, lwd = segmentlinewidth)

#lines(xx1, mrsA(xx1, rmax = 20, xmax = 10), col = COLA[4], lwd = graphlinewidth)

# segments(0, 7.5, 75, 7.5, lty = 2, Grays[20] , lwd = segmentlinewidth)
# segments(75, 0, 75, 7.5, lty = 2, col = Grays[20] , lwd = segmentlinewidth)
points(75, 7.5, pch = 16, col = "black", cex = 1.5)
text(75, 8.25, expression(c), cex = labelsize)

segments(0, Demand(x = 54), xlims[2], Supply(x = 110), lty = 2, Grays[20] , lwd = segmentlinewidth)
segments(110, 0, 110, Supply(x = 110), lty = 2, col = Grays[20] , lwd = segmentlinewidth)
segments(54, 0, 54, Demand(x = 54), lty = 2, col = Grays[20] , lwd = segmentlinewidth)
segments(0, Supply(x = 54), 54, Supply(x = 54), lty = 2, col = Grays[20] , lwd = segmentlinewidth)
points(54, Demand(x = 54), pch = 16, col = "black", cex = 1.5)
points(54, Supply(x = 54), pch = 16, col = "black", cex = 1.5)
points(110, Supply(x = 110), pch = 16, col = "black", cex = 1.5)
text(54 - 2, Demand(x = 54) - 0.5, expression(a), cex = labelsize)
text(54 - 2, Supply(x = 54) + 0.5, expression(d), cex = labelsize)
text(110 + 2, Supply(x = 110) - 0.5, expression(b), cex = labelsize)

# segments(0, Supply(x = 60), 84, Demand(x = 84), lty = 2, Grays[20] , lwd = segmentlinewidth)
# segments(60, 0, 60, Supply(x = 60), lty = 2, col = Grays[20] , lwd = segmentlinewidth)
# segments(84, 0, 84, Demand(x = 84), lty = 2, col = Grays[20] , lwd = segmentlinewidth)
# points(60, Supply(x = 60), pch = 16, col = "black", cex = 1.5)
# points(84, Demand(x = 84), pch = 16, col = "black", cex = 1.5)
# text(84, Demand(x = 84) + 0.75, expression(d), cex = labelsize)
# text(60, Supply(x = 60) + 0.75, expression(c), cex = labelsize)


#Label axes
mtext(expression(paste("Market quantity of output, ", X)), side=1, line = 4, cex = axislabelsize)
text(-26, 0.6*ylims[2], expression(paste("Price per unit of x, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 
# 
# points(50, 10, pch = 16, col = "black", cex = 1.5)
# text(52, 10.5, expression(M))

#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
text(120, 13.75, expression("Sellers'"), cex = labelsize)
text(120, 12.75, expression("supply"), cex = labelsize)
#text(7.3, 2.5, expression("Curve"))

Arrows(56, 11.5, 108, 11.5, col = "black", lty = 1, code = 3, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(80, 12.5, expression(paste("Excess supply at ", p^H)), cex = labelsize)

Arrows(48, 6, 48, 10.25, col = "black", lty = 1, code = 3, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(25, 8.5, expression(paste("Sellers' rent at ", p^H)), cex = labelsize)


# Arrows(62, 5.5, 82, 5.5, col = "black", lty = 1, code = 3, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# text(72, 4.75, expression(paste("Excess")), cex = labelsize)
# text(72, 3.75, expression(paste("Demand")), cex = labelsize)
# text(72, 2.75, expression(paste("at ", p^L)), cex = labelsize)


#Label Demand
text(120, 3, expression(paste("Buyers'")), cex = labelsize)
text(120, 2, expression(paste("demand")), cex = labelsize)

dev.off()
