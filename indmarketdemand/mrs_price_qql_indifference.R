require(shape)
pdf(file = "indmarketdemand/mrs_price_qql_indifference.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 6, 1, 1))#, #mfrow = c(2,1))


# demandcd ----------------------------------------------------------------


uA <- function(x, y, alpha = 0.5) {
  x^(alpha)*y^(1 - alpha)
}

bcA <- function(x, w = 20, p = 2) {
  w - p*x
}


xlims <- c(0, 100)
ylims <- c(0, 600)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(sqrt(2)*sqrt(600), 2*sqrt(600), 600)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs = "i", 
     yaxs = "i")

ticksy <- c(0, 600, ylims[2])
ylabels <- c(NA, 6, expression(paste(frac(m,p[y])==600 )), NA)
ticksx <- c(0, 2, 4, 6, 8, 12, xlims[2])
xlabels <- c(NA, 2, 4, 6, 8, 12, NA)


npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, bcA(xx1, w = 600, p = 6), col = COLA[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 600, p = 10), col = COLA[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 600, p = 14), col = COLA[3], lwd = graphlinewidth)
#lines(xx1, indiffA1(xx1, uA = 20, rmax = 2.5, xmax = 10), col = COLB[4], lwd = graphlinewidth)


#Label Axes
#mtext(expression(paste("Kilograms of coffee, ", x)), side=1, line = 2.5, cex = axislabelsize)

#text(0.5*xlims[2], -2, expression(paste("Kilograms of coffee, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-2, 0.5*ylims[2], expression(paste("Money left over, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#segments(0, 6, 6, 6, lty = 2, col = "gray" , lwd = segmentlinewidth, xpd = TRUE)
segments(2, -10, 2, 6, lty = 2, col = "gray" , lwd = segmentlinewidth, xpd = TRUE)
segments(4, -10, 4, 6, lty = 2, col = "gray" , lwd = segmentlinewidth, xpd = TRUE)
segments(6, -10, 6, 6, lty = 2, col = "gray" , lwd = segmentlinewidth, xpd = TRUE)

#Label the budget curve functions for the HG, Aisha
text(3.5, 0.5, expression(bc[1]), cex = annotatesize)
text(7.2, 0.5, expression(bc[2]), cex = annotatesize)
text(11.1, 0.5, expression(bc[3]), cex = annotatesize)

#adding iso-welfare functions:

#Label the iso-welfare functions for the HG, Aisha
text(12.2, 1.2, expression(u[1]), cex = annotatesize)
text(12.2, 2.2, expression(u[2]), cex = annotatesize)
text(12.2, 3.15, expression(u[3]), cex = annotatesize)
#text(6.6, 8.3, expression(u[4]^A))

contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLB[3],
        lwd = graphlinewidth,
        levels = a, 
        xaxs = "i", 
        yaxs = "i", 
        add = TRUE)

abline(h=6, col=COL[3], lwd=graphlinewidth)


points(2, 6, pch = 16, col = "black", cex = 1.5)
text(2.2, 6.3, expression(a), cex = annotatesize)
points(4, 6, pch = 16, col = "black", cex = 1.5)
text(4.2, 6.3, expression(b), cex = annotatesize)
points(6, 6, pch = 16, col = "black", cex = 1.5)
text(6.2, 6.3, expression(c), cex = annotatesize)

#Arrows(8.5, 6, 6.5, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
#text(9.7, 6.3, expression(paste("Price-offer curve")), cex = annotatesize)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

