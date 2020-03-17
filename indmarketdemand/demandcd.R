library(shape)
pdf(file = "indmarketdemand/demandcd.pdf", width = 8, height = 8)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5


COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Margins
par(mar =  c(5, 5, 1, 1))

uA <- function(x, y, alpha = 0.5) {
  x^(alpha)*y^(1 - alpha)
}

bcA <- function(x, w = 20, p = 2) {
  w - p*x
}


xlims <- c(0, 13)
ylims <- c(0, 13)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(sqrt(2)*sqrt(6), 2*sqrt(6), 6,7,8)

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
ticksy <- c(0, 6, 12, ylims[2])
ylabels <- c(NA, 6, expression(paste(frac(m,p[y])==12 )), NA)
ticksx <- c(0, 2, 4, 6, 8, 12, xlims[2])
xlabels <- c(NA, 2, expression(paste(m/p[x1]) ==4), 6, expression(paste(m/p[x2])==8), expression(paste(m/p[x3])==12), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, bcA(xx1, w = 12, p = 1), col = COLA[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 12, p = 1.5), col = COLA[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 12, p = 3), col = COLA[3], lwd = graphlinewidth)
#lines(xx1, indiffA1(xx1, uA = 20, rmax = 2.5, xmax = 10), col = COLB[4], lwd = graphlinewidth)


#Label Axes
#mtext(expression(paste("Kilograms of coffee, ", x)), side=1, line = 2.5, cex = axislabelsize)

text(0.5*xlims[2], -1.3, expression(paste("Kilograms of coffee, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-1.2, 0.5*ylims[2], expression(paste("Gigabytes of data, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

segments(0, 6, 6, 6, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(2, 0, 2, 6, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(4, 0, 4, 6, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(6, 0, 6, 6, lty = 2, col = "gray" , lwd = segmentlinewidth)

#Label the budget curve functions for the HG, Aisha
text(3.5, 0.5, expression(bc[1]), cex = annotatesize)
text(7.2, 0.5, expression(bc[2]), cex = annotatesize)
text(11.1, 0.5, expression(bc[3]), cex = annotatesize)

#adding iso-welfare functions:

#Label the iso-welfare functions for the HG, Aisha
text(12.2, 1.2, expression(u[1]), cex = annotatesize)
text(12.2, 2.2, expression(u[2]), cex = annotatesize)
text(12.2, 3.15, expression(u[3]), cex = annotatesize)
text(12.2, 4.2, expression(u[4]), cex = annotatesize)
text(12.2, 5.5, expression(u[5]), cex = annotatesize)


#text(6.6, 8.3, expression(u[4]^A))

contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLB[3],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE)

abline(h=6, col=COL[3], lwd=graphlinewidth)


points(2, 6, pch = 16, col = "black", cex = 1.5)
text(2.2, 6.3, expression(a), cex = annotatesize)
points(4, 6, pch = 16, col = "black", cex = 1.5)
text(4.2, 6.3, expression(b), cex = annotatesize)
points(6, 6, pch = 16, col = "black", cex = 1.5)
text(6.2, 6.3, expression(c), cex = annotatesize)

#Arrows(8.5, 6, 6.5, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(9.7, 6.3, expression(paste("Price-offer curve")), cex = annotatesize)


dev.off()
