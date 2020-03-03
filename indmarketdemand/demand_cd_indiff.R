require(shape)
pdf(file = "indmarketdemand/demand_cd_indiff.pdf", width = 9, height = 7)

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

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 5, 1, 1))

uA <- function(x, y, alpha = 0.5) {
  x^(alpha)*y^(1 - alpha)
}

bcA <- function(x, w = 20, px = 2, py = 1) {
  w/py - (px/py)*x
}


xlims <- c(0, 13)
ylims <- c(0, 13)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(4, 6, 8)

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
ylabels <- c(NA, expression(paste(y,"*")), expression(paste(y == frac(m,p[y]) )), NA)
ticksx <- c(0, 6, 12, xlims[2])
xlabels <- c(NA, expression(paste(x,"*")), NA, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

text(12, -1, expression(paste(x == frac(w, p[x]) )), cex = labelsize, xpd = TRUE)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, bcA(xx1, w = 12, px = 1, py = 1), col = COLA[3], lwd = graphlinewidth)
#lines(xx1, indiffA1(xx1, uA = 20, rmax = 2.5, xmax = 10), col = COLB[4], lwd = graphlinewidth)



mtext(expression(paste("Kilograms of coffee, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-1.2, 0.5*ylims[2], expression(paste("Gigabytes of data, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Label the iso-welfare functions for the HG, Aisha
text(11.8, 1.6, expression(u[1]), cex = annotatesize)
text(11.8, 3.3, expression(u[2]), cex = annotatesize)
text(11.8, 5.7, expression(u[3]), cex = annotatesize)
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

segments(0, 6, 6, 6, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(6, 0, 6, 6, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(6, 6, pch = 16, col = "black", cex = 1.5)
text(6.1, 6.5, expression(i), cex = annotatesize)


points(1.55, 10.45, pch = 16, col = "black", cex = 1.5)
points(10.45, 1.55, pch = 16, col = "black", cex = 1.5)
text(1.65, 11, expression(a), cex = annotatesize)
text(10.55, 2, expression(b), cex = annotatesize)

text(9.2, 0.6, expression(paste("Budget constraint, ", bc[1])), cex = annotatesize)
#Arrows(10, 9.7, 10, 2.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


dev.off()
