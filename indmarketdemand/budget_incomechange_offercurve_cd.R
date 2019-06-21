require(shape)
pdf(file = "budget_incomechange_offercurve_cd.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2


COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 7, 4, 4))

uA <- function(x, y, alpha = 0.5){
  (x^alpha)*(y^(1-alpha))
}

bcA <- function(x, w = 20, p = 2) {
  w - p*x
}


xlims <- c(0, 13)
ylims <- c(0, 13)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(3, 4.5, 6)

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
ticksy <- c(0, 6, 9, 12, ylims[2])
ylabels <- c(NA, expression(paste(y==m[1])), expression(paste(y == m[2])), expression(paste(y == m[3])), NA)
ticksx <- c(0, 6, 9, 12, xlims[2])
xlabels <- c(NA, expression(paste(x == m[1]/p[x])), expression(paste(x == m[2]/p[x])), expression(paste(x == m[3]/p[x])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, bcA(xx1, w = 12, p = 1), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 6, p = 1), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 9, p = 1), col = COLB[3], lwd = graphlinewidth)
#lines(xx1, indiffA1(xx1, uA = 20, rmax = 2.5, xmax = 10), col = COLB[4], lwd = graphlinewidth)
abline(0, 1, col=COL[3], lwd=graphlinewidth)

#Label Axes
mtext(expression(paste("Kilograms of coffee, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-2.3, 0.5*ylims[2], expression(paste("Gigabytes of data, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the budget curve functions for the HG, Aisha
text(6.6, .3, expression(bc[m1]), cex = labelsize)
text(9.6, .3, expression(bc[m2]), cex = labelsize)
text(12.6, .3, expression(bc[m3]), cex = labelsize)

text(11, 9, expression(paste("Income offer curve")), cex = labelsize)


#adding iso-welfare functions:

#Label the iso-welfare functions for the HG, Aisha
# text(11.8, 1.6, expression(u[1]))
# text(11.8, 3.3, expression(u[2]))
# text(11.8, 5.7, expression(u[3]))
#text(6.6, 8.3, expression(u[4]^A))

contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLA[3],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE)

points(3, 3, pch = 16, col = "black", cex = 1.5)
text(3.2, 3.5, expression(a))
points(4.5, 4.5, pch = 16, col = "black", cex = 1.5)
text(4.7, 5, expression(b))
points(6, 6, pch = 16, col = "black", cex = 1.5)
text(6.2, 6.5, expression(c))


dev.off()
