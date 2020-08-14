require(shape)
pdf(file = "indmarketdemand/budget_incomechange_offercurve_cd.pdf", width = 9, height = 7)

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
grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")


#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 5, 1, 1))

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
ylabels <- c(NA, expression(paste(frac(m[1],p[y]) )), expression(paste(frac(m[2],p[y]) )), expression(paste(frac(m[3],p[y]) )), NA)
ticksx <- c(0, 6, 9, 12, xlims[2])
xlabels <- c(NA, expression(paste(x == m[1]/p[x])), expression(paste(x == m[2]/p[x])), expression(paste(x == m[3]/p[x])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, bcA(xx1, w = 12, p = 1), col = CBCols[2], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 6, p = 1), col = CBCols[2], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 9, p = 1), col = CBCols[2], lwd = graphlinewidth)
#lines(xx1, indiffA1(xx1, uA = 20, rmax = 2.5, xmax = 10), col = COLB[4], lwd = graphlinewidth)
abline(0, 1, col=CBCols[3], lwd=graphlinewidth)

#Label Axes
#mtext(expression(paste("Kilograms of coffee, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-1.3, 0.5*ylims[2], expression(paste("Gigabytes of data, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(0.5*xlims[2], -1.5, expression(paste("Kilograms of coffee, ", x)), xpd = TRUE, cex = axislabelsize) 

#Label the budget curve functions for the HG, Aisha
text(5.2, .3, expression(bc[1]), cex = annotatesize)
text(8.2, .3, expression(bc[2]), cex = annotatesize)
text(11.2, .3, expression(bc[3]), cex = annotatesize)

text(11, 9.2, expression(paste("Income-offer")), cex = labelsize)
text(11, 8.4, expression(paste("curve")), cex = labelsize)

#adding iso-welfare functions:

#Label the iso-welfare functions for the HG, Aisha
text(1.1, 11.8, expression(u[1]), cex = annotatesize)
text(2.05, 11.8, expression(u[2]), cex = annotatesize)
text(3.4, 11.8, expression(u[3]), cex = annotatesize)
#text(6.6, 8.3, expression(u[4]^A))

contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = CBCols[1],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE)

points(3, 3, pch = 16, col = "black", cex = 1.5)
text(3, 3.5, expression(a), cex = annotatesize)
points(4.5, 4.5, pch = 16, col = "black", cex = 1.5)
text(4.5, 5, expression(b), cex = annotatesize)
points(6, 6, pch = 16, col = "black", cex = 1.5)
text(6, 6.5, expression(c), cex = annotatesize)


dev.off()
