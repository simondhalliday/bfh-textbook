require(shape)
pdf(file = "indmarketdemand/demand_cd_indiff_veblen.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 6, 1, 1))

uA <- function(x, y, alpha = 0.5) {
  x^(alpha)*y^(1 - alpha)
}

uA <- function(x, y, alpha = 0.5) {
  x^(alpha)*y^(1 - alpha)
}


bcA <- function(x, w = 1, h = 1) {
  h - w*x
}


xlims <- c(0, 5)
ylims <- c(0, 1.1)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(1)

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
ticksy <- c(0, 0.3, 0.5, 1, ylims[2])
ylabels <- c(NA, expression(paste(1 - h^v)), expression(paste(1 - h,"*")), expression(paste(l == 1)), NA)
ticksx <- c(0, 2, 2.8, 4, xlims[2])
xlabels <- c(NA, expression(paste(x,"*" == w*h,"*")), expression(paste(x^v == w*h^v)), expression(paste(x == w)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, bcA(xx1, w = 0.25, h = 1), col = COLB[3], lwd = graphlinewidth)
#lines(xx1, indiffA1(xx1, uA = 20, rmax = 2.5, xmax = 10), col = COLB[4], lwd = graphlinewidth)



text(0.5*xlims[2], -0.15, expression(paste("Consumption, ", x == w*h)), xpd = TRUE, cex = axislabelsize) 
#mtext(expression(paste("Consumption, ", x == w*h)), side=1, line = 2.5, cex = axislabelsize)
text(-0.66, 0.5*ylims[2], expression(paste("Leisure time as a proportion, ", l == 1 - h)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Label the iso-welfare functions for the HG, Aisha
text(4.6, 0.035, expression(u[1]^v), cex = annotatesize)
text(4.6, 0.13, expression(u[2]^v), cex = annotatesize)
text(4.6, 0.25, expression(paste(u,"*")), cex = annotatesize)

contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLA[3],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE)

b <- c(1.32, 1.44)

contour(x, y, 
        outer(x, y, uA, alpha = 0.7),
        drawlabels = FALSE,
        col = COLA[3],
        lwd = graphlinewidth,
        lty = 2, 
        levels = b, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE)

segments(0, 0.5, 2, 0.5, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(2, 0, 2, 0.5, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(2, 0.5, pch = 16, col = "black", cex = 1.5)
text(1.9, 0.48, expression(a), cex = annotatesize)

segments(0, 0.3, 2.8, 0.3, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(2.8, 0, 2.8, 0.3, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(2.8, 0.3, pch = 16, col = "black", cex = 1.5)
text(2.9, 0.32, expression(b), cex = annotatesize)


points(1.55, 10.45, pch = 16, col = "black", cex = 1.5)
points(10.45, 1.55, pch = 16, col = "black", cex = 1.5)

text(10, 10, expression(paste("Budget constraint, ", bc[1])))
Arrows(10, 9.7, 10, 2.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


dev.off()
