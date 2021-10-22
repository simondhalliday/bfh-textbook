require(shape)
pdf(file = "random/bertrand_brfs_isoprofits.pdf", width = 9, height = 7)

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
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")
grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")

par(mar =  c(4, 8.5, 2, 0.1))

piA <- function(p1, p2, c1 = 15) {
  (p1 - c1)*(1/2)*(1 + p2 - p1)
}

piB <- function(p1, p2, c2 = 15) {
  (p2 - c2)*(1/2)*(1 + p1 - p2)
}

brfB <- function(p1, c2 = 15) {
  1/2*(1 + p1 + c2)
}

brfA <- function(p1, c1 = 15) {
  2*p1 - 1 - c1
}

NEa <- function(c1=15, c2 = 15) {
  1 + (2*c1 + c2)/3
}

NEb <- function(c1=15, c2 = 15) {
  1 + (c1 + 2*c2)/3
}

xlims <- c(0, 36.5)
ylims <- c(0, 36.5)

npts <- 501 



plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs = "i", 
     yaxs = "i")

ticksy <- c(0, 8, 16, ylims[2])
ylabelsnum <- c(NA, expression(paste(frac(1,2)*(1 + c[2]))), expression(paste(p[2]^N)), NA)
ticksx <- c(0, 8, 16, xlims[2])
xlabelsnum <- c(NA, NA, expression(paste(p[1]^N)), NA)

text(8, -2, expression(paste(frac(1,2)*(1 + c[1]))), cex = labelsize, xpd = TRUE)


npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)


lines(xx1, brfA(xx1), col = CBCols[1], lwd = graphlinewidth)
lines(xx1, brfB(xx1), col = CBCols[2], lwd = graphlinewidth)


x <- seq(14, xlims[2], length.out = npts)
y <- seq(14, ylims[2], length.out = npts) 
a <- c(piA(p1 = NEa(c1 = 15), p2 = NEb(c2 = 15)), piA(p1 = NEa(c1 = 15), p2 = NEb(c2 = 15)) + 5, piA(p1 = NEa(c1 = 15), p2 = NEb(c2 = 15)) + 15)
b <- c(piA(p1 = NEa(c1 = 15), p2 = NEb(c2 = 15)), piA(p1 = NEa(c1 = 15), p2 = NEb(c2 = 15)) + 5, piA(p1 = NEa(c1 = 15), p2 = NEb(c2 = 15)) + 15)

contour(x, y,
        outer(x, y, piA),
        drawlabels = FALSE,
        col = CBCols[1],
        lty = 2, 
        lwd = graphlinewidth-0.2,
        levels = a,
        xaxs = "i",
        yaxs = "i",
        add = TRUE)
# 
# contour(x, y,
#         outer(x, y, piB),
#         drawlabels = FALSE,
#         col = CBCols[2],
#         lty = 2, 
#         lwd = graphlinewidth-0.2,
#         levels = b,
#         xaxs = "i",
#         yaxs = "i",
#         add = TRUE)

text(0.5*xlims[2], -3.5, expression(paste("Price of iPhones, ", p[1])), xpd = TRUE, cex = axislabelsize) 
text(-7.3, 0.5*ylims[2], expression(paste("Price of Pixels, ", p[2])), xpd = TRUE, cex = axislabelsize, srt = 90) 
# 
# segments(14.25, 0, 14.25, 7.5, lty = 2, col = grays[20] , lwd = segmentlinewidth)
# points(14.25, 7.5, pch = 16, col = "black", cex = 1.5)
# 
# segments(6.60, 0, 6.60, 7.5, lty = 2, col = grays[20] , lwd = segmentlinewidth)
# points(12, 12, pch = 16, col = "black", cex = 1.5)
# 
segments(0, 16, 16, 16, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(16, 0, 16, 16, lty = 2, col = grays[20] , lwd = segmentlinewidth)
points(16, 16, pch = 16, col = "black", cex = 1.5)

# text(14.25 - 0.5, 7.5 - 1, expression(paste(a)), cex = annotatesize)
# text(12 - 0.5, 12 - 1, expression(paste(b)), cex = annotatesize)
# text(9.2 - 0.5, 17.7 - 1, expression(paste(c)), cex = annotatesize)


#labels for the isoprofits
text(19, 28, expression(paste(pi[3]^1)), cex = annotatesize)
text(17, 24, expression(paste(pi[2]^1)), cex = annotatesize)
text(14, 20, expression(paste(pi[1]^1)), cex = annotatesize)

axis(1, at = ticksx, pos = 0, labels = xlabelsnum, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabelsnum, las = 1, cex.axis = labelsize)

text(21, 16 - 1, expression(paste("Nash equilibrium")), xpd = TRUE, cex = annotatesize) 

text(30, 21, expression(paste("Pixel BRF")), xpd = TRUE, cex = annotatesize) 

text(22, 36, expression(paste("iPhone BRF")), xpd = TRUE, cex = annotatesize) 
text(10, 36, expression(paste("iPhone isoprofits")), xpd = TRUE, cex = annotatesize) 
text(10, 34, expression(paste(pi[3]^1 > pi[2]^1, phantom() > pi[1]^1)), xpd = TRUE, cex = annotatesize) 



dev.off()