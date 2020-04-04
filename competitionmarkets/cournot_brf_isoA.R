require(shape)
pdf(file = "competitionmarkets/cournot_brf_isoA.pdf", width = 9, height = 7)

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

par(mar =  c(4, 8, 2, 2))

piA <- function(xa, xb, s = 0.5, pmax = 20, c1 = 2) {
  (pmax - s*xb)*xa - s*(xa)^2 - c1*xa
}

piB <- function(xa, xb, s = 0.5, pmax = 20, c1 = 2) {
  (pmax - s*xa)*xb - s*(xb)^2 - c1*xb
}

brfB <- function(xa, s = 0.5, pmax = 20, c1 = 2) {
  (pmax - c1)/(2*s) - (1/2)*xa
}

brfA <- function(xa, s = 0.5, pmax = 20, c1 = 2) {
  (pmax - c1)/s - 2*xa
}

xlims <- c(0, 36.5)
ylims <- c(0, 36.5)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(42, 72, 102)
b <- c(72, 81, 90)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs = "i", 
     yaxs = "i")

ticksy <- c(0, 7.5, 12, 17.6, 36, ylims[2])
ylabelsnum <- c(NA, expression(paste(x[1]^B == 8)), expression(paste(x[2]^B == 12)), expression(paste(x[3]^B)==17.5), expression(paste(frac(bar(p) - c,beta) == 36), NA))
#ylabels <- c(NA, expression(paste(x[1]^B)), expression(paste(x[2]^B)), expression(paste(x[3]^B)), expression(paste(frac(bar(p) - c[1],beta))))
ticksx <- c(0, 9.2, 12, 14.25, 18, xlims[2])
#xlabels <- c(NA, expression(paste(x[1]^A)), expression(paste(x[2]^A)), expression(paste(x[3]^A)), NA, NA)
xlabelsnum <- c(NA, expression(paste(9)), expression(paste(12)), expression(paste(14)), expression(paste(18)), NA)

#xlabels that include labels and numerical values 
#text(7, -2.3, expression(paste(x[1]^A == 9)),xpd = TRUE, cex = labelsize - 0.05)
#text(10, -2.3, expression(paste(x[2]^A == 12)),xpd = TRUE, cex = labelsize - 0.05)
#text(15, -2.3, expression(paste(x[3]^A == 14)),xpd = TRUE, cex = labelsize - 0.05)
#text(20, -2.5, expression(paste(frac(bar(p) - c[1],2*beta) == 18)),xpd = TRUE, cex = labelsize - 0.05)

axis(1, at = ticksx, pos = 0, labels = xlabelsnum, cex.axis = labelsize)
#axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabelsnum, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, brfA(xx1, s = 0.5, pmax = 20, c1 = 2), col = COLA[4], lwd = graphlinewidth)

contour(x, y, 
        outer(x, y, piA),
        drawlabels = FALSE,
        col = COLA[2],
        lwd = graphlinewidth,
        levels = a, 
        xaxs = "i", 
        yaxs = "i", 
        add = TRUE) 

text(0.5*xlims[2], -3.5, expression(paste("A's output, ", x^A)), xpd = TRUE, cex = axislabelsize) 
text(-7.3, 0.5*ylims[2], expression(paste("B's output, ", x^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 


segments(0, 12, 14, 12, lty = 2, col = "darkgrey" , lwd = segmentlinewidth)
segments(12, 0, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(12, 12, pch = 16, col = "black", cex = 1.5)
segments(0, 7.5, 16.25, 7.5, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(14.25, 0, 14.25, 7.5, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(14.25, 7.5, pch = 16, col = "black", cex = 1.5)
segments(0, 17.7, 11.2, 17.7, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(9.2, 0, 9.2, 17.7, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(9.2, 17.7, pch = 16, col = "black", cex = 1.5)

text(24, 19.4, expression(paste("Iso-profit horizontal")), cex = annotatesize)
text(24, 18, expression(paste("at intersection with")), cex = annotatesize)
text(24, 16.4, expression(paste("best-response function")), cex = annotatesize)
Arrows(18, 18, 10.25, 18, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#A's brf
text(8.8, 30.4, expression(paste("A's best-response")), cex = annotatesize)
text(8.8, 29.2, expression(paste("function")), cex = annotatesize)
#Arrows(9, 28.2, 9, 23.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#labels for the isoprofits
text(18, 7.4, expression(paste(pi[3])), cex = annotatesize)
text(18, 10.8, expression(paste(pi[2])), cex = annotatesize)
text(18, 14.1, expression(paste(pi[1])), cex = annotatesize)



dev.off()
