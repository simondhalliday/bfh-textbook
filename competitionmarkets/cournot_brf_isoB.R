require(shape)
pdf(file = "competitionmarkets/cournot_brf_isoB.pdf", width = 9, height = 7)

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

par(mar =  c(4, 6, 2, 0.1))

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

ticksy <- c(0, NA, NA, 18, NA, ylims[2])
ylabelsnum <- c(NA, NA, NA, 18, NA, NA)
#ylabels <- c(NA, expression(paste(x[1]^B)), expression(paste(x[2]^B)), expression(paste(x[3]^B)), expression(paste(frac(bar(p) - c[1],beta))))
ticksx <- c(0, NA, NA, NA, 36, xlims[2])
#xlabels <- c(NA, expression(paste(x[1]^A)), expression(paste(x[2]^A)), expression(paste(x[3]^A)), NA, NA)
xlabelsnum <- c(NA, NA, NA, NA, expression(paste(frac(bar(p) - c,beta) == 36)), NA)

#xlabels that include labels and numerical values 
#text(7, -2.3, expression(paste(x[1]^A == 9)),xpd = TRUE, cex = labelsize - 0.05)
#text(10, -2.3, expression(paste(x[2]^A == 12)),xpd = TRUE, cex = labelsize - 0.05)
#text(15, -2.3, expression(paste(x[3]^A == 14)),xpd = TRUE, cex = labelsize - 0.05)
#text(20, -2.5, expression(paste(frac(bar(p) - c[1],2*beta) == 18)),xpd = TRUE, cex = labelsize - 0.05)

axis(1, at = ticksx, pos = 0, labels = FALSE, cex.axis = labelsize)
#axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabelsnum, las = 1, cex.axis = labelsize)
text(x = c(0, 9.2, 12, 14.75, 34, xlims[2]), par("usr")[3] - 0.4, labels = xlabelsnum, srt = 0, pos = 1, xpd = TRUE, cex = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, brfB(xx1, s = 0.5, pmax = 20, c1 = 2), col = COLA[4], lwd = graphlinewidth)

contour(x, y, 
        outer(x, y, piB),
        drawlabels = FALSE,
        col = CBCols[1],
        lwd = graphlinewidth,
        levels = a, 
        xaxs = "i", 
        yaxs = "i", 
        add = TRUE) 

text(0.5*xlims[2], -3.5, expression(paste("A's output, ", x^A)), xpd = TRUE, cex = axislabelsize) 
text(-4.5, 0.5*ylims[2], expression(paste("B's output, ", x^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#B's brf
text(27, 10, expression(paste("B's best-response")), cex = annotatesize)
text(27, 8, expression(paste("function")), cex = annotatesize)
#Arrows(9, 28.2, 9, 23.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#labels for the isoprofits
text(5.5, 18, expression(paste(pi[3])), cex = annotatesize)
text(8.8, 18, expression(paste(pi[2])), cex = annotatesize)
text(12.2, 18, expression(paste(pi[1])), cex = annotatesize)

text(22, 21, expression(paste("B's isoprofit curves")), cex = annotatesize)
Arrows(17, 21, 12, 21, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

dev.off()
