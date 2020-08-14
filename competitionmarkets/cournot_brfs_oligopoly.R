## require(shape)
pdf(file = "competitionmarkets/cournot_brfs_oligopoly.pdf", width = 8, height = 8)

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
grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")


par(mar =  c(5, 5, 1, 1))

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

ProfitI <- function(n, s = 0.5, pmax = 20, c1 = 2) {
  (1/(n+1)^2)*(pmax - c1)^2/(s)
}


xlims <- c(0, 37)
ylims <- c(0, 37)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(ProfitI(n = 2), ProfitI(n = 2), ProfitI(n = 3), ProfitI(n = 4), ProfitI(n = 8))

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
ticksy <- c(0,  36, ylims[2])
ylabels <- c(NA,  expression(paste( frac(bar(p) - c,beta) )), NA)
ticksx <- c(0, 12, 18, xlims[2])
xlabels <- c(NA, expression(paste(x^{N})(n == 2)), expression(paste(frac(bar(p) - c,2*beta))),  NA)

axis(1, at = ticksx, pos = 0, labels = FALSE, cex.axis = labelsize)

text(x = c(0, 12, 18, 36, xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE, cex = labelsize)

axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, brfA(xx1, s = 0.5, pmax = 20, c1 = 2), col = CBCols[1], lwd = graphlinewidth)
## lines(xx1, brfB(xx1, s = 0.5, pmax = 20, c1 = 2), col = COLB[4], lwd = graphlinewidth)

## segments(0, 12, 12, 12, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(12, 0, 12, 12, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, 0, 36, 36, lty = 2, col = CBCols[2] , lwd = graphlinewidth)

segments(0, 0, 18, 36, lty = 2, col = CBCols[2], lwd = graphlinewidth)
segments(0, 0, 12, 36, lty = 2, col = CBCols[2] , lwd = graphlinewidth)
segments(0, 0, 36/7, 36, lty = 2, col = CBCols[2] , lwd = graphlinewidth)
## segments(0, 0, 7, 35, lty = 2, col = COLB[3] , lwd = graphlinewidth)
#mtext(expression(paste("A's output, ", x^A)), side=1, line = 2.5, cex = axislabelsize)
text(0.5*(xlims[2]), -4.2, expression(paste("The firm's output, ", x^i)), xpd = TRUE, cex = axislabelsize) 
text(-4.6, 0.5*ylims[2], expression(paste("Other firms' total outputs, ", X^{-i})), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Label point i. 
points(12, 12, pch = 16, col = "black", cex = 1.5)
text(13.8, 12, expression(paste(n == 2)), cex = labelsize)

points(9, 18, pch = 16, col = "black", cex = 1.5)
text(10.8, 18, expression(paste(n == 3)), cex = labelsize)

points(7.2, 21.6, pch = 16, col = "black", cex = 1.5)
text(9, 21.6, expression(paste(n == 4)), cex = labelsize)

points(4, 28, pch = 16, col = "black", cex = 1.5)
text(5.8, 28, expression(paste(n == 8)), cex = labelsize)


#A's brf
text(21.2, 4.3, expression(paste("The firm's")), cex = labelsize)
text(21.2, 3.1, expression(paste("best-response")), cex = labelsize)
text(21.2, 1.9, expression(paste("function")), cex = labelsize)
## text(6, 32.75, expression(paste("function")))
## Arrows(6, 32, 6, 26, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(34, 31, expression(paste(45*degree, " line")), cex = labelsize)
text(34, 29, expression(paste(x^i == X[{n==2}]^{-i})), cex = labelsize)
text(34, 27, expression(paste("slope", phantom()==1)), cex = labelsize)

text(17.5, 29, expression(paste(x^i == X[{n==3}]^{-i})), cex = labelsize)
text(17.5, 27, expression(paste("slope", phantom()==2)), cex = labelsize)

dev.off()
