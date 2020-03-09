#Graph Designer: Simon Halliday & Madeleine Wettach '20
#Authors: Bowles, Foley and Halliday
#Title: Microeconomics: Competition, Conflict and Coordination

require(shape)
pdf(file = "competitionmarkets/cournot_brfs_social.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(5, 7, 2, 2))

socialW <- function(xa, s = 0.5, pmax = 20, c1 = 2, slope = 1) {
  (pmax - c1)/s - slope*xa
}

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

xANE <- function(pbar, beta = 2, c1 = 1){
  (pbar - c1)/(3*beta)
}

isoA <- function(xa, s = 0.5, pmax = 20, c1 = 2, piA = 72){
  -(c1*xa - pmax*xa + s*xa^2 + piA)/(s*xa)
}

isoB1 <- function(xa, s = 0.5, pmax = 20, c1 = 2, piB = 72){
  (-sqrt((c1 - pmax + s*xa)^2 - 4*piB*s) - c1 + pmax -s*xa)/(2*s)
}

isoB2 <- function(xa, s = 0.5, pmax = 20, c1 = 2, piB = 72){
  (sqrt((c1 - pmax + s*xa)^2 - 4*piB*s) - c1 + pmax -s*xa)/(2*s)
}


#Input into Wolfram Alpha: solve for x y = 1/b - (0.5*x)/(a*b) - u/(a*b*x)

#solve for y 72 = (p - s*y)*x - s*(x)^2 - c1*x 
#solve for y 72 = (p - s*x)*y - s*(y)^2 - c1*y

xlims <- c(0, 36)
ylims <- c(0, 36)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(72, 81)
b <- c(72, 81)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")


ticksy <- c(0, 9, 12, 18, 36, ylims[2])
ylabels <- c(NA, expression(paste(x^{B},"*")), expression(paste(x^{BN})), expression(paste( frac(bar(p) - c[1],2*beta) )), expression(paste( frac(bar(p) - c[1],beta) )), NA)
ticksx <- c(0, 9, 12, 18, 36, xlims[2])
xlabels <- c(NA, expression(paste(x^{A},"*")), expression(paste(x^{AN})), expression(paste(frac(bar(p) - c[1],2*beta))), expression(paste( frac(bar(p) - c[1],beta) )), NA)


axis(1, at = ticksx, pos = 0, labels = FALSE)
text(x = c(0, 9, 12, 18, 36, xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE, cex = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)


lines(xx1, brfA(xx1, s = 0.5, pmax = 20, c1 = 2), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfB(xx1, s = 0.5, pmax = 20, c1 = 2), col = COLB[4], lwd = graphlinewidth)
lines(xx1, socialW(xx1, s = 0.5, pmax = 20, c1 = 2), col = COL[2], lwd = graphlinewidth)



text(0.5*(xlims[2]), -5, expression(paste("A's output, ", x^A)), xpd = TRUE, cex = axislabelsize) 
text(-6, 0.5*ylims[2], expression(paste("B's output, ", x^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label Nash Equilibrium 
segments(0, 12, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(12, 0, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(12, 12, pch = 16, col = "black", cex = 1.5)
text(12.75, 12.75, expression(paste(n)), cex = labelsize)


segments(0, 18, 18, 18, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(18, 0, 18, 18, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(18, 18, pch = 16, col = "black", cex = 1.5)
text(18.75, 18.75, expression(paste(j)), cex = labelsize)

#B's brf
text(31, 11.5, expression(paste("B's best response")), cex = annotatesize)
text(31, 10, expression(paste("function")), cex = annotatesize)
Arrows(32, 9, 32, 2.9, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#A's brf
text(7, 34.5, expression(paste("A's best response")), cex = annotatesize)
text(7, 33, expression(paste("function")), cex = annotatesize)
Arrows(6, 32, 6, 25, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Arrow for owners and consumers
Arrows(23, 21, 23, 14, lty = 1, lwd = 2, arr.type = "triangle",  col = "black")
text(23, 23.5, expression(paste("Efficient output")), cex = labelsize)
text(23, 22, expression(paste("for consumers")), cex = labelsize)
# 
# Arrows(12, 8, 9.5, 5.5, lty = 1, lwd = 2, arr.type = "triangle",  col = "#f0027f")
# text(10.5, 5, expression(paste("Better")), cex = labelsize)
# text(10.5, 4, expression(paste("for")), cex = labelsize)
# text(10.5, 3, expression(paste("owners")), cex = labelsize)

#f0027f
#e41a1c
dev.off()
