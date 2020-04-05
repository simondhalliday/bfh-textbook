require(shape)
pdf(file = "competitionmarkets/cournot_brf_isoA_feasible.pdf", width = 9, height = 7)

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

par(mar =  c(4, 8.5, 2, 0.1))

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
ylabelsnum <- c(NA, expression(paste(x[1]^B == 7.5)), expression(paste(x[2]^B == 12)), expression(paste(x[3]^B)==17.5), expression(paste(frac(bar(p) - c,beta) == 36), NA))
ticksx <- c(0, 3.4, 6.6, 14.25, xlims[2])
xlabelsnum <- c(NA, expression(paste(3.4)), expression(paste(6.6)), expression(paste(14)), NA)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Feasible set
polygon(c(0, 0, xlims[2], xlims[2]), 
        c(ylims[2], 7.5 , 7.5, ylims[2]),
        border = FALSE, col = COLA[1])

# Feasible
segments(0, 7.5, xlims[2], 7.5, col = COLA[4], lwd = graphlinewidth)
text(5, 35, expression("Feasible set"), cex = annotatesize)
# polygon( feasible )

contour(x, y, 
        outer(x, y, piA),
        drawlabels = FALSE,
        col = COLB[3],
        lwd = graphlinewidth,
        levels = a, 
        xaxs = "i", 
        yaxs = "i", 
        add = TRUE) 

text(0.5*xlims[2], -3.5, expression(paste("A's output, ", x^A)), xpd = TRUE, cex = axislabelsize) 
text(-7.3, 0.5*ylims[2], expression(paste("B's output, ", x^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 

segments(6.60, 0, 6.60, 7.5, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(12, 12, pch = 16, col = "black", cex = 1.5)

segments(14.25, 0, 14.25, 7.5, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(14.25, 7.5, pch = 16, col = "black", cex = 1.5)

segments(3.4, 0, 3.4, 7.5, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(9.2, 17.7, pch = 16, col = "black", cex = 1.5)

#labels for the isoprofits
text(18, 5.9, expression(paste(pi[3])), cex = annotatesize)
text(18, 10.8, expression(paste(pi[2])), cex = annotatesize)
text(18, 14.2, expression(paste(pi[1])), cex = annotatesize)

axis(1, at = ticksx, pos = 0, labels = xlabelsnum, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabelsnum, las = 1, cex.axis = labelsize)

dev.off()