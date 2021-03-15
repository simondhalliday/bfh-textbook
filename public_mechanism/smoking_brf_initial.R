require(shape)
library(pBrackets)

pdf(file = "public_mechanism/smoking_BRF_initial.pdf", width = 9, height = 7)

# Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)

par(mar =  c(5, 5, 1, 1)) # It's big to match paired fig

BRFXi <- function(Xj, P, alpha = 10, beta = 2){
  (alpha+beta*Xj)/P
}

BRFXj <- function(Xj, P, alpha = 10, beta = 2){
  (P*Xj-alpha)/beta
}

xlims <- c(0, 8)
ylims <- c(0, 8)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

plot(0, 0, 
     xlim = xlims, 
     ylim = ylims, 
     type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

ticksy <- c(0,  2.5, 5, ylims[2])
ylabels <- c(NA, NA, NA,  NA, NA)
ticksx <- c(0, 2.5, 5, xlims[2])
xlabels <- c(NA, NA, NA, NA, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#text(0.95, -0.49, expression(paste(frac(alpha,P[t]))), xpd = TRUE, cex = axislabelsize)
text(5, -0.45,  expression(paste(x[a]^{AN})), xpd=TRUE, cex=labelsize)
text(2.5, -0.49, expression(paste(frac(alpha, p[0]))), xpd=TRUE, cex=labelsize )
text(-0.3, 2.5, expression(paste(frac(alpha, p[0]))), xpd=TRUE, cex=labelsize )
# text(-0.3, 1.15, expression(paste(x[Bt]^N)), xpd = TRUE, cex = axislabelsize)
# text(-0.3, 2.1, expression(paste(x[Bt]^{direct})) , xpd=TRUE, cex=axislabelsize)
text(-0.3, 5, expression(paste(x[a]^{BN})), xpd=TRUE, cex=labelsize )
text(0.5*xlims[2], -1, expression(paste("A's smoking")), xpd = TRUE, cex = axislabelsize) 
text(-0.75, 0.5*ylims[2], expression(paste("B's smoking ")), xpd = TRUE, cex = axislabelsize, srt = 90) 

npts <- 500 
xx1 <- seq(0, xlims[2], length.out = npts)

lines(xx1, BRFXj(xx1,P=4), col = COLB[4], lwd = graphlinewidth)
lines(xx1, BRFXi(xx1,P=4), col = COLA[4], lwd = graphlinewidth)
#lines(xx1, BRFXj(xx1,P=10), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, BRFXi(xx1,P=10), col = COLA[4], lwd = graphlinewidth)

segments(5, 0, 5, 5, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, 5, 5, 5, lty = 2, col = grays[20] , lwd = segmentlinewidth)
points(5, 5, pch = 16, col = "black", cex = 1.5)
text(5.15, 4.9, expression(paste(a)), cex = labelsize)
text(4.25, 5.6, expression(paste("Pre-tax Nash")), cex = labelsize)
text(4.25, 5.2, expression(paste("equilibrium")), cex = labelsize)

# segments(1.25, 0, 1.25, 1.25, lty = 2, col = grays[20] , lwd = segmentlinewidth)
# segments(0, 1.25, 1.25, 1.25, lty = 2, col = grays[20] , lwd = segmentlinewidth)
# points(1.25, 1.25, pch = 16, col = "black", cex = 1.5)
# text(1.4, 1.1, expression(paste(b)), cex = labelsize)

#segments(0, 2, 5, 2, lty = 2, col = grays[20] , lwd = segmentlinewidth)

#text(1.5, 7.3, expression(paste("A's best response")), cex = labelsize)
#text(1.5, 7, expression(paste("after tax")), cex = labelsize)
text(5.1, 7.3, expression(paste("A's initial")), cex = labelsize)
text(5.1, 7, expression(paste("best response")), cex = labelsize)

text(7.1, 5.5, expression(paste("B's initial")), cex = labelsize, xpd = TRUE)
text(7.1, 5.1, expression(paste("best response")), cex = labelsize, xpd = TRUE)
#text(7, 2, expression(paste("B's best response")), cex = labelsize)
#text(7, 1.75, expression(paste("after tax")), cex = labelsize)

# brackets(-0.6, 1.25, -0.6, 2, h = .2,  ticks = 0.5, curvature = 0.5, type = 1,
#          col = 1, lwd = segmentlinewidth, lty = 1, xpd = TRUE)
# 
# brackets(-0.6, 2, -0.6, 5, h = .2,  ticks = 0.5, curvature = 0.5, type = 1,
#          col = 1, lwd =segmentlinewidth, lty = 1, xpd = TRUE)


# text(-1.15, 1.75, expression(paste("Indirect")), cex = labelsize, xpd = TRUE)
# text(-1.15, 1.5, expression(paste("effect")), cex = labelsize, xpd = TRUE)
# text(-1.15, 3.65, expression(paste("Direct")), cex = labelsize, xpd = TRUE)
# text(-1.15, 3.4, expression(paste("effect")), cex = labelsize, xpd = TRUE)

dev.off()
