#' Graph Designer: Simon Halliday & Scott Cohn
#' Authors: Bowles, Foley and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics


library(shape)
pdf(file = "public_mechanism/public_marginal.pdf", width = 8, height = 8)

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

par(mar =  c(5, 7, 1, 1))

MCost <- function(a, slope = 1, intercept = 0){
  intercept + slope*a
}


xlims <- c(0, 14)
ylims <- c(0, 14)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(12, 46.08, 90)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs = "i", 
     yaxs = "i")

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
# ticksx <- seq(from = 0, to = xlims[2], by = 2)
# xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(0, 2.5, 10, ylims[2])
ylabels <- c(NA, expression(paste(mpb)),  expression(paste(msb)), NA)
ticksx <- c(0, 2.5, 10, xlims[2])
xlabels <- c(NA, expression(paste(a^BN)), expression(paste(a^{BW})), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#mtext(expression(paste("Bridget's amount contributed, ", a^B)), side=1, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], -1.6, expression(paste("Bridget's amount contributed, ", a^B)), xpd = TRUE, cex = axislabelsize) 
text(-2.3, 0.5*ylims[2], expression(paste("Benefits and costs, $")), xpd = TRUE, cex = axislabelsize, srt = 90) 

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(4, 8, length.out = npts)

#lines(xx1, MBenefit(xx1, ua = 10), col = COLA[4], lwd = graphlinewidth)
lines(xx1, MCost(xx1), col = COLA[4], lwd = graphlinewidth)
#lines(xx1, indiffA(xx1, ua = 46.08), col = COLA[4], lwd = graphlinewidth)
#lines(xx2, mrsA(xx2), col = grays[20], lwd = graphlinewidth, lty = 2)


segments(0, 10, xlims[2], 10, lty = 1, col = COLB[3] , lwd = segmentlinewidth)
segments(0, 2.5, xlims[2], 2.5, lty = 1, col = COLB[3] , lwd = segmentlinewidth)

# segments(9.6, 0, 9.6, 9.6, lty = 2, col = grays[20] , lwd = segmentlinewidth)
# 
# segments(0, 6.9, 6.9, 6.9, lty = 2, col = grays[20] , lwd = segmentlinewidth)
# segments(6.9, 0, 6.9, 6.9, lty = 2, col = grays[20] , lwd = segmentlinewidth)


#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
# text(9.5, 30, expression(paste("Slope at ", (list(e^A, y^A)) == (list(6, 28)), " is ", e^A == 6)))

text(1.5, 11.7, expression(paste("Marginal")), cex = labelsize)
text(1.5, 11, expression(paste("social")), cex = labelsize)
text(1.5, 10.3, expression(paste("benefit")), cex = labelsize)
text(1.5, 4.2, expression(paste("Marginal")), cex = labelsize)
text(1.5, 3.5, expression(paste("private")), cex = labelsize)
text(1.5, 2.8, expression(paste("benefit")), cex = labelsize)
text(10, 13, expression(paste("Marginal cost", phantom() == a^B)), cex = labelsize)


text(12.5, 10.5, expression(paste(msb == n*gamma, phantom() == 5*gamma)), cex = labelsize, xpd = TRUE)
text(13, 3, expression(paste(mpb == gamma)), cex = labelsize)
#Arrows(1.5, 9, 1.5, 11.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
#text(3, 11, expression(paste(alpha, " increases, or ")), cex = labelsize)
#text(3, 10.5, expression(paste(h^B, " decreases")), cex = labelsize)

#Arrows(1.5, 7.5, 1.5, 4.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
#text(3, 5.5, expression(paste(alpha, " decreases, or ")), cex = labelsize)
#text(3, 5, expression(paste(h^B, " increases")), cex = labelsize)

segments(10, 0, 10, 10, lty = 2, col = grays[20] , lwd = segmentlinewidth)
points(10, 10, pch = 16, col = "black", cex = 1.5)
text(10.25, 9.5, expression(paste(g)), cex = labelsize)

segments(2.5, 0, 2.5, 2.5, lty = 2, col = grays[20] , lwd = segmentlinewidth)
points(2.5, 2.5, pch = 16, col = "black", cex = 1.5)
text(2.5 + 0.25, 2.5 - 0.5, expression(paste(e)), cex = labelsize)

dev.off()
