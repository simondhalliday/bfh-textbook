#' Graph Designer(s): Simon Halliday, Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(pBrackets)
pdf(file = "risk/risk_averse_dara_a.pdf", width = 10, height = 8)

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

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 7, 1, 1))

#Concave utility of wealth function

ConcaveU <- function(x) {
  log(x + 1)
}

#Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 4)
xlims <- c(0, 40)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs = "i", 
     yaxs = "i"
)

#x and y limits with plain axes without ticks/numbers to match previous graph

ticksy <- c(0, ConcaveU(2), 1.6275*ConcaveU(3), ConcaveU(10), 2*ConcaveU(3), ConcaveU(22), ConcaveU(30), ylims[2])
ylabels <- c(NA, expression(paste(u(y^{d^B}))), NA, NA, expression(v(L)[c]), expression(paste(u(y^c^G))), expression(paste(u(y^d^G))), NA)

# ticksx <- c(0, 3, 10, 22, 36, xlims[2])
ticksx <- c(0, 16, xlims[2])
# xlabels <- c(NA, expression(paste(y^{a})), expression(paste(y^b)), expression(paste(y^c)), expression(paste(y^d)), NA)
xlabels <- c(NA, NA, NA)

axis(1, at = ticksx,  pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy,  pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Axis labels and draw linear utility function
#mtext(expression(paste("Income, y")), side = 1, line = 2.5, cex = axislabelsize)
text(-5, 0.5*ylims[2], expression(paste("Utility of income, u(y)")), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(0.55*xlims[2], -0.3, expression(paste("Income, y")), xpd = TRUE, cex = axislabelsize) 

text(-2.5, ConcaveU(10) + 0.03, expression(paste(u(y^{c^B}))), xpd = TRUE, cex = labelsize)
text(-2.5, 1.6275*ConcaveU(3) - 0.03, expression(v(L)[d]), xpd = TRUE, cex = labelsize)

# label yhat on x
text(16, -0.2, expression(paste(hat(y)[c] == hat(y)[d])), xpd = TRUE, cex = labelsize)
segments(16, 0, 16, ConcaveU(16) - 0.05, lty = 2, col = grays[20], lwd = segmentlinewidth)


# indiff
lines(xx1, ConcaveU(xx1), col = COLA[5], lwd = graphlinewidth)

#Points cB to cG
# segments(10, ConcaveU(10), 22, ConcaveU(22), lty = 2, col = COLB[4], lwd = graphlinewidth)
segments(10, ConcaveU(10), 22, ConcaveU(22), lty = 2, col = COLB[4], lwd = graphlinewidth)

#Point dB to dG
# segments(3, ConcaveU(3), 29, ConcaveU(29), lty = 2, col = COLB[4], lwd = graphlinewidth)
segments(2, ConcaveU(2), 30, ConcaveU(30), lty = 2, col = COLB[4], lwd = graphlinewidth)

#Label 4 points on line
#Point dB
text(2.8, ConcaveU(3) - .4, expression(paste(d^B)), cex = labelsize)
# segments(3, ConcaveU(3), 3, 0, lty = 2, col = grays[20], lwd = segmentlinewidth)
points(2, ConcaveU(2), pch = 16, col = "black", cex = 1.5)

#Point cG
text(22.8, ConcaveU(22) - .1, expression(paste(c^G)), cex = labelsize)
# segments(22, ConcaveU(22), 22, 0, lty = 2, col = grays[20], lwd = segmentlinewidth)
points(22, ConcaveU(22), pch = 16, col = "black", cex = 1.5)

#Point cB
text(10 - 0.8, ConcaveU(10) + .1, expression(paste(c^B)), cex = labelsize)
# segments(10, ConcaveU(10), 10, 0, lty = 2, col = grays[20], lwd = segmentlinewidth)
points(10, ConcaveU(10), pch = 16, col = "black", cex = 1.5)

#Point dG
text(29.8, ConcaveU(29) - .1, expression(paste(d^G)), cex = labelsize)
# segments(36, ConcaveU(36), 36, 0, lty = 2, col = grays[20], lwd = segmentlinewidth)
points(30, ConcaveU(30), pch = 16, col = "black", cex = 1.5)

segments(0, 2*ConcaveU(3), 16, 2*ConcaveU(3), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(16, 2*ConcaveU(3), pch = 16, col = "black", cex = 1.5)

segments(0, 1.6275*ConcaveU(3), 16, 1.6275*ConcaveU(3), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(16, 1.6275*ConcaveU(3), pch = 16, col = "black", cex = 1.5)

text(xlims[2] - 2, ConcaveU(xlims[2] - 2) + 0.2, expression(paste(u(y))),  xpd = TRUE,  cex = labelsize)


dev.off()
