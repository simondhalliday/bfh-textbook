#' Graph Designer(s): Simon Halliday,  Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(pBrackets)
pdf(file = "risk/risk_averse_dara_b.pdf", width = 10, height = 8)

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
par(mar =  c(5, 6, 2, 3))

#Concave utility of wealth function

ConcaveU <- function(x){
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

ticksy <- c(0, ConcaveU(2), ConcaveU(11), ConcaveU(24.5), ConcaveU(33.5), ylims[2])
ylabels <- c(NA, expression(paste(u(y^{c}) )), expression(paste(u(y^c^minute))), expression(paste(u(y^d))), expression(paste(u(y^d^minute))), NA)
ticksx <- c(0, 6.5, 29, xlims[2])
xlabels <- c(NA, NA, NA, NA) # labels are under axis title labels


axis(1, at = ticksx,  pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy,  pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Axis labels and draw linear utility function
mtext(expression(paste("Wealth, y")), side = 1, line = 2.5, cex = axislabelsize)
text(-5, 0.5*ylims[2], expression(paste("Utility of wealth, u(y)")), xpd = TRUE, cex = axislabelsize, srt = 90) 

# label yhat
text(6.5, -0.2, expression(paste(hat(y)^{L})), xpd = TRUE, cex = labelsize)
segments(6.5, 0, 6.5, ConcaveU(6.5), lty = 2, col = grays[20], lwd = segmentlinewidth)

text(29, -0.2, expression(paste(hat(y)^{H})), xpd = TRUE, cex = labelsize)
segments(29, 0, 29, ConcaveU(29), lty = 2, col = grays[20], lwd = segmentlinewidth)

# util
lines(xx1, ConcaveU(xx1), col = COLA[5], lwd = graphlinewidth)

#Points B to C
segments(2, ConcaveU(2), 11, ConcaveU(11), lty = 2, col = COLB[4], lwd = graphlinewidth)

#Point A to D
segments(24.5, ConcaveU(24.5), 33.5, ConcaveU(33.5), lty = 2, col = COLB[4], lwd = graphlinewidth)

#Label 4 points on line
#Point a
text(2.8, ConcaveU(2) - .12, expression(paste(c)), cex = labelsize)
points(2, ConcaveU(2), pch = 16, col = "black", cex = 1.5)

#Point b
text(11.8, ConcaveU(11) - .1, expression(paste(c*minute)), cex = labelsize)
points(11, ConcaveU(11), pch = 16, col = "black", cex = 1.5)

#Point c
text(24.5 + 0.8, ConcaveU(24.5) - .1, expression(paste(d)), cex = labelsize)
points(24.5, ConcaveU(24.5), pch = 16, col = "black", cex = 1.5)


#Point d 
text(33.5 + 0.8, ConcaveU(33.5) - .1, expression(paste(d*minute)), cex = labelsize)
points(33.5, ConcaveU(33.5), pch = 16, col = "black", cex = 1.5)

text(xlims[2] - 2, ConcaveU(xlims[2] - 2) + 0.2, expression(paste(u(y))),  xpd = TRUE,  cex = labelsize)



dev.off()
