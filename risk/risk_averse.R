#Graph Designer(s): Simon Halliday, Riley Boeth '17, Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(pBrackets)
pdf(file = "risk/risk_averse_labeled.pdf", width = 10, height = 8)

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

ticksy <- c(0, ConcaveU(3), ConcaveU(10), ConcaveU(22), ConcaveU(36), ylims[2])
ylabels <- c(NA, expression(paste(u(y^{a}) )), expression(paste(u(y^b))), expression(paste(u(y^c))), expression(paste(u(y^d))), NA)
ticksx <- c(0, 3, 10, 22, 36, xlims[2])
xlabels <- c(NA, expression(paste(y^{a})), expression(paste(y^b)), expression(paste(y^c)), expression(paste(y^d)), NA)


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

lines(xx1, ConcaveU(xx1), col = COLA[5], lwd = graphlinewidth)

#Add line from pt a to pt c
#segments(2, ConcaveU(2), 36, ConcaveU(36), lty = 1, col = COLB[4] , lwd = graphlinewidth)

#Points B to C
segments(10, ConcaveU(10), 22, ConcaveU(22), lty = 1, col = COLB[4], lwd = graphlinewidth)

#Point a to d
segments(3, ConcaveU(3), 36, ConcaveU(36), lty = 1, col = COLB[4], lwd = graphlinewidth)

#Label 4 points on line
#Point a
text(3.8, ConcaveU(3)-.12, expression(paste(a)), cex = labelsize)
segments(3, ConcaveU(3), 3, 0, lty = 2, col = grays[20], lwd = segmentlinewidth)
points(3, ConcaveU(3), pch = 16, col = "black", cex = 1.5)

#Point c
text(22.8, ConcaveU(22)-.1, expression(paste(c)), cex = labelsize)
segments(22, ConcaveU(22), 22, 0, lty = 2, col = grays[20], lwd = segmentlinewidth)
points(22, ConcaveU(22), pch = 16, col = "black", cex = 1.5)

#Point b
text(10.8, ConcaveU(10)-.1, expression(paste(b)), cex = labelsize)
segments(10, ConcaveU(10), 10, 0, lty = 2, col = grays[20], lwd = segmentlinewidth)
points(10, ConcaveU(10), pch = 16, col = "black", cex = 1.5)

#Point d 
text(36.8, ConcaveU(36)-.1, expression(paste(d)), cex = labelsize)
segments(36, ConcaveU(36), 36, 0, lty = 2, col = grays[20], lwd = segmentlinewidth)
points(36, ConcaveU(36), pch = 16, col = "black", cex = 1.5)

#Very risk averse
text(5, ConcaveU(5)+0.6, expression(paste("very risk")),  xpd = TRUE,  cex = labelsize)
text(5, ConcaveU(5)+0.4, expression(paste("averse")),  xpd = TRUE,  cex = labelsize)
#Risk averse
text(18, ConcaveU(18)+0.4, expression(paste("risk")),  xpd = TRUE,  cex = labelsize)
text(18, ConcaveU(18)+0.2, expression(paste("averse")),  xpd = TRUE,  cex = labelsize)
#Risk neutral
text(39, ConcaveU(39)+0.4, expression(paste("almost risk")),  xpd = TRUE,  cex = labelsize)
text(39, ConcaveU(39)+0.2, expression(paste("neutral")),  xpd = TRUE,  cex = labelsize)

Arrows(26.5, 2.5, 13.5, 2.5, col = "black", lty = 1, lwd = 2, code = 2, arr.type = "triangle", arr.lwd = 0.5)
text(31, 2.5, expression(paste(Delta^{bc} == y^c - y^b)),  xpd = TRUE,  cex = labelsize)
brackets(x1 = 26.5, y1 = 2.6, x2 = 35.5, y2 = 2.6,  
         ticks = 0.5, curvature = 0.5, type = 1, h = 0.15,  
         col = "black", lwd = 2, lty = 1, xpd = TRUE)
text(31, 2.9, expression(paste("low risk")),  xpd = TRUE,  cex = labelsize)


Arrows(26.5, 2, 13.5, 2, col = "black", lty = 1, lwd = 2, code = 2, arr.type = "triangle", arr.lwd = 0.5)
text(31, 2, expression(paste(Delta^{da} == y^d - y^a)),  xpd = TRUE,  cex = labelsize)
brackets(x1 = 35.5, y1 = 1.9, x2 = 26.5, y2 = 1.9,  
         ticks = 0.5, curvature = 0.5, type = 1, h = 0.15,
         col = "black", lwd = 2, lty = 1, xpd = TRUE)
text(31, 1.6, expression(paste("high risk")),  xpd = TRUE,  cex = labelsize)

dev.off()