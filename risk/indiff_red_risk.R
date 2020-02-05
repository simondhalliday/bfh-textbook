#Graph Designer: Simon Halliday & Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(pBrackets)
pdf(file = "risk/indiff_red_risk.pdf", width = 10, height = 8)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5
a <- c(2, 4, 6)


COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(4, 7, .5, .5))
xlims <- c(0, 15)
ylims <- c(0, 18)

indiff <- function(g, intercept = 4, slope = 0.09){
  intercept + slope*g^2 + 0.15*g
}

seg <- function(x, m, b){
  m*x + b
}

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
     xaxs="i", 
     yaxs="i"
)

#x and y limits with plain axes without ticks/numbers to match previous graph

ticksx <- c(xlims[1], 2.1, 10, xlims[2])
xlabels <- c(NA, expression(paste(Delta^T)), expression(paste(Delta^P)), NA)

ticksy <- c(ylims[1], indiff(2.1, intercept = 8, slope = 0.075), indiff(10, intercept = 2, slope = 0.09), ylims[2])
ylabels <- c(NA, expression(paste(bar(y), "(1 - ", phi, ")")), expression(paste(y^P)), NA)

axis(1,at = ticksx,  pos = 0, labels = xlabels)
axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)
xx5 <- seq(3, 18, length.out = npts)


# lines(xx1, indiffA(xx1, ua = 2), col = COLB[4], lwd = graphlinewidth)
# lines(xx1, indiffA(xx1, ua = 16.2), col = COLB[4], lwd = graphlinewidth)
# lines(xx1, indiffA(xx1, ua = 28.4), col = COLB[4], lwd = graphlinewidth)

lines(xx1, indiff(xx1, intercept = 2, slope = 0.09), col = COLA[4], lwd = graphlinewidth, lty = 1)
#lines(xx1, indiff(xx1, intercept = 7, slope = 0.08), col = COLA[4], lwd = graphlinewidth, lty = 1)
lines(xx1, indiff(xx1, intercept = 8, slope = 0.075), col = COLA[4], lwd = graphlinewidth, lty = 1)
#lines(xx1, indiff(xx1, intercept = 9.2, slope = 0.08), col = COLA[4], lwd = graphlinewidth, lty = 1)
lines(xx1, indiff(xx1, intercept = 13, slope = 0.04), col = COLA[4], lwd = graphlinewidth, lty = 1)

#Axis labels and draw linear utility function
mtext(expression(paste("Risk, ", Delta)), side = 1, line = 2.5, cex = axislabelsize)
text(-2, 0.5*ylims[2], expression(paste("Expected income, ", hat(y))), xpd = TRUE, cex = axislabelsize, srt = 90) 

#label the three indifference curves

text(.5, 2.55, expression(u[1]), cex = labelsize)
#text(.5, 7.55, expression(u[2]), cex = labelsize)
text(.5, 8.4, expression(u[2]), cex = labelsize)
text(.5, 13.55, expression(u[3]), cex = labelsize)

#Label average wealth curve and indifference curves generally

text(12, 11.32, expression(paste("Reduced")), xpd = TRUE, cex = labelsize)
text(12, 10.57, expression(paste("Expected")), xpd = TRUE, cex = labelsize)
text(12, 9.82, expression(paste("Income")), xpd = TRUE, cex = labelsize)

# Bracket
brackets(x1 = 10.5, y1 = indiff(10, intercept = 2, slope = 0.09), 
         x2 = 10.5, y2 = indiff(2.1, intercept = 8, slope = 0.075),  ticks = 0.5, curvature = 0.5, type = 1, 
         col = "black", lwd = 2, lty = 1, h = 0.5, xpd = TRUE)

# Segments
segments(2.1, 0, 2.1, indiff(2.1, intercept = 8, slope = 0.075), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, indiff(2.1, intercept = 8, slope = 0.075), 10, indiff(2.1, intercept = 8, slope = 0.075), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, indiff(10, intercept = 2, slope = 0.09), 10, indiff(10, intercept = 2, slope = 0.09), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(10, 0, 10, indiff(10, intercept = 8, slope = 0.075), lty = 2, col = "gray", lwd = segmentlinewidth)

#The tangent line to the higher indifference curve
#m <- (indiffA(16, ua = 2) - indiffA(5, ua = 16.2))/(16 - 4.75)
#This works for a lower b. 
#lines(xx1, seg(x = xx1, m = 0.43, b = 15.5), col = COLA[2], lty = 2, lwd = segmentlinewidth)
#Trying to find a higher b. 
lines(xx1, seg(x = xx1, m = 0.482, b = 7.64), col = COLB[4], lty = 1, lwd = graphlinewidth)

# Points
points(2.1, indiff(2.1, intercept = 8, slope = 0.075), pch = 16, col = "black", cex = 1.5,xpd = TRUE)
text(2.1 + 0.25, indiff(2.1, intercept = 8, slope = 0.075) - 0.5, expression(paste("g'")), xpd = TRUE, cex = labelsize)

points(10, indiff(10, intercept = 2, slope = 0.09) - 0.05, pch = 16, col = "black", cex = 1.5,xpd = TRUE)
text(10 - 0.15, indiff(10, intercept = 2, slope = 0.09) + 0.5, expression(paste("g")), xpd = TRUE, cex = labelsize)

points(10, indiff(10, intercept = 8, slope = 0.075), pch = 16, col = "black", cex = 1.5,xpd = TRUE)
text(10 + 0.25, indiff(10, intercept = 8, slope = 0.075) - 0.25, expression(paste("h")), xpd = TRUE, cex = labelsize)


text(13, 15.5, expression(paste("Tax and")), xpd = TRUE, cex = labelsize)
text(13, 14.75, expression(paste("transfer line")), xpd = TRUE, cex = labelsize)

# Arrows
Arrows(9.5, -0.8, 2.5, -0.8, col = "black", code = 2, xpd = TRUE, length = 0.1, lwd = 2, arr.type = "triangle")
Arrows(-0.5, indiff(10, intercept = 2, slope = 0.09) - 0.5, -0.5, indiff(2.1, intercept = 8, slope = 0.075) + 0.75, col = "black", code = 2, xpd = TRUE, length = 0.1, lwd = 2, arr.type = "triangle")


dev.off()