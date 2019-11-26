#Graph Designer: Simon Halliday & Scott Cohn
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(pBrackets)
pdf(file = "risk/indiff_no_tax.pdf", width = 10, height = 8)


# Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(4, 5, 1, 1))

indiffA <- function(x, ua = 5, slope = 1/12) {
  ua + slope*(x)^2
}

seg <- function(x, m, b){
  m*x + b
}

# Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 40)
xlims <- c(0, 25)

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

ticksx <- c(xlims[1], xlims[2])
xlabels <- c(0, NA)
#expression(paste(Delta))

ticksy <- c(ylims[1], 3, ylims[2])
ylabels <- c(NA, expression(paste(underline(y)(1 - phi))), NA)
#expression(paste("y"))

axis(1,at = ticksx,  pos = 0, labels = xlabels)
axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(1, 5, length.out = npts)
xx4 <- seq(5, 11, length.out = npts)
xx5 <- seq(7, 13, length.out = npts)


# Shade above and below tax line
polygon(c(0, xx1, xlims[2]), c(3, 0, indiffA(xx1, ua = 3, slope = 1/3)), border = FALSE, col = COLB[1])
polygon(c(0, xx1, xlims[2]), c(0, indiffA(xx1, ua = 3, slope = 1/3), 0), border = FALSE, col = COLA[1])
polygon(c(0, 0, 5), c(3, ylims[2], ylims[2]), border = FALSE, col = COLB[1])


lines(xx1, indiffA(xx1, ua = 5), col = COLB[4], lwd = graphlinewidth)
lines(xx1, indiffA(xx1, ua = 16.2), col = COLB[4], lwd = graphlinewidth)
lines(xx1, indiffA(xx1, ua = 28.4), col = COLB[4], lwd = graphlinewidth)

lines(xx1, indiffA(xx1, ua = 3, slope = 1/3), col = COLA[4], lty = 1, lwd = graphlinewidth)

lines(xx3, seg(x = xx3, m = 0.4714, b = 4.3), col = "dark grey", lty = 2, lwd = segmentlinewidth)
lines(xx4, seg(x = xx4, m = 1.21106, b = 11.79), col = "dark grey", lty = 2, lwd = segmentlinewidth)
lines(xx5, seg(x = xx5, m = 1.67995, b = 19.94), col = "dark grey", lty = 2, lwd = segmentlinewidth)

# Axis labels and draw linear utility function
mtext(expression(paste("Risk, ", Delta)), side = 1, line = 2.5, cex = axislabelsize)
text(-1.5, 0.5*ylims[2], expression(paste("Average income, ", "y")), xpd = TRUE, cex = axislabelsize, srt = 90) 

# Label the three indifference curves


text(20, indiffA(20)+3.355, expression(paste("v"[1])), xpd = TRUE, cex = labelsize)
text(17, indiffA(18)+9.8, expression(paste("v"[2])),  xpd = TRUE, cex = labelsize)
text(12, indiffA(16)+15.5, expression(paste("v"[3])),  xpd = TRUE, cex = labelsize)

text(20 + 0.7, indiffA(20)+0.855, expression(paste(v[1])), xpd = TRUE, cex = labelsize)
text(17 + 0.2, indiffA(18)+7.3, expression(paste(v[2])),  xpd = TRUE, cex = labelsize)
text(12 + 0.3, indiffA(16)+13, expression(paste(v[3])),  xpd = TRUE, cex = labelsize)

text(20 + 0.7, indiffA(20)+0.855, expression(paste(v[1])), xpd = TRUE, cex = labelsize)
text(17 + 0.2, indiffA(18)+7.3, expression(paste(v[2])),  xpd = TRUE, cex = labelsize)
text(12 + 0.3, indiffA(16)+13, expression(paste(v[3])),  xpd = TRUE, cex = labelsize)

# Label average wealth curve and indifference curves generally

text(3, 35, expression(paste("Prefer")), xpd = TRUE, cex = labelsize)
text(3, 33.5, expression(paste("No Tax")), xpd = TRUE, cex = labelsize)

text(18, 12, expression(paste("Prefer")), xpd = TRUE, cex = labelsize)
text(18, 10.5, expression(paste(tau > 0)), xpd = TRUE, cex = labelsize)

text(7.5, indiffA(16)+2, expression(paste("No Tax")), xpd = TRUE, cex = labelsize)
text(7.5, indiffA(16)+0.5, expression(paste("Curve")), xpd = TRUE, cex = labelsize)

# Segments

segments(0, 3, xlims[2] - 2, 3, lty = 2, col = "dark gray", lwd = segmentlinewidth)
#text(xlims[2] - 1, 3, expression(paste(Delta)), xpd = TRUE, cex = labelsize)

# Points
points(2.8284, indiffA(2.8, ua = 5), pch = 16, col = "black", cex = 1.5,xpd = TRUE)
text(3.2, indiffA(2.8, ua = 5) - 0.5, expression(paste("f")), xpd = TRUE, cex = labelsize)

points(7.26636, indiffA(7.26636, ua = 16.2), pch = 16, col = "black", cex = 1.5,xpd = TRUE)
text(7.76636, indiffA(7.26636, ua = 16.2) - 0.5, expression(paste("g")), xpd = TRUE, cex = labelsize)

points(10.0797, indiffA(10.0797, ua = 28.4), pch = 16, col = "black", cex = 1.5,xpd = TRUE)
text(10.5797, indiffA(10.0797, ua = 28.4) - 0.5, expression(paste("h")), xpd = TRUE, cex = labelsize)


dev.off()