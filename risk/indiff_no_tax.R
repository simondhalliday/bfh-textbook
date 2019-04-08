#Graph Designer: Simon Halliday & Scott Cohn
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

# TO DO
# Add slope lines for each point
# Figure out what is printing function (n, v - ... ) 

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

par(mar =  c(6, 10, 4, 4))

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
xlabels <- c(NA, Delta)

ticksy <- c(ylims[1], 3, ylims[2])
ylabels <- c(NA, expression(paste(underline(y)(1 - phi))), expression(paste("y")))

axis(1,at = ticksx,  pos = 0, labels = xlabels)
axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)
xx5 <- seq(3, 18, length.out = npts)


lines(xx1, indiffA(xx1, ua = 5), col = COLB[4], lwd = graphlinewidth)
lines(xx1, indiffA(xx1, ua = 16.2), col = COLB[4], lwd = graphlinewidth)
lines(xx1, indiffA(xx1, ua = 28.4), col = COLB[4], lwd = graphlinewidth)

lines(xx1, indiffA(xx1, ua = 3, slope = 1/3), col = COLA[2], lty = 2, lwd = graphlinewidth)
# Axis labels and draw linear utility function
mtext(expression(paste("Risk, ", Delta)), side = 1, line = 2.5, cex = axislabelsize)
text(-5, 0.5*ylims[2], expression(paste("Average Wealth, ", "y")), xpd = TRUE, cex = axislabelsize, srt = 90) 

# Label the three indifference curves

text(20, indiffA(20)+3.355, expression(paste("v"["-"])), xpd = TRUE, cex = labelsize)
text(17, indiffA(18)+9.8, expression(paste("v"[0])),  xpd = TRUE, cex = labelsize)
text(12, indiffA(16)+15.5, expression(paste("v"["+"])),  xpd = TRUE, cex = labelsize)

# Label average wealth curve and indifference curves generally

text(3, 35, expression(paste("Prefer")), xpd = TRUE, cex = labelsize)
text(3, 33.5, expression(paste("No Tax")), xpd = TRUE, cex = labelsize)

text(18, 12, expression(paste("Prefer")), xpd = TRUE, cex = labelsize)
text(18, 10.5, expression(paste(t > 0)), xpd = TRUE, cex = labelsize)

text(9, indiffA(16)+14, expression(paste("No Tax")), xpd = TRUE, cex = labelsize)
text(9, indiffA(16)+12.5, expression(paste("Curve")), xpd = TRUE, cex = labelsize)

# Segments

segments(0, 3, xlims[2] - 2, 3, lty = 2, col = "gray", lwd = segmentlinewidth)
text(xlims[2] - 1, 3, expression(paste(Delta)), xpd = TRUE, cex = labelsize)

# Points
points(2.8, indiffA(2.8, ua = 5), pch = 16, col = "black", cex = 1.5,xpd = TRUE)
text(3.2, indiffA(2.8, ua = 5) - 0.5, expression(paste("f")), xpd = TRUE, cex = labelsize)

points(7.25, indiffA(7.25, ua = 16.2), pch = 16, col = "black", cex = 1.5,xpd = TRUE)
text(7.75, indiffA(7.25, ua = 16.2) - 0.5, expression(paste("g")), xpd = TRUE, cex = labelsize)

points(10.1, indiffA(10.1, ua = 28.4), pch = 16, col = "black", cex = 1.5,xpd = TRUE)
text(10.6, indiffA(10.1, ua = 28.4) - 0.5, expression(paste("h")), xpd = TRUE, cex = labelsize)


dev.off()