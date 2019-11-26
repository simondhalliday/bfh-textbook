#Graph Designer: Simon Halliday & Scott Cohn
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(pBrackets)
pdf(file = "risk/taxes_transfers.pdf", width = 10, height = 8)


#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 1, 1))

seg <- function(x, m, b){
  m*x + b
}

#Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 30)
xlims <- c(0, 30)

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


ticksx <- c(xlims[1], 7.5, 15, 22.5, xlims[2])
xlabels <- c(NA, expression(paste(y[L])), expression(paste(underline(y)(1 -  phi))), expression(paste(y[H])), NA)

ticksy <- c(ylims[1], 15, ylims[2])
ylabels <- c(NA, NA, NA)

axis(1,at = ticksx,  pos = 0, labels = xlabels)
axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)
xx5 <- seq(xlims[1], 25, length.out = npts)

#Axis labels and draw linear utility function
#mtext(expression(paste("Risk, ", Delta)), side = 1, line = 2.5, cex = axislabelsize)
text(-1.2, 0.5*ylims[2], expression(paste("Taxes paid, ", hat(T), ", and transfers received, $")), xpd = TRUE, cex = axislabelsize, srt = 90) 
lines(xx5, seg(m = 1, x = xx5, b = 0), col = COLA[4], lwd = graphlinewidth)

# Segments
segments(15, 0, 15, 15, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, 15, 25, 15, lty = 1, col = COLB[4], lwd = graphlinewidth)

segments(0, 7.5, 7.5, 7.5, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(7.5, 0, 7.5, 7.5, lty = 2, col = "gray", lwd = segmentlinewidth)


segments(0, 22.5, 22.5, 22.5, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(22.5, 0, 22.5, 22.5, lty = 2, col = "gray", lwd = segmentlinewidth)


# Points
points(15, 15, pch = 16, col = "black", cex = 1.5,xpd = TRUE)
points(7.5, 7.5, pch = 16, col = "black", cex = 1.5,xpd = TRUE)
points(22.5, 22.5, pch = 16, col = "black", cex = 1.5,xpd = TRUE)

# Brackets
brackets(x1 = 6.5, y1 = 8, x2 = 6.5, y2 = 14.5,  ticks = 0.5, curvature = 0.5, type = 1, 
         col = "black", lwd = 2, lty = 1, h = 0.5, xpd = TRUE)

brackets(x1 = 23, y1 = 22, x2 = 23, y2 = 15.5,  ticks = 0.5, curvature = 0.5, type = 1, 
         col = "black", lwd = 2, lty = 1, h = 0.5, xpd = TRUE)

# Labels
text(4, 12.75, expression(paste("Taxes paid")), xpd = TRUE, cex = labelsize)
text(4, 11.25, expression(paste("lower than")), xpd = TRUE, cex = labelsize)
text(4, 9.75, expression(paste("transfers")), xpd = TRUE, cex = labelsize)

text(25.5, 12.75 + 7.5, expression(paste("Taxes paid")), xpd = TRUE, cex = labelsize)
text(25.5, 11.25 + 7.5, expression(paste("higher than")), xpd = TRUE, cex = labelsize)
text(25.5, 9.75 + 7.5, expression(paste("transfers")), xpd = TRUE, cex = labelsize)

text(7.5, 8.5, expression(paste("a")), xpd = TRUE, cex = labelsize)
text(15, 16, expression(paste("b")), xpd = TRUE, cex = labelsize)
text(22.5, 23.5, expression(paste("c")), xpd = TRUE, cex = labelsize)

text(27, 27, expression(paste("Taxes")), xpd = TRUE, cex = labelsize)
text(27, 25.5, expression(paste("Paid")), xpd = TRUE, cex = labelsize)
text(27, 24, expression(paste((tau*y))), xpd = TRUE, cex = labelsize)

text(27, 14.5, expression(paste("Transfers")), xpd = TRUE, cex = labelsize)
text(27, 13, expression(paste("Received")), xpd = TRUE, cex = labelsize)
text(27, 11.5, expression(paste(tau*underline(y), (1 - phi))), xpd = TRUE, cex = labelsize)



dev.off()