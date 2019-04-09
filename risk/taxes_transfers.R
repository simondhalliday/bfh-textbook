#Graph Designer: Simon Halliday & Scott Cohn
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(pBrackets)
pdf(file = "risk/taxes_transfers.pdf", width = 10, height = 8)


#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 10, 4, 4))

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


ticksx <- c(xlims[1], 15, xlims[2])
xlabels <- c(NA, expression(paste(underline(y)(1 -  phi))), expression(paste("y")))

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
#text(-5, 0.5*ylims[2], expression(paste("Average Wealth, ", "y")), xpd = TRUE, cex = axislabelsize, srt = 90) 

lines(xx5, seg(m = 1, x = xx5, b = 0), col = COLA[4], lwd = graphlinewidth)

# Segments
segments(15, 0, 15, 15, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, 15, 25, 15, lty = 1, col = COLB[4], lwd = graphlinewidth)


# Points
points(15, 15, pch = 16, col = "black", cex = 1.5,xpd = TRUE)

# labels
text(27, 27, expression(paste("Taxes")), xpd = TRUE, cex = labelsize)
text(27, 25.5, expression(paste("Paid")), xpd = TRUE, cex = labelsize)
text(27, 24, expression(paste("(ty)")), xpd = TRUE, cex = labelsize)

text(27, 16.5, expression(paste("Transfers")), xpd = TRUE, cex = labelsize)
text(27, 15, expression(paste("Recieved")), xpd = TRUE, cex = labelsize)
text(27, 13.5, expression(paste("t", underline(y), (1 - phi))), xpd = TRUE, cex = labelsize)


dev.off()