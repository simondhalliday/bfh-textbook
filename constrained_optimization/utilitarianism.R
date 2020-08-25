#Graph Designer: Simon Halliday
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics
library(shape)
pdf(file = "constrained_optimization/utilitarianism.pdf", width = 8, height = 6)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 2
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 9, 2, 1))

#proportion of wealth functions

MUb <- function(x, y){
   0.75*x
}

MUa <- function(x, y){
  30 - (0.75)*x
}


#Add limits on axes and levels of utility for each function 
ylims <- c(0, 42)
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
     xaxs="i", 
     yaxs="i"
)

#x and y limits with plain axes without ticks/numbers to match previous graph; y axes on both sides

ticksx <- c(0, 20, 33, 40)
xlabels <- c(0, expression(paste(a[i])), expression(paste(a[h])), 1)
#ticksy <- seq(from = 0, to = ylims[2]+1, by = 41)
ticksy <- c(0, MUa(33), MUb(33), ylims[2])
#ylabels <- seq(from = 0, to = ylims[2], by = 10)
ylabels <- c(0, expression(paste(m*u^A*(a[h]) )), expression(paste(m*u^B*(1 - a[h]) )), NA)
ticksy2 <- seq(from = 0, to = ylims[2]+1, by = 41)
ylabels2 <- seq(from = 0, to = ylims[2], by = 10)

# text(-2, MUb(33), expression(paste(-u[a]^B*(a[x]) )),  xpd = TRUE, cex = labelsize)
# text(-2, MUa(33), expression(paste(u[a]^A*(a[x]) )),  xpd = TRUE, cex = labelsize)


# text(20, -.9, expression(paste(a[i])), xpd = TRUE, cex = labelsize)
# text(33, -.9, expression(paste(a[x])), xpd = TRUE, cex = labelsize)

axis(1,at = ticksx,  pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)
axis(4, at = ticksy2, pos = xlims[2], labels = FALSE,las=1)


npts <- 503 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Axis labels and draw linear utility functions
#mtext(expression(paste("A's Share of Wealth, a")), side = 1, line = 2, cex = axislabelsize)

text(0.5*xlims[2], -5.5, expression(paste("A's share of wealth, a")), xpd = TRUE, cex = axislabelsize) 
text(-11, 0.5*ylims[2], expression(paste("Marginal utility")), xpd = TRUE, cex = axislabelsize, srt = 90) 

lines(xx1, MUb(xx1, y), col = COLB[4], lwd = graphlinewidth)
lines(xx1, MUa(xx1, y), col = COLA[5], lwd = graphlinewidth)

#Label points on line

text(20, MUb(20) + 2, expression(paste(i)), cex = labelsize)
segments(20, 0, 20, MUb(20), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, MUb(20), 20, MUb(20), lty = 2, col = "gray", lwd = segmentlinewidth)
points(20, MUb(20), pch = 16, col = "black", cex = 1.5)

text(33 + 1, MUb(33) - 1, expression(paste(g)), cex = labelsize)
segments(33, MUb(33), 0, MUb(33), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(33, 0, 33, MUb(33), lty = 2, col = "gray", lwd = segmentlinewidth)
points(33, MUb(33), pch = 16, col = "black", cex = 1.5)

text(33 + 1, MUa(33) + 1, expression(paste(h)), cex = labelsize)
segments(33, MUa(33), 0, MUa(33), lty = 2, col = "gray", lwd = segmentlinewidth)
points(33, MUa(33), pch = 16, col = "black", cex = 1.5)



text(8, 30, expression(paste("A's marginal utility")), xpd = TRUE, cex = labelsize)
#text(8, 37.5, expression(paste(u[a]^A)), xpd = TRUE, cex = labelsize)
text(32, 30, expression(paste("B's marginal utility")),  xpd = TRUE, cex = labelsize)
#text(32, 37.5, expression(paste(-u[a]^B)),  xpd = TRUE, cex = labelsize)

#Axis arrow
Arrows(30.5, -5.5, 38, -5.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5, xpd = TRUE)



dev.off()