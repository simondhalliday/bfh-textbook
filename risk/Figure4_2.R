#Graph Designer(s): Simon Halliday & Riley Boeth '17
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "risk/risk_neutral_u.pdf", width = 10, height = 8)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 10, 4, 4))

#Linear utility of wealth equation

LinearU <- function(x,y) {
  x
}


#Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 40)
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
     xaxs="i"#, 
     #yaxs="i"
)

#x and y limits with plain axes without ticks/numbers to match previous graph

ticksx <- seq(from = 0, to = xlims[2]+1, by = 41)
xlabels <- seq(from = 0, to = xlims[2], by = 1)
ticksy <- seq(from = 0, to = ylims[2]+1, by = 41)
ylabels <- seq(from = 0, to = ylims[2], by = 10)

axis(1,at = ticksx,  pos = 0, labels = FALSE)
axis(2,at = ticksy,  pos = 0, labels = FALSE, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Axis labels and draw linear utility function
mtext(expression(paste("Wealth, y")), side = 1, line = 2.5, cex = axislabelsize)
text(-9, 0.5*ylims[2], expression(paste("The utility of wealth, u(y)")), xpd = TRUE, cex = axislabelsize, srt = 90) 

lines(xx1, LinearU(xx1, y), col = COLA[5], lwd = graphlinewidth)

#Label 3 points on line

text(4, 5.5, expression(paste("a")), cex = labelsize)
segments(4, 0, 4, 4, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, 4, 4, 4, lty = 2, col = "gray", lwd = segmentlinewidth)
points(4, 4, pch = 16, col = "black", cex = 1.5)

text(20, 21.5, expression(paste("b")), cex = labelsize)
segments(20, 0, 20, 20, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, 20, 20, 20, lty = 2, col = "gray", lwd = segmentlinewidth)
points(20, 20, pch = 16, col = "black", cex = 1.5)

text(36, 37.5, expression(paste("c")), cex = labelsize)
segments(36, 0, 36, 36, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, 36, 36, 36, lty = 2, col = "gray", lwd = segmentlinewidth)
points(36, 36, pch = 16, col = "black", cex = 1.5)

#Label y-sub,x-sub,etc. on axes

text(36, -.9, expression(paste(y +  delta[1])), xpd = TRUE, cex = labelsize)
text(20, -.9, expression(paste(y == y[0])),  xpd = TRUE,  cex = labelsize)
text(4, -.9, expression(paste(y - delta[2])),  xpd = TRUE,  cex = labelsize)


text(-3, 36, expression(paste(u(y + delta[1]))), xpd = TRUE, cex = labelsize)
text(-2, 21.7, expression(paste(u(y[0]) == phantom())),  xpd = TRUE, cex = labelsize)
text(-2.8, 20, expression(paste(p%.%u(y + delta[1]) + phantom())),  xpd = TRUE, cex = labelsize)
text(-4.2, 18.3, expression(paste((1 - p)%.%u(y - delta[2]))),  xpd = TRUE, cex = labelsize)
text(-1.8, 16.6, expression(paste(phantom() == v(L))),  xpd = TRUE, cex = labelsize)
text(-3, 4, expression(paste(u(y - delta[2]))),  xpd = TRUE,  cex = labelsize)


dev.off()