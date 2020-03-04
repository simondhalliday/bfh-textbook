#Graph Designer: Simon Halliday + Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "indmarketdemand/decomposition_qql.pdf", width = 8, height = 8)

#Set parameters for graphics
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

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 5, 1, 1))

#Original budget constraint and pivoted budget constraint

bc1 <- function(x) {
  40 - .5*x
}

bc2 <- function(x) {
  40 - .33*x
}

#Utility functions 

uFn <- function(x, y, rmax = 1, xmax = 80) {
  y + rmax*x - (1/2)*(rmax/xmax)*x^2
}

#Compensated budget constraint parallel to bc1

cbc1 <- function(x, int = 48) {
  int - .5*x
}

#Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 40)
xlims <- c(0, 80)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

a <- c(50, 58) #alpha = 0.6

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

ticksx <- c(0, 39, 55, xlims[2])
xlabels <- c(NA, expression(paste(x[e*minute]) == x[sub]), expression(paste(x[e])), NA )
ticksy <- c(0, bc1(39), bc2(55), cbc1(39), ylims[2])
ylabels <- c(NA, expression(paste(y[e*minute])), expression(paste(y[e])), expression(paste(y[sub])), NA)

axis(1,at = ticksx,  pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)


npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

contour(x, y, 
        outer(x, y, uFn),
        drawlabels = FALSE,
        col = COLA[4],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE)


#Axis labels
text(0.5*xlims[2], -4, expression(paste("Kilograms of fish, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-9.5, 0.5*ylims[2], expression(paste("Money for other goods, y")), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label curves

text(13.5, 39, expression(u[1]), cex = labelsize)
text(24, 39, expression(u[2]), cex = labelsize)
text(78, 2.5, expression(bc[1]), cex = labelsize)
text(78, 13, expression(bc[2]), cex = labelsize)
text(78, 7.5, expression(cbc[1]), cex = labelsize)

#Label points e-sub, e, e'

text(40 + 2, cbc1(39)+.9, expression(paste(e[sub])), cex = labelsize)
segments(39, 0, 39, cbc1(x = 39), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, cbc1(x = 39), 39, cbc1(x = 39), lty = 2, col = "gray", lwd = segmentlinewidth)


text(55 + 0.8, bc2(55) + .8, expression(paste(e)), cex = labelsize)
segments(55, 0, 55, bc2(x = 55), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, bc2(x = 55), 55, bc2(x = 55), lty = 2, col = "gray", lwd = segmentlinewidth)


text(39 - 1.5, bc1(39) - 1, expression(paste(e*minute)), cex = labelsize)
segments(0, bc1(x = 39), 39, bc1(x = 39), lty = 2, col = "gray", lwd = segmentlinewidth)


#Label y-sub,x-sub,etc. on axes

#text(37.5, -.9, expression(paste(x[sub])), xpd = TRUE, cex = labelsize)
#text(40.8, -.9, expression(paste(x[e*minute])),  xpd = TRUE,  cex = labelsize)
#text(56.9, -.9, expression(paste(x[e])),  xpd = TRUE, cex = labelsize)


#text(-2, cbc1(39), expression(paste(y[sub])), xpd = TRUE, cex = labelsize)
#text(-1.8, bc1(39)+.5, expression(paste(y[e*minute])),  xpd = TRUE,  cex = labelsize)
#text(-2.1, bc1(39)-1, expression(paste(y[e])),  xpd = TRUE, cex = labelsize)

lines(xx1, bc1(xx1), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bc2(xx1), col = COLB[3], lwd = graphlinewidth)
lines(xx1, cbc1(xx1), col = COLB[5], lwd = graphlinewidth)

points(39, cbc1(x = 39), pch = 16, col = "black", cex = 1.5)
points(55, bc2(x = 55), pch = 16, col = "black", cex = 1.5)
points(39, bc1(x = 39), pch = 16, col = "black", cex = 1.5)

dev.off()