#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "indmarketdemand/decomposition_cd.pdf", width = 8, height = 8)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.3
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 6, 2, 2))

#Original budget constraint and pivoted budget constraint

bc1 <- function(x) {
  40 - .5*x
}

bc2 <- function(x) {
  40 - .345*x
}

#Utility functions 

uFn <- function(x, y, alpha = 0.5){
  (x^alpha)*(y^(1-alpha))
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

a <- c(28.25, 34) #alpha = 0.6

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

ticksx <- c(0, 39, 47, 56.8, xlims[2])
xlabels <- c(NA, expression(paste(x[e*minute])), expression(paste(x[sub])) , expression(paste(x[e])), NA)
ticksy <- c(0, bc1(39), cbc1(47), ylims[2])
ylabels <- c(NA, expression(paste(y[e*minute] == y[e])), expression(paste(y[sub])), NA)

axis(1, at = ticksx,  pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy,  pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

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
mtext(expression(paste("Kilograms of coffee, ", x)), side = 1, line = 2.5, cex = axislabelsize)
text(-12.5, 0.5*ylims[2], expression(paste("Gigabytes of data, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

lines(xx1, bc1(xx1), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bc2(xx1), col = COLB[3], lwd = graphlinewidth)
lines(xx1, cbc1(xx1), col = COLB[5], lwd = graphlinewidth)


#Label curves

text(22, 39, expression(u[1]), cex = labelsize)
text(31.5, 39, expression(u[2]), cex = labelsize)
text(78, 2.5, expression(bc[1]), cex = labelsize)
text(78, 12, expression(bc[2]), cex = labelsize)
text(78, 7.5, expression(cbc[1]), cex = labelsize)

#Label points e-sub, e, e'

text(47 + 1, cbc1(47) + 1, expression(paste(e[sub])), cex = labelsize)
segments(47, 0, 47, cbc1(x = 47), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, cbc1(x = 47), 47, cbc1(47), lty = 2, col = "gray", lwd = segmentlinewidth)
points(47, cbc1(x = 47), pch = 16, col = "black", cex = 1.5)

text(57.8, bc2(56.8)+.8, expression(paste("e")), cex = labelsize)
segments(56.8, 0, 56.8, bc2(x = 56.8), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, bc2(x = 56.8), 56, bc2(x = 56.8), lty = 2, col = "gray", lwd = segmentlinewidth)
points(56.8, bc2(x = 56.8), pch = 16, col = "black", cex = 1.5)

text(39 - 0.9, bc1(39) - .9, expression(paste(e*minute)), cex = labelsize)
segments(39, 0, 39, bc1(x = 39), lty = 2, col = "gray", lwd = segmentlinewidth)
points(39, bc1(x = 39), pch = 16, col = "black", cex = 1.5)


#Label y-sub,x-sub,etc. on axes

#text(37.5, -.9, expression(paste(x[sub])), xpd = TRUE, cex = labelsize)
#text(40.8, -.9, expression(paste(x[e'])),  xpd = TRUE,  cex = labelsize)
#text(56.9, -.9, expression(paste(x[e])),  xpd = TRUE, cex = labelsize)

#text(-2, cbc1(47), expression(paste(y[sub])), xpd = TRUE, cex = labelsize)
#text(-2, bc1(39) + .5, expression(paste(y[e*minute] == y[e])),  xpd = TRUE,  cex = labelsize)
#text(-2, bc1(39) - 1, expression(paste(y[e])),  xpd = TRUE, cex = labelsize)



dev.off()