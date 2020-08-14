#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(pBrackets)

pdf(file = "indmarketdemand/decomposition_cd_update.pdf", width = 8, height = 8)

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
grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(7, 7, 1, 1.5))

#Original budget constraint and pivoted budget constraint

bc1 <- function(x, slope = 0.75) {
  40 - slope*x
}

bc2 <- function(x) {
  40 - .345*x
}

#Utility functions 

uFn <- function(x, y, alpha = 0.5){
  (x^alpha)*(y^(1-alpha))
}

#Compensated budget constraint parallel to bc1

cbc1 <- function(x, int = 66.8, slope = 0.7) {
  int - slope*x
}

#Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 50)
xlims <- c(0, 115)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

#a <- c(23.2, 36.6) #alpha = 0.6
a <- c(23.2, 40) #alpha = 0.6

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

ticksx <- c(0, 26, 48, 78.25, xlims[2])
xlabels <- c(NA, expression(paste(x[b])), expression(paste(x[c])) , expression(paste(x[a])), NA)
ticksy <- c(0, bc1(26), cbc1(48), ylims[2])
ylabels <- c(NA, expression(paste(y[b] == y[a])), expression(paste(y[c])), NA)

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
        col = CBCols[1],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE)

#Axis labels
#mtext(expression(paste("Kilograms of coffee, ", x)), side = 1, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], -9, expression(paste("Kilograms of coffee, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-21, 0.5*ylims[2], expression(paste("Gigabytes of data, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

lines(xx1, bc1(xx1, slope = 0.75), col = COLB[3], lwd = graphlinewidth)
#lines(xx1, bc1(xx1, slope = 0.3), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bc1(xx1, slope = 0.25), col = COLB[3], lwd = graphlinewidth)

lines(xx1, cbc1(xx1, int = 66.8, slope = 0.7), col = COLB[5], lwd = graphlinewidth)


#Label curves

text(8, 49, expression(u[1]), cex = labelsize)
text(30, 49, expression(u[2]), cex = labelsize)

text(37, 8, expression(bc[b]), cex = labelsize)
text(37, 6, expression(paste("budget")), cex = labelsize)
text(37, 4, expression(paste("when ", p[x])), cex = labelsize)
text(37, 2, expression(paste("increases")), cex = labelsize)

text(106, 12, expression(bc[a]), cex = labelsize)
text(106, 10, expression(paste("initial")), cex = labelsize)
text(106, 8, expression(paste("budget")), cex = labelsize)


text(88, 2.75, expression(bc[c]), cex = labelsize)
text(106, 2.75, expression(paste("compensated")), cex = labelsize, xpd = TRUE)
text(106, 1, expression(paste("budget")), cex = labelsize, xpd = TRUE)

#Label points e-sub, e, e'

text(48 + 3, cbc1(48) + 1, expression(paste(c)), cex = labelsize)
segments(48, 0, 48, cbc1(x = 48), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, cbc1(x = 48), 48, cbc1(48), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(48, cbc1(x = 48), pch = 16, col = "black", cex = 1.5)

#Point on initial budget constraint
text(78.25 + 3, bc1(78.25, slope = 0.25) + 1, expression(paste(a)), cex = labelsize)
segments(78.25, 0, 78.25, bc1(x = 78.25, slope = 0.25), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, bc1(x = 78.25, slope = 0.25), 78.25, bc1(x = 78.25, slope = 0.25), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(78.25, bc1(x = 78.25, slope = 0.25), pch = 16, col = "black", cex = 1.5)

#Point on budget constraint with price increase
text(26 - 2, bc1(26) - .9, expression(paste(b)), cex = labelsize)
segments(26, 0, 26, bc1(x = 26), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(26, bc1(x = 26), pch = 16, col = "black", cex = 1.5)

brackets(48 - 0.5, -2.9, 26 + 0.5, -2.9,  h = 1,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = segmentlinewidth, lty = 1, xpd = TRUE)

text(26 + (48 - 26)/2, -4.5, expression(paste("Income")), cex = labelsize, xpd = TRUE)
text(26 + (48 - 26)/2, -6.2, expression(paste("effect")), cex = labelsize, xpd = TRUE)


brackets(78.25 - 0.5, -2.9, 48 + 0.5, -2.9,  h = 1,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = segmentlinewidth, lty = 1, xpd = TRUE)

text(56 + (78.25 - 56)/2, -4.5, expression(paste("Substitution")), cex = labelsize, xpd = TRUE)
text(56 + (78.25 - 56)/2, -6.2, expression(paste("effect")), cex = labelsize, xpd = TRUE)


#Label y-sub,x-sub,etc. on axes


dev.off()