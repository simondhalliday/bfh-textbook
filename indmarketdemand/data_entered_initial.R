#Graph Designer: Simon Halliday + Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "specprodexch/data_entered_initial.pdf", width = 8, height = 8)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
smalllabelsize <- 0.9
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")


ffA <- function(x, yintA = 20, sA = 20/11) {
  yintA - sA*x
}

ffB <- function(x, yintB = 8, sB = 8/10) {
  yintB - sB*x
}

exchange <- function(x, slope = 1/1.25) {
  slope*x
}

priceA <- function(x, pintA = 20, psA = 1.45) {
  pintA - psA*x
}

priceB <- function(x, pintB = 14.5, psB = 1.45) {
  pintB - psB*x
}

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 4, 4))

xlims <- c(0, 15)
ylims <- c(0, 21)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(3.25, 5.25, 7.25)

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


# ticksy <- seq(from = 0, to = ylims[2], by = 1)
# ylabels <- seq(from = 0, to = ylims[2], by = 1)
# ticksx <- seq(from = 0, to = xlims[2], by = 1)
# xlabels <- seq(from = 0, to = xlims[2], by = 1)
ticksy <- c(ylims[1], 4, 6.11, 8, 20, ylims[2])
ylabels <- c(NA, 4, 6.11, 8,  20, NA)
ticksx <- c(xlims[1], 5, 7.64, 10, 11,  xlims[2])
xlabels <-  c(NA, 5, 7.64, 10, 11,  NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Draw the graphs
lines(xx1, ffA(xx1), col = COLA[3], lwd = graphlinewidth)
lines(xx1, ffB(xx1), col = COLA[3], lwd = graphlinewidth)
lines(xx1, exchange(xx1), lty = 2, col = "gray", lwd = segmentlinewidth)
#lines(xx1, priceA(xx1), col = COLB[3], lwd = graphlinewidth)
#lines(xx1, priceB(xx1), col = COLB[3], lwd = graphlinewidth)


#segments(0, 8, 6.4, 8, lty = 2, col = "dark grey",  lwd = segmentlinewidth)



segments(0, 4, 5, 4, lty = 2, col = "grey",  lwd = segmentlinewidth)
segments(5, 0, 5, 4, lty = 2, col = "grey",  lwd = segmentlinewidth)
segments(0, 6.11, 7.64, 6.11, lty = 2, col = "grey",  lwd = segmentlinewidth)
segments(7.64, 0, 7.64, 6.11, lty = 2, col = "grey",  lwd = segmentlinewidth)

# segments(0, 15, 10, 0, lty = 1, col = COLA[3],  lwd = graphlinewidth)
# 
# segments(0, 4.14, 4.14, 4.14, lty = 2, col = "grey",  lwd = segmentlinewidth)
# segments(4.14, 0, 4.14, 4.14, lty = 2, col = "grey",  lwd = segmentlinewidth)

#Label Points
xpts <- c(5, 7.638888889)
ypts <- xpts/1.25
yadj1 <- 0.5
yadj2 <- -0.5

points(xpts, ypts, pch = 16, col = "black", cex = 1.5)
ptlabels <- c("g", "i")
text(xpts, ypts + yadj1, ptlabels)
#coordlabs <- c("(5,4)", "(6.44, 5.16)", "(7.64,6.11)", "(8.89,7.11)")
#text(xpts, ypts + yadj2, coordlabs, cex = smalllabelsize)

#Axis labels
mtext(expression(paste("Data entered ('000's), ", x)), side = 1, line = 2.5, cex = axislabelsize)
text(-1.5, 0.5*ylims[2], expression(paste("Graphs made, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Text
text(11, 12, expression(paste("Each graph requires")))
text(11, 11.25, expression(paste("1250 keystrokes of data")))
text(11, 10.4, expression(paste(y == frac(x,1.25) )))

#Label the curves
# text(1, 12.5, expression(p[1]), cex = labelsize)
# text(1, 19.4, expression(p[2]), cex = labelsize)

text(1, 17.5, expression(ff[A]), cex = labelsize)
text(1, 6.8, expression(ff[B]), cex = labelsize)

dev.off()
