require(shape)
pdf(file = "competitionmarkets/competition_average_2.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(4, 6, 1, 1))

piA <- function(xbar, x, s = 0.5, pmax = 20, c1 = 2, n = 3) {
  (pmax - s*(n - 1)*xbar)*x - s*(x)^2 - c1*x
}

brfA <- function(xbar, s = 0.5, pmax = 20, c1 = 2, n = 3) {
  (pmax - c1)/(2*s) - ((n - 1)/(2))*xbar
}

Equal45 <- function(xbar, slope = 1){
  slope*xbar
}


xlims <- c(0, 19)
ylims <- c(0, 19)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(34, 40.5, 54)
# a <- c(72, 81, 95)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs = "i", 
     yaxs = "i")


ticksy <- c(0, 9, 18, ylims[2])
ylabels <- c(NA, expression(paste(x^N) == 9), expression(x == 18), NA)
ticksx <- c(0, 9, 18, xlims[2])
xlabels <- c(NA, expression(paste(bar(x)^{N} == 9)), expression(x == 18), NA)


axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)


npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(xlims[1], 8.3, length.out = npts)

lines(xx1, brfA(xx1, s = 0.5, pmax = 20, c1 = 2, n = 3), col = COLA[4], lwd = graphlinewidth)
lines(xx1, Equal45(xx1, slope = 1), col = COLB[4], lwd = graphlinewidth)

contour(x, y,
        outer(x, y, piA),
        drawlabels = FALSE,
        col = COLA[2],
        lwd = graphlinewidth,
        levels = a,
        xaxs = "i",
        yaxs = "i",
        add = TRUE)

mtext(expression(paste("Average output of other firms, ", bar(x))), side = 1, line = 3, cex = axislabelsize)
text(-2.5, 0.5*ylims[2], expression(paste("The firm's output, ", x)), xpd = TRUE, cex = axislabelsize, srt = 90) 

# segments(0, 6, 6, 6, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(6, 0, 6, 6, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(6, 6, pch = 16, col = "black", cex = 1.5)
text(5.8, 6.5, expression(paste("i")), cex = labelsize)
 
segments(9, 0, 9, 9, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 9, 9, 9, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(9, 9, pch = 16, col = "black", cex = 1.5)
text(8.7, 9.8, expression(paste("n")), cex = labelsize)


text(10.25, brfA(xbar = 9.75) + 0.3, expression(paste("h")), cex = labelsize)
segments(9.75, brfA(xbar = 9.75) - 2, 9.75, brfA(xbar = 9.75) + 2, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(9.75, brfA(xbar = 9.75), pch = 16, col = "black", cex = 1.5)

text(9.75, 3.5, expression(paste("Best response is")), cex = labelsize)
text(9.75, 2.7, expression(paste("where isoprofit")),cex = labelsize)
text(9.75, 1.9, expression(paste("is vertical")),cex = labelsize)
Arrows(9.75, 4, 9.75, 6.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

Arrows(15.5, 13, 15.5, 15, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(15.5, 12.5, expression(paste(x == bar(x))), cex = labelsize)
text(15.5, 11.7, expression(paste("The firm's output")),cex = labelsize, xpd = TRUE)
text(15.5, 10.9, expression(paste("equals average ouput")),cex = labelsize, xpd = TRUE)
text(15.5, 10.1, expression(paste("of other firms")),cex = labelsize, xpd = TRUE)


text(2.5, 13.1 - 0.8, expression(paste("The firm's")),cex = labelsize)
text(2.5, 12.3 - 0.8, expression(paste("best-response")),cex = labelsize)
text(2.5, 11.5 - 0.8, expression(paste("function")),cex = labelsize)
Arrows(2.5, 13, 2.5, 14.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#Label the iso-welfare functions for the HG, Aisha
text(5.4, 18, expression(pi[1]), cex = labelsize)
text(6.4, 18, expression(pi[2]), cex = labelsize)
text(7.6, 18, expression(pi[3]), cex = labelsize)

text(11, 18.8, expression("The firm's"),cex = labelsize, xpd = TRUE)
text(11, 18, expression("isoprofit curves"),cex = labelsize,xpd = TRUE)
text(11, 17.2, expression(paste(pi[3] > pi[2], phantom() > pi[1])),cex = labelsize)

#B's brf
# text(7, 30, expression(paste("A's best response")),cex = labelsize)
# text(7, 29, expression(paste("function")),cex = labelsize)
# Arrows(7, 28.2, 7, 23.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


dev.off()
