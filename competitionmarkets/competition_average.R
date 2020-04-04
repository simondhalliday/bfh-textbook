require(shape)
pdf(file = "competitionmarkets/competition_average.pdf", width = 9, height = 7)

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

piA <- function(xbar, x, s = 0.5, pmax = 20, c1 = 2, n = 5) {
  (pmax - s*(n - 1)*xbar)*x - s*(x)^2 - c1*x
}

brfA <- function(xbar, s = 0.5, pmax = 20, c1 = 2, n = 5) {
  (pmax - c1)/(2*s) - ((n-1)/(2))*xbar
}

Equal45 <- function(xbar, slope = 1){
  slope*xbar
}

xANEcomp <- function(pbar, c1 = 2, n = 8, beta = 0.5){
  (1/(n+1))*(pbar - c1)/beta
}


xlims <- c(0, 19)
ylims <- c(0, 19)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(2, 18, 32.5)
b <- c(60, 72, 81)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

ticksy <- c(0, 3.7, 6, ylims[2])
ylabels <- c(NA, expression(paste(x,"*") == 4), expression(paste(x^N) == 6), NA)
ticksx <- c(0, 3.7, 6, xlims[2])
xlabels <- c(NA, expression(paste(bar(x),"*" == 4)), expression(paste(bar(x)^{N} == 6)), NA)


axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, brfA(xx1, s = 0.5, pmax = 20, c1 = 2, n = 5), col = COLA[4], lwd = graphlinewidth)
lines(xx1, Equal45(xx1, slope = 1), col = COLB[4], lwd = graphlinewidth)

contour(x, y,
        outer(x, y, piA),
        #labels = c("v1", "v2", "v3"),
        drawlabels = FALSE,
        col = COLA[2],
        #xlab = expression(paste("A's Apples, ", x)),
        #ylab = expression(paste("A's Oranges, ", y)),
        #cex.lab = axislabelsize,
        lwd = graphlinewidth,
        levels = a,
        xaxs="i",
        yaxs="i",
        add = TRUE)

mtext(expression(paste("Average output of other firms, ", bar(x))), side=1, line = 3, cex = axislabelsize)
text(-2.5, 0.5*ylims[2], expression(paste("Typical firm's output, ", x)), xpd = TRUE, cex = axislabelsize, srt = 90) 




segments(0, 6, 6, 6, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(6, 0, 6, 8, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(6, 6, pch = 16, col = "black", cex = 1.5)
text(6.5, 6, expression(paste(n,"'")), cex = labelsize)
segments(0, 3.7, 3.7, 3.7, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(3.7, 0, 3.7, 3.7, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(3.7, 3.7, pch = 16, col = "black", cex = 1.5)
text(4.2, 3.6, expression(paste(i,"'")), cex = labelsize)
segments(8, brfA(xbar = 8) - 2, 8, brfA(xbar = 8) + 2, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(8, brfA(xbar = 8), pch = 16, col = "black", cex = 1.5)
text(8.5, brfA(xbar = 8), expression(paste(h,"'")), cex = labelsize)


text(14, brfA(xbar = 8) + 0.8, expression(paste("Isoprofit vertical")), cex = annotatesize)
text(14, brfA(xbar = 8), expression(paste("at intersection")), cex = annotatesize)
text(14, brfA(xbar = 8) - 0.8, expression(paste("with best response")), cex = annotatesize)
Arrows(11, brfA(xbar = 8), 9.5, brfA(xbar = 8), col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

Arrows(13.5, 10.9, 13.5, 13, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(13.5, 10.5, expression(paste(x == bar(x))), cex = annotatesize)
text(13.5, 9.7, expression(paste("Typical firm's output")), cex = annotatesize)
text(13.5, 8.9, expression(paste("equals average ouput")), cex = annotatesize)
text(13.5, 8.1, expression(paste("of other firms")), cex = annotatesize)

text(2.1, 9, expression(paste("Typical firm's")), cex = annotatesize)
text(2.1, 8.2, expression(paste("best-response")), cex = annotatesize)
text(2.1, 7.4, expression(paste("function")), cex = annotatesize)
Arrows(2, 9.4, 2, 13, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#Label the iso-welfare functions for the HG, Aisha
text(7, 9, expression(pi[1]), cex = annotatesize)
text(6.1, 9, expression(pi[2]), cex = annotatesize)
text(5.3, 9, expression(pi[3]), cex = annotatesize)
text(7.1, 18, expression("Typical firm's"), cex = annotatesize)
text(7.1, 17.2, expression("isoprofit curves"), cex = annotatesize)
text(7.1, 16.4, expression(paste(pi[3] > pi[2], phantom() > pi[1])), cex = annotatesize)


#B's brf
text(7, 30, expression(paste("A's best response")), cex = annotatesize)
text(7, 29, expression(paste("function")), cex = annotatesize)
Arrows(7, 28.2, 7, 23.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


dev.off()
