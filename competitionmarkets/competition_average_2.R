require(shape)
pdf(file = "competitionmarkets/competition_average_2.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(4, 6, 4, 6))

piA <- function(xbar, x, s = 0.5, pmax = 20, c1 = 2, n = 2) {
  (pmax - s*(n - 1)*xbar)*x - s*(x)^2 - c1*x
}

brfA <- function(xbar, s = 0.5, pmax = 20, c1 = 2, n = 2) {
  (pmax - c1)/(2*s) - ((n-1)/(2))*xbar
}

Equal45 <- function(xbar, slope = 1){
  slope*xbar
}



xlims <- c(0, 19)
ylims <- c(0, 19)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
#a <- c(30, 35, 40)
a <- c(60, 72, 81)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
# ticksx <- seq(from = 0, to = xlims[2], by = 2)
# xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(0, 9, 12, ylims[2])
ylabels <- c(NA, expression(paste(x,"*")), expression(paste(x^N)), NA)
ticksx <- c(0, 9, 12, xlims[2])
xlabels <- c(NA, expression(paste(bar(x),"*")), expression(paste(bar(x)^{N})), NA)


axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, brfA(xx1, s = 0.5, pmax = 20, c1 = 2, n = 2), col = COLA[4], lwd = graphlinewidth)
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

mtext(expression(paste("Average output of other firms, ", bar(x))), side=1, line = 2.5, cex = axislabelsize)
text(-2, 0.5*ylims[2], expression(paste("Typical firm's output, ", x)), xpd = TRUE, cex = axislabelsize, srt = 90) 




#Add arrows:
#arrows(-0.8, 10, -0.8, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
#arrows(6.2, -1.7, 9, -1.7, xpd = TRUE, length=0.1,angle=40,lwd=3)

# segments(0, 6, 6, 6, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(6, 0, 6, 8, lty = 2, col = "gray" , lwd = segmentlinewidth)
# points(6, 6, pch = 16, col = "black", cex = 1.5)
# text(6.2, 6, expression(paste(n)))

segments(0, 9, 9, 9, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(9, 0, 9, 9, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(9, 9, pch = 16, col = "black", cex = 1.5)
text(9.5, 9, expression(paste("i")))
#segments(0, 4, 7, 4, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(12, 0, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 12, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(12, 12, pch = 16, col = "black", cex = 1.5)
text(12.5, 12.1, expression(paste("n")))


text(14.5, brfA(xbar = 14.1) + 0.2, expression(paste("h")))
segments(14.1, brfA(xbar = 14.1) - 2, 14.1, brfA(xbar = 14.1) + 2, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(14.1, brfA(xbar = 14.1), pch = 16, col = "black", cex = 1.5)

text(14.1, 4.7, expression(paste("Iso-profit vertical")))
text(14.1, 4, expression(paste("at intersection")))
text(14.1, 3.3, expression(paste("with best response")))
Arrows(14.1, 5.2, 14.1, 8.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

Arrows(16.5, 14.9, 16.5, 16, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(16.5, 14.5, expression(paste(x == bar(x))))
text(16.5, 13.7, expression(paste("Typical firm's output")))
text(16.5, 12.9, expression(paste("equals average ouput")))
text(16.5, 12.1, expression(paste("of other firms")))


text(3, 13, expression(paste("Typical firm's")))
text(3, 12.3, expression(paste("best-response function")))
Arrows(3, 13.4, 3, 16, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#Label the iso-welfare functions for the HG, Aisha
text(11, 18, expression(pi[1]))
text(9.5, 18, expression(pi[2]))
text(8.5, 18, expression(pi[3]))

text(14, 18.7, expression("Typical firm's"))
text(14, 18, expression("iso-profit curves"))
text(14, 17.3, expression(paste(pi[3] > pi[2], phantom() > pi[1])))
#text(6.6, 8.3, expression(u[4]^A))

#Label the indifference curves for the HG, Betty
# text(8.9, 19, expression(pi[1]^B))
# text(7.9, 19, expression(pi[2]^B))
# text(7, 19, expression(pi[3]^B))
#text(3.4, 6.9, expression(v[4]^B))

# #Label Nash Equilibrium 
# segments(0, 12, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(12, 0, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)



#B's brf
text(7, 30, expression(paste("A's best response")))
text(7, 29, expression(paste("function")))
Arrows(7, 28.2, 7, 23.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


dev.off()
