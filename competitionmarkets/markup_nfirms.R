require(shape)
pdf(file = "competitionmarkets/markup_nfirms.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(5, 5, 4, 4))

marketProfit <- function(n, s = 0.5, pmax = 20, c1 = 2) {
  n*(1/(n+1)^2)*(pmax - c1)^2/(s)
}

consumerSurplus <- function(n, s = 0.5, pmax = 20, c1 = 2) {
  (1/2)*(pmax - (c1 + ((1/(n+1))*(pmax -c1)  )) )*(n/(n+1))*( (pmax - c1)/(s))
}

markUp <- function(n, pmax = 20, c1 = 2) {
  c1 + (1/(n+1))*(pmax - c1)
}


xlims <- c(0, 40)
ylims <- c(0, 12)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(46.08, 55, 64)
b <- c(46.08, 55, 64)


#B's value when at A's bliss point
#0.35*log(5.88) + 0.35*log(8.88) + 0.5*log(10 - 5.88) + 0.5*log(15 - 8.88) 

#B's bliss point x = 4.11765; y = 6.17647
#A's value when at A's bliss point
#0.5*log(4.11765) + 0.5*log(6.17647) + 0.35*log(10 - 4.11765) + 0.35*log(15 - 6.17647) 


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

ticksy <- seq(from = 0, to = ylims[2], by = 2)
ylabels <- seq(from = 0, to = ylims[2], by = 2)
ticksx <- seq(from = 0, to = xlims[2], by = 10)
xlabels <- seq(from = 0, to = xlims[2], by = 10)
# ticksy <- c(0, ylims[2])
# ylabels <- c(NA, NA)
# ticksx <- c(0, xlims[2])
# xlabels <- c(NA, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)

#text(x = c(0, 12, 18, 36, xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE)

axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(1, xlims[2], length.out = npts)

lines(xx2, markUp(xx2, pmax = 20, c1 = 2), col = COLA[4], lwd = graphlinewidth)
#lines(xx2, marketProfit(xx2, s = 1/2, pmax = 20, c1 = 2), col = COLB[4], lwd = graphlinewidth)

segments(0, 2, xlims[2], 2, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(12, 0, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)

#mtext(expression(paste("A's output, ", x^A)), side=1, line = 2.5, cex = axislabelsize)
text(0.5*(xlims[2]), -1.2, expression(paste("Number of firms, ", n)), xpd = TRUE, cex = axislabelsize) 
text(-3, 0.5*ylims[2], expression(paste("Market price and mark-up, $")), xpd = TRUE, cex = axislabelsize, srt = 90) 

#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
#text(7.3, 3, expression("Pareto Efficient"))
#text(7.3, 2.5, expression("Curve"))
#Arrows(7.3, 3.5, 7.3, 6.1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the iso-welfare functions for the HG, Aisha
# text(3.8, 1.5, expression(u[1]^A))
# text(4.6, 1.5, expression(u[2]^A))
# text(5.5, 1.5, expression(u[3]^A))
#text(6.6, 8.3, expression(u[4]^A))

#Label the indifference curves for the HG, Betty
# text(7.6, 17, expression(u[1]^B))
# text(6.75, 17, expression(u[2]^B))
# text(6, 17, expression(u[3]^B))
#text(3.4, 6.9, expression(v[4]^B))

#Label point i. 
# points(12, 12, pch = 16, col = "black", cex = 1.5)
# text(16.5, 12.5, expression(paste("Nash Equilibrium")))
# text(11.3, 11.3, expression(paste("n")))

#Annotate Pareto Efficient Curve and relevant points
# segments(8, 6, 6, 8, lty = 1, col = COL[2] , lwd = graphlinewidth)
# points(6, 8, pch = 16, col = "black", cex = 1.5)
# text(6, 8.5, expression(paste("g")))
# 
# points(8, 6, pch = 16, col = "black", cex = 1.5)
# text(7, 7.5, expression(paste("i")))
# 
# points(7, 7, pch = 16, col = "black", cex = 1.5)
# text(8, 6.5, expression(paste("f")))

#points(5.84, 8.77, pch = 16, col = "black", cex = 1.5)

#Mark-up
text(3.5, 3.25, expression(paste("Mark-up")), cex = labelsize)
#text(14, 6, expression(paste("function")))
Arrows(6, 2.3, 6, 4.2, col = "black", lty = 1, code = 3, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label price
text(35, 4, expression(paste("Price")), cex = labelsize)
text(35, 3.2, expression(paste(p == c[1] + frac(1, n + 1)*(bar(p) - c[1]))))

text(35, 1.5, expression(paste("Marginal cost")), cex = labelsize)
text(35, 0.9, expression(paste(mc == c[1])))




dev.off()
