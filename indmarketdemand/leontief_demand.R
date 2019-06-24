#Graph Designer: Simon Halliday
#Authors: Bowles and Halliday
#Title: Microeconomics: Competition, Conflict and Coordination

library(shape)
pdf(file = "indmarketdemand/leontief_demand.pdf", width = 8, height = 8)

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 2, 2))


demand <- function(x, a = 0.5, b = 0.5, endowment = 20, py = 2){
  (endowment*b)/(a*x) - py
}

ylims <- c(0, 6)
xlims <- c(0, 8)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(uFn(16, 2), uFn(16, 4), uFn(16, 8)) #alpha = 0.8

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

xdem <- c(4, 5, 6)
price <- c(3, 2, 4/3)
pricelab <- c(3, 2, 1.33)

# ticksy <- seq(from = 0, to = ylims[2], by = 1)
# ylabels <- seq(from = 0, to = ylims[2], by = 1)
# ticksx <- seq(from = 0, to = xlims[2], by = 2)
# xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(xlims[1], price, xlims[2])
ylabels <- c(NA, pricelab, NA)
ticksx <- c(xlims[1], xdem, xlims[2])
xlabels <- c(NA, xdem, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize, cex = axislabelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize, cex = axislabelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Draw the graphs
lines(xx1, demand(xx1), col = COLA[5], lwd = graphlinewidth)

#Axis labels
#mtext(expression(paste("Quantity of knives, ", x)), side = 1, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], ylims[1] - 0.1*ylims[2], expression(paste("Quantity of knives, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-0.7, 0.5*ylims[2], expression(paste("Price of good x, ", p[x])), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Add mrs = mrt at i
text(11, 16.75, expression(paste("for ", alpha == 0.5, " and ", m == 100)), cex = labelsize)
text(11, 15.25, expression(paste("then ", x == 5)), cex = labelsize)
text(11, 16, expression(paste("when ", p[x]== 10)), cex = labelsize)
Arrows(10, 15, 5.5, 10.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the indifference curves
text(8.1, 38, expression(u[1]^A), cex = labelsize)
text(9.5, 38, expression(u[2]^A), cex = labelsize)
text(11.3, 38, expression(u[3]^A), cex = labelsize)

#Label the feasible frontiers
text(3.9, 5.5, expression(paste("Demand for x ")), cex = labelsize)
text(3.9, 5.2, expression(paste(x(m,p[x],p[y]))), cex = labelsize)
#text(6, 5, expression(paste(x == frac(alpha*m, p[x]), ", or")), cex = labelsize)
#text(6, 4.5, expression(paste(p[x] == frac(alpha*m, x))), cex = labelsize)

#Annotate max u point on feasible frontier
#text(5.2, ppf(5) + 0.2, expression(paste(i)), cex = labelsize)


segments(0, 3, 4, 3, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(4, 0, 4, 3, lty = 2, col = "gray", lwd = segmentlinewidth)
#points(4, 3, pch = 16, col = "black", cex = 1.5)

segments(0, 2, 5, 2, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(5, 0, 5, 2, lty = 2, col = "gray", lwd = segmentlinewidth)
#points(5, 2, pch = 16, col = "black", cex = 1.5)

segments(0, price[3], xdem[3], price[3], lty = 2, col = "gray", lwd = segmentlinewidth)
segments(xdem[3], 0, xdem[3], price[3], lty = 2, col = "gray", lwd = segmentlinewidth)
points(xdem, price, pch = 16, col = "black", cex = 1.5)



# text(2.3+0.2, ppf(2.3) + 0.2, expression(paste(a)), cex = labelsize)
# segments(2.3, 0, 2.3, ppf(x = 2.3), lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, ppf(x = 2.3), 2.3, ppf(x = 2.3), lty = 2, col = "gray", lwd = segmentlinewidth)
# points(2.3, ppf(x = 2.3), pch = 16, col = "black", cex = 1.5)


# text(7.7+0.2, ppf(7.7) + 0.2, expression(paste(b)), cex = labelsize)
# segments(7.7, 0, 7.7, ppf(x = 7.7), lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, ppf(x = 7.7), 7.7, ppf(x = 7.7), lty = 2, col = "gray", lwd = segmentlinewidth)
# points(7.7, ppf(x = 7.7), pch = 16, col = "black", cex = 1.5)



dev.off()
