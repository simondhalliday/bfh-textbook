require(shape)
pdf(file = "indmarketdemand/consumersurplus_market.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 6, 4, 4))

MarketD <- function(x, rmax = 20, xmax = 10, n = 10) {
  rmax - (rmax/(n*xmax))*x
}

mrsA <- function(x, rmax = 20, xmax = 10) {
  rmax - (rmax/xmax)*x
}

# uA <- function(x, y, rmax, rmax = 20, xmax = 10) {
#   y + rmax*x - (1/2)(rmax/xmax)*x^2
# }

xlims <- c(0, 115)
ylims <- c(0, 22)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(46.08, 55, 64)
b <- c(46.08, 55, 64)

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
ticksy <- c(0, 10, 20, ylims[2])
ylabels <- c(NA, expression(paste(p == 10)), expression(paste(bar(r) == 20)), NA)
ticksx <- c(0, 50, 100, xlims[2])
xlabels <- c(NA, expression(paste(X,"*") == 50), expression(paste(n*bar(x)==100)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

xpoly <- c(0, 50, 0)
ypoly <- c(10, 10, 20)
polygon(x = xpoly, y = ypoly, col = COL[4], density=NULL, border = NA)

xpoly3 <- c(0, 50, 50, 0, 0)
ypoly3 <- c(0, 0, 10, 10, 0)
polygon(x = xpoly3, y = ypoly3, col = COLB[1], density=NULL, border = NA)


#Lines for mrs graph
lines(xx1, MarketD(xx1, rmax = 20, xmax = 10, n = 10), col = COLA[4], lwd = graphlinewidth)
#lines(xx1, mrsA(xx1, rmax = 20, xmax = 10), col = COLA[4], lwd = graphlinewidth)

segments(0, 10, xlims[2], 10, lty = 1, col = COL[2] , lwd = graphlinewidth)
segments(50, 0, 50, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)

#Label axes
mtext(expression(paste("Market Quantity of fish in kilograms, ", X)), side=1, line = 2.5, cex = axislabelsize)
text(-15, 0.5*ylims[2], expression(paste("Price per kilogram, ", p[X])), xpd = TRUE, cex = axislabelsize, srt = 90) 

points(50, 10, pch = 16, col = "black", cex = 1.5)
text(52, 10.5, expression(M))

#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
text(100, 10.5, expression("Market price, p = 10"), cex = labelsize)
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

#Label mrs function
text(94, 6.8, expression(paste("Demand: ", X(p) == 100 - 5*p)))
text(94, 5.5, expression(paste("Inverse Demand: ", p(X) == 20 - frac(1,5)*X)))
#Arrows(10, 7.5, 10, 5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label satiation
#text(20, 3.5, expression(paste(bar(x) == "Point")))
#text(20, 3, expression(paste("of Satiation")))
#Arrows(20, 2.5, 20, 0.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label highest willingness to pay
#text(5, 10, expression("Consumer Surplus"))
#text(5, 9, expression(paste(CS==frac(1, 2)*bgroup("(",bar(r) - p,")")*x)))
#Arrows(5, 8.5, 5, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label highest willingness to pay
text(25, 12.25, expression("Consumer Surplus"))
text(25, 11, expression(paste(CS==frac(1, 2)*bgroup("(",bar(r) - p,")")*X, phantom()== 250)))
#text(15, 11.5, expression(paste(CS==frac(1, 2)*bgroup("(",20 - 10,")")*50, phantom()== 250)))
#Arrows(25, 15, 25, 12, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label consumer expenditure
text(25, 6, expression("Consumer Expenditure"))
text(25, 4.5, expression(paste(CE==p(X)%.%X, phantom() == 500)))

dev.off()
