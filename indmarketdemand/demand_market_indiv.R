require(shape)
pdf(file = "indmarketdemand/demand_market_indiv.pdf", width = 9, height = 7)

#Set parameters for graphics
pointsize <- 1.8
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

MarketD <- function(x, rmax = 20, xmax = 10, n = 10) {
  rmax - (rmax/(n*xmax))*x
}

mrsA <- function(x, rmax = 20, xmax = 10) {
  rmax - (rmax/xmax)*x
}

uA <- function(x, y, rmax = 20, xmax = 10) {
  y + rmax*x - (1/2)(rmax/xmax)*x^2
}

xlims <- c(0, 120)
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
ylabels <- c(NA, expression(paste(p == 10)), expression(paste(bar(p) == 20)), NA)
ticksx <- c(0, 5, 10, xlims[2])
xlabels <- c(NA, expression(5), expression(paste(10)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#xpoly <- c(0, 10, 0)
#ypoly <- c(5, 5, 10)
#polygon(x = xpoly, y = ypoly, col = COL[4], density=NULL, border = NA)

#Lines for mrs graph
#lines(xx1, MarketD(xx1, rmax = 20, xmax = 10, n = 10), col = COLA[4], lwd = graphlinewidth)
lines(xx1, mrsA(xx1, rmax = 20, xmax = 10), col = COLA[4], lwd = graphlinewidth)

segments(0, 10, xlims[2], 10, lty = 1, col = COL[2] , lwd = graphlinewidth)
segments(5, 0, 5, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)

#Label axes
#mtext(expression(paste("Quantity of fish in kilograms, ", x)), side=1, line = 2.5, cex = axislabelsize)

text(0.5*xlims[2], -2, expression(paste("Kilograms of fish, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-16, 0.5*ylims[2], expression(paste("Price per kilogram, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 

points(5, 10, pch = 16, col = "black", cex = 1.5)
text(6, 10.5, expression(i), cex = annotatesize)

#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
text(95, 10.5, expression("Market price, p = 10"), cex = labelsize)
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
text(35, 7, expression(paste("Demand: ", x(p) == 10 - frac(1,2)*p)), cex = annotatesize)
text(35, 5.5, expression(paste("Inverse Demand: ", p(x) == 20 - 2*x)), cex = annotatesize)

#Arrows(10, 7.5, 10, 5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label satiation
#text(20, 3.5, expression(paste(x[max] == "Point")))
#text(20, 3, expression(paste("of Satiation")))
#Arrows(20, 2.5, 20, 0.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label highest willingness to pay
#text(5, 10, expression("Consumer Surplus"))
#text(5, 9, expression(paste(CS==frac(1, 2)*bgroup("(",r[max] - p,")")*x)))
#Arrows(5, 8.5, 5, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

dev.off()
