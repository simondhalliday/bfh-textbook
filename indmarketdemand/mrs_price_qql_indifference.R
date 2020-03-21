require(shape)
pdf(file = "indmarketdemand/mrs_price_qql_indiff.pdf", width = 9, height = 14)

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 6, 0.2, 0.2), mfrow = c(2,1))



# demandcd ----------------------------------------------------------------


#uA <- function(x, y, alpha = 0.5) {
  #x^(alpha)*y^(1 - alpha)
#}


uA <- function(x, y) {
  y + 20*x - x^2
}


bcA <- function(x, w = 20, p = 2) {
  w - p*x
}


xlims <- c(0, 10.5)
ylims <- c(520, 650)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(uA(x = 3, y = 600 - 3*14), uA(x = 5, y = 600 - 5*10), uA(x = 7, y = 600 - 7*6))

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs = "i", 
     yaxs = "i")

#a <- c(uA(x = 3, y = 600 - 3*14), uA(x = 5, y = 600 - 5*10), uA(x = 7, y = 600 - 7*6))

ticksy <- c(0, 600 - 3*14, 600 - 5*10, 600 - 7*6, 600, ylims[2])
ylabels <- c(NA, 600 - 3*14, 600 - 5*10, 600 - 7*6, 600, NA)
ticksx <- c(0, 3, 5, 7, 10, xlims[2])
xlabels <- c(NA, expression(paste(x[H] == 3)), expression(paste(x,"*" == 5)), expression(paste(x[L] == 7)), expression(paste(bar(x) == 10)), NA)


npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, bcA(xx1, w = 600, p = 6), col = COLA[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 600, p = 10), col = COLA[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 600, p = 14), col = COLA[3], lwd = graphlinewidth)
#lines(xx1, indiffA1(xx1, uA = 20, rmax = 2.5, xmax = 10), col = COLB[4], lwd = graphlinewidth)


#Label Axes
#mtext(expression(paste("Kilograms of coffee, ", x)), side=1, line = 2.5, cex = axislabelsize)

#text(0.5*xlims[2], -2, expression(paste("Kilograms of coffee, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-15, 0.5*ylims[2], expression(paste("Money left over, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#segments(0, 6, 6, 6, lty = 2, col = "gray" , lwd = segmentlinewidth, xpd = TRUE)
#segments(2, -10, 2, 6, lty = 2, col = "gray" , lwd = segmentlinewidth, xpd = TRUE)
#segments(4, -10, 4, 6, lty = 2, col = "gray" , lwd = segmentlinewidth, xpd = TRUE)
#segments(6, -10, 6, 6, lty = 2, col = "gray" , lwd = segmentlinewidth, xpd = TRUE)

#Label the budget curve functions for the HG, Aisha
#text(3.5, 0.5, expression(bc[1]), cex = annotatesize)
#text(7.2, 0.5, expression(bc[2]), cex = annotatesize)
#text(11.1, 0.5, expression(bc[3]), cex = annotatesize)




#Label the iso-welfare functions for the HG, Aisha
text(105, 40, expression(u[1]), cex = annotatesize)
text(105, 110, expression(u[2]), cex = annotatesize)
text(105, 160, expression(u[3]), cex = annotatesize)
#text(6.6, 8.3, expression(u[4]^A))

contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLB[3],
        lwd = graphlinewidth,
        levels = a, 
        xaxs = "i", 
        yaxs = "i", 
        add = TRUE)

#abline(h=6, col=COL[3], lwd=graphlinewidth)


points(x = 3, y = 600 - 3*14, pch = 16, col = "black", cex = 1.5)
text(x = 3 - 0.2, y = 600 - 3*14 - 3, expression(a), cex = annotatesize)
points(x = 5, y = 600 - 5*10, pch = 16, col = "black", cex = 1.5)
text(x = 5 - 0.2, y = 600 - 5*10 - 3, expression(b), cex = annotatesize)
points(x = 7, y = 600 - 7*6, pch = 16, col = "black", cex = 1.5)
text(x = 7 - 0.2, y = 600 - 7*6 - 3, expression(c), cex = annotatesize)

#Arrows(8.5, 6, 6.5, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
#text(9.7, 6.3, expression(paste("Price-offer curve")), cex = annotatesize)

axis(1, at = ticksx, pos = ylims[1], labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#------


mrsA <- function(x, rmax = 10, xmax = 20) {
  rmax - (rmax/xmax)*x
}

# uA <- function(x, y, rmax, rmax = 10, xmas = 20) {
#   y + rmax*x - (1/2)(rmax/xmax)*x^2
# }

xlims <- c(0, 10.5)
ylims <- c(0, 22)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

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
ticksy <- c(0, 6, 10, 14, 20, ylims[2])
ylabels <- c(NA, expression(paste(p[L])), expression(paste(p,"*")), expression(paste(p[H])), expression(paste(bar(p) == 20)), NA)
ticksx <- c(0, 3, 5, 7, 10, xlims[2])
xlabels <- c(NA, expression(paste(x[H] == 3)), expression(paste(x,"*" == 5)), expression(paste(x[L] == 7)), expression(paste(bar(x) == 10)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#xpoly <- c(0, 10, 0)
#ypoly <- c(5, 5, 10)
#polygon(x = xpoly, y = ypoly, col = COL[4], density=NULL, border = NA)

lines(xx1, mrsA(xx1, rmax = 20, xmax = 10), col = COLA[4], lwd = graphlinewidth)
#lines(xx1, brfB(xx1, alpha = 16, beta = 1/24), col = COLB[4], lwd = graphlinewidth)




#mtext(expression(paste("Quantity of the good, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], -2, expression(paste("Quantity of the good, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-1.2, 0.5*ylims[2], expression(paste("Price per unit of the good, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 


segments(0, 10, xlims[2], 10, lty = 1, col = COLB[4] , lwd = graphlinewidth)
segments(5, 0, 5, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)
text(10, 10.6, expression(paste("Base price, ", p,"*" == 10)), cex = labelsize)
points(5, 10, pch = 16, col = "black", cex = 1.5)
text(5.25, 10.5, expression(e), cex = annotatesize)

segments(0, 14, xlims[2], 14, lty = 1, col = COLB[4] , lwd = graphlinewidth)
segments(3, 0, 3, 14, lty = 2, col = "gray" , lwd = segmentlinewidth)
text(10, 14.6, expression(paste("High price, ", p[H] == 14)), cex = labelsize)
points(3, 14, pch = 16, col = "black", cex = 1.5)
text(3.25, 14.5, expression(f), cex = annotatesize)


segments(0, 6, xlims[2], 6, lty = 1, col = COLB[4] , lwd = graphlinewidth)
segments(7, 0, 7, 6, lty = 2, col = "gray" , lwd = segmentlinewidth)
text(10, 6.6, expression(paste("Low price, ", p[L] == 6)), cex = labelsize)
points(7, 6, pch = 16, col = "black", cex = 1.5)
text(7.25, 6.5, expression(g), cex = annotatesize)


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
text(2.5, 19, expression(paste(mrs(x,y) == 20 - 2*x)), cex = labelsize)
#Arrows(10, 7.5, 10, 5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label satiation
#text(20, 3.5, expression(paste(bar(x) == "Point")))
#text(20, 3, expression(paste("of Satiation")))
#Arrows(20, 2.5, 20, 0.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label highest willingness to pay
#text(5, 10, expression("Consumer Surplus"))
#text(5, 9, expression(paste(CS==frac(1, 2)*bgroup("(",bar(r) - p,")")*x)))
#Arrows(5, 8.5, 5, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

dev.off()
