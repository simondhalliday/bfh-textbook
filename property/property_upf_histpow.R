require(ggplot2)
require(shape)
require(plotrix)
pdf(file = "bfh-textbook/property/property_upf_histpow.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 4
arrowwidth <- 0.5

upf <- function(ub) {
  12.24745 - ub
}


#Notes
#ua = (xa^0.5)*(ya^0.5)
#ub = ((10-xa)^05)*((15-ya)^0.5)

#ua = (10^0.5)*(15^0.5) = 12.24745
#ua_e = u(8,2) = (8^0.5)*(2^0.5) = 4

#Point g
# => x^A = 4/((3/2)^0.5) = 3.27 => x^B = 6.73
# => y^A = 3/2(x^A) = 4.9 => y^B = 10.1
#ua_g(4.9^0.5)*(3.27^0.5) = 4.002874
#ub_g(6.73^0.5)*(10.1^0.5) = 8.244574
#W = (4.002874^0.5)*(8.244574^0.5) = 5.744736

COL <- c("#1B9E77", "#D95F02", "#fbb4ae", "#b3cde3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
#COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")
par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 13)
ylims <- c(0, 13)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("B's Utility, ", u^B)),
     ylab = expression(paste("A's Utility, ", u^A)),
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(xlims[1], xlims[2], length.out = npts2)

#polygons
#I need something like xx1 with npts for 
xpoly1 <- c(5.12, 8.245, 5.12, 5.12)
ypoly1 <- c(4, 4, 7.12, 4)
polygon(x = xpoly1, y = ypoly1, col=COL[4], density=NULL, border = NA)

#xpoly2 = c(0, 0, 12.24745, xlims[2], xlims[2], 0)
#ypoly2 = c(ylims[2], 12.24745, 0, 0, ylims[2], ylims[2])
#polygon(x = xpoly2, y = ypoly2, col=COL[4], density=NULL, border = NA)



#Draw the lines for the graphs
lines(xx1, upf(xx1), col = COL[1], lwd = graphlinewidth)

#Customize ticks and labels for the plot
ticksy <- seq(from = ylims[1], to = ylims[2], by = 1)
ylabels <- seq(from = ylims[1], to = ylims[2], by = 1)
ticksx <- seq(from =  xlims[1], to = xlims[2], by = 1)
xlabels <- seq(from = xlims[1], to = xlims[2], by = 1)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Annotation of the  graphs
#UPF
text(8.8, 1.5, expression(paste("Utility Possibilities Frontier")))
text(8.8, 1, expression(paste(u^A == bar(W) - u^B)))

#SWF
#text(11.5, 4.5, expression(paste("Social Planner's")))
#text(11.5, 4, expression(paste("Iso-welfare Curves")))

#Annotate Point f
#u^A = (5.84^0.5)*(8.77^0.5) = 7.16
#u^B = (4.16^0.5)*(6.23^0.5) = 5.09
#W = (7.16^0.5)*(5.09^0.5) = 6.03692

#points(5.09, 7.15, pch = 16, col = "black", cex = 1.5)
#text(5.3, 7.3, expression(f))


#Annotate Point i
#(5^0.5)*(7.5^0.5) = 6.123724
#W = (6.123724^0.5)(6.123724^0.5) = 6.123724
#points(6.123724, 6.123724, pch = 16, col = "black", cex = 1.5)
#text(6.3, 6.3, expression(i))

#Annotate point g
#ua_g(4.9^0.5)*(3.27^0.5) = 4.002874
#ub_g(6.73^0.5)*(10.1^0.5) = 8.244574
#W = (4.002874^0.5)*(8.244574^0.5) = 5.744736
#points(8.24, 4, pch = 16, col = "black", cex = 1.5)
#text(8.4, 4.2, expression(g))

#Annotate point e.
#ua_e = (8^0.5)*(2^0.5) = 4
#ub_e = (2^0.5)*(13.1^0.5) = 5.118594
#W = (4^0.5)*(5.118594^0.5) = 4.524862
segments(0, 4, 13, 4, lty = 2, col = "darkgray", lwd = 2)
segments(5.12, 0, 5.12, 13, lty = 2, col = "darkgray", lwd = 2)
#Annotate e
points(5.12, 4, pch = 16, col = "black", cex = 1.5)
text(4.92, 3.8, expression(e))

#Annotate f
points(5.12, 7.12, pch = 16, col = "black", cex = 1.5)
text(5.3, 7.3, expression(f))
#Annotate g
points(8.24, 4, pch = 16, col = "black", cex = 1.5)
text(8.4, 4.2, expression(g))

#Label economic rents
#Arrow to Slope of BRF
Arrows(6.68, 5.12, 6.68, 7.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = arrowwidth)
text(6.68, 7.82, expression(paste("Economic Rents")))

#Point h
#points(4, 10, pch = 16, col = "black", cex = 1.5)
#u^A_h(6, 5) = (6^0.5)*(5^0.5) = 5.477226
#u^A_h(4,10) = (4^0.5)*(10^0.5) = 6.324555
points(5.47, 6.32, pch = 16, col = "black", cex = 1.5)
text(5.32, 6.12, expression(h))

#Label Participation Constraints
#Aisha's
text(11, 4.2, expression(paste("Aisha's Participation Constraint")))

#Betty's
text(7.1, 12.5, expression(paste("Betty's Participation Constraint")))


dev.off()

