require(ggplot2)
require(shape)
require(plotrix)
#pdf(file = "property_fig8STEP1.pdf", width = 9, height = 7)
#pdf(file = "property_fig8STEP2.pdf", width = 9, height = 7)
pdf(file = "property/property_fig8.pdf", width = 9, height = 7)

#Set parameters for graphics
namesize <- 1.3
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

upf <- function(ub) {
  12.24745 - ub
}

#Welfare at z: (((9^0.5)*(1^0.5))^0.5)*(((1^0.5)*(14^0.5))^0.5)

swf.z <- function(ub, a = 0.5, W = 3.350369) {
  (((W)*(1/ub)^a)^(1/(1-a)))
}

uAlog <- function(xA, yA, alpha = 1/2){
  alpha*log(xA) + (1-alpha)*log(yA)
}

#Welfare at h: (((7^0.5)*(1.3^0.5))^0.5)*(((3^0.5)*(13.7^0.5))^0.5)
swf.h <- function(ub, a = 0.5, W = 4.397651) {
  (((W)*(1/ub)^a)^(1/(1-a)))
}

swf.f <- function(ub, a = 0.5, W = 5.612486) {
  (((W)*(1/ub)^a)^(1/(1-a)))
}


swf.g <- function(ub, a = 0.5, W = 5.2671) {
  (((W)*(1/ub)^a)^(1/(1-a)))
}

swf.i <- function(ub, a = 0.5, W = 6.123724) {
  (((W)*(1/ub)^a)^(1/(1-a)))
}


#W = (^0.6)(^0.4)


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

COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7")


#COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")
par(mar =  c(4, 4, 1, 2))
xlims <- c(0, 13)
ylims <- c(0, 13)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = "",
     ylab = "",
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
xx4 <- seq(xlims[1], 13, length.out = npts2)

text(0.5*xlims[2], -1.3, expression(paste("A's Utility, ", u^A)), xpd = TRUE, cex = axislabelsize) 
text(-0.9, 0.5*ylims[2], expression(paste("B's Utility, ", u^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 


xpoly <- c(0, 12.24745, 0, 0)
ypoly <- c(0,0,12.24745,0)
polygon(x = xpoly, y = ypoly, col = "#e0f3db", density=NULL, border = NA)



#Draw the lines for the graphs
lines(xx1, upf(xx1), col = CBCols[1], lwd = graphlinewidth)
lines(xx1, swf.h(xx1), col = CBCols[2], lwd = graphlinewidth, lty = 1)
lines(xx1, swf.z(xx1), col = CBCols[2], lwd = graphlinewidth, lty = 1)
lines(xx1, swf.f(xx1), col = CBCols[2], lwd = graphlinewidth, lty = 1)
lines(xx1, swf.g(xx1), col = CBCols[2], lwd = graphlinewidth, lty = 1)
lines(xx1, swf.i(xx1), col = CBCols[2], lwd = graphlinewidth)
lines(xx1, swf.i(xx1, W = 7.5), col = CBCols[2], lwd = graphlinewidth)
#lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 4)
#lines(xx2, solowInfeas(xx2, delta = 5), col = COL[1], lwd = 4, lty = 2)

#Customize ticks and labels for the plot
ticksy <- seq(from = ylims[1], to = ylims[2], by = 1)
ylabels <- seq(from = ylims[1], to = ylims[2], by = 1)
ticksx <- seq(from =  xlims[1], to = xlims[2], by = 1)
xlabels <- seq(from = xlims[1], to = xlims[2], by = 1)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#Annotation of the  graphs
#UPF
text(9.1, 0.5, expression(paste("Utility possibilities frontier")), cex = annotatesize)
#text(8.1, 0.8, expression(paste(u^B == (10^0.5)*(15^0.5) - u^A)), cex = annotatesize)

#Iso-welfare curve labels
text(10.5, 7.6, expression(paste("Impartial Spectator's")), cex = annotatesize)
text(10.5, 7, expression(paste("iso-social")), cex = annotatesize)
text(10.5, 6.4, expression(paste("welfare curves")), cex = annotatesize)

#Annotate point g
#ua_g(2.44949^0.5)*(((3/2)*2.44949)^0.5) = 3
#ub_g((10 - 2.44949)^0.5)*(15 - (3/2)*2.44949)^0.5) = 9.247448
#W = (3^0.5)*(9.247448^0.5) = 5.2671
#points(8.24, 4, pch = 16, col = "black", cex = 1.5)
points(3, 9.247448, pch = 16, col = "black", cex = 1.5)
text(3 - 0.3, 9.247448 - 0.3, expression(t^B), cex = annotatesize)


#Annotate Point i
#(5^0.5)*(7.5^0.5) = 6.123724
#W = (6.123724^0.6)*(6.123724^0.4) = 6.123724

points(6.123724, 6.123724, pch = 16, col = "black", cex = 1.5)
text(6.3, 6.3, expression(i), cex = annotatesize)

#Annotate point f
#ua_f(6.95^0.5)*(10.45^0.5) = 8.573214
#ub_f(3^0.5)*(4.5^0.5) = 3.674235
#W = (8.573214^0.5)*(3.674235^0.5) = 5.744736
#W = (4.002874^0.6)*(8.244574^0.4) = 5.344295
points(8.522177, (1^0.5)*(14^0.5), pch = 16, col = "black", cex = 1.5)
text(8.522177 + 0.3, (1^0.5)*(14^0.5) + 0.2, expression(t^A), cex = annotatesize)

#Annotate point e.
#ua_e = (8^0.5)*(2^0.5) = 4
#ub_e = (2^0.5)*(13.1^0.5) = 5.118594
#W = (4^0.5)*(5.118594^0.5) = 4.524862
#W = (4^0.6)*(5.118594^0.4) = 4.41465
# For old endowment, 8,2 and 2, 13 
# points(5.12, 4, pch = 16, col = "black", cex = 1.5)
# text(4.9, 3.8, expression(z), cex = annotatesize)
#For new endowment, 9,1 and 1, 14 
points((9^0.5)*(1^0.5), (1^0.5)*(14^0.5), pch = 16, col = "black", cex = 1.5)
text((9^0.5)*(1^0.5) - 0.3, (1^0.5)*(14^0.5) - 0.3, 
     expression(z), cex = annotatesize)

points(3, (3^0.5)*(13.7^0.5), pch = 16, col = "black", cex = 1.5)
text(3 - 0.3, (3^0.5)*(13.7^0.5) - 0.3, 
     expression(h), cex = annotatesize)

text(13.3, 0.8, expression(w[1]), cex = annotatesize, xpd = TRUE)
text(13.3, 1.4, expression(w[2]), cex = annotatesize, xpd = TRUE)
text(13.3, 1.95, expression(w[3]), cex = annotatesize, xpd = TRUE)
text(13.3, 2.4, expression(w[4]), cex = annotatesize, xpd = TRUE)
text(13.3, 2.8, expression(w[5]), cex = annotatesize, xpd = TRUE)
text(13.3, 4.2, expression(w[6]), cex = annotatesize, xpd = TRUE)

text(2, 2.5, expression("Feasible"), cex = annotatesize, xpd = TRUE)
text(2, 2, expression("combinations"), cex = annotatesize, xpd = TRUE)
text(2, 1.5, expression("of utility"), cex = annotatesize, xpd = TRUE)

text(12, 12.5, expression("Infeasible"), cex = annotatesize, xpd = TRUE)
text(12, 12, expression("combinations"), cex = annotatesize, xpd = TRUE)
text(12, 11.5, expression("of utility"), cex = annotatesize, xpd = TRUE)

text(6.123724, 7.7, expression(mrs == mrt), cex = annotatesize)
Arrows(6.123724, 7.5, 6.123724, 6.123724 + 0.4, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


# For guidance when drawing the figures. 
# segments(3, 0, 3, ylims[2], lty = 2, col = "darkgray", lwd = 2)
# segments(0, 3.741657, xlims[2], 3.741657, lty = 2, col = "darkgray", lwd = 2)

dev.off()

