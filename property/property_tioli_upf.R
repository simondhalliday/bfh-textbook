require(ggplot2)
require(shape)
require(plotrix)
#pdf(file = "property_tioli_upfSTEP1.pdf", width = 9, height = 7)
#pdf(file = "property_tioli_upfSTEP2.pdf", width = 9, height = 7)
#pdf(file = "property_tioli_upfSTEP3.pdf", width = 9, height = 7)
pdf(file = "property/property_tioli_upf.pdf", width = 9, height = 7)

#Set parameters for graphics
namesize <- 1.3
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5



#Colors
COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")
grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")

upf <- function(ub) {
  12.24745 - ub
}


uA <- function(xA, yA, alpha = 1/2){
  ((xA)^alpha)*((yA)^(1 - alpha))
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

#COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")
par(mar =  c(4, 4, 1, 1))
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

text(0.5*xlims[2], -1.3, expression(paste("A's Utility, ", u^A)), xpd = TRUE, cex = axislabelsize) 
text(-0.9, 0.5*ylims[2], expression(paste("B's Utility, ", u^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#polygons
#I need something like xx1 with npts for 
xpoly1 <- c(uA(9,1), 8.5, uA(9,1), uA(9,1))
ypoly1 <- c(uA(1,14), uA(1,14), upf(uA(9,1)), uA(1,14))
polygon(x = xpoly1, y = ypoly1, col=COL[4], density=NULL, border = NA)

#xpoly2 = c(0, 0, 12.24745, xlims[2], xlims[2], 0)
#ypoly2 = c(ylims[2], 12.24745, 0, 0, ylims[2], ylims[2])
#polygon(x = xpoly2, y = ypoly2, col=COL[4], density=NULL, border = NA)



#Draw the lines for the graphs
lines(xx1, upf(xx1), col = CBCols[1], lwd = graphlinewidth)

#Customize ticks and labels for the plot
ticksy <- seq(from = ylims[1], to = ylims[2], by = 1)
ylabels <- seq(from = ylims[1], to = ylims[2], by = 1)
ticksx <- seq(from =  xlims[1], to = xlims[2], by = 1)
xlabels <- seq(from = xlims[1], to = xlims[2], by = 1)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#Annotation of the  graphs
#UPF
text(8.1, 1.5, expression(paste("Utility possibilities frontier")), 
     cex = annotatesize)
# text(8.1, 1, expression(paste(u^B == bar(W) - u^A)), 
#      cex = annotatesize)

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
segments(0,  uA(1,14), xlims[2], uA(1,14), lty = 2, col = "darkgray", lwd = 2)
segments(uA(9,1), 0, uA(9,1), ylims[2], 13, lty = 2, col = "darkgray", lwd = 2)

#Annotate e
points(uA(9,1),  uA(1,14), pch = 16, col = "black", cex = 1.5)
text(uA(9,1)-0.3,  uA(1,14)-0.3, expression(z), cex = annotatesize)

#Annotate f
points(8.5, uA(1,14), pch = 16, col = "black", cex = 1.5)
text(8.5 - 0.2, uA(1,14) - 0.3, expression(t^A), cex = annotatesize)

#Annotate g
points(uA(9,1, alpha = 1/2), upf(uA(9,1)), pch = 16, col = "black", cex = 1.5)
text(uA(9,1, alpha = 1/2)-0.3, upf(uA(9,1))-0.3, expression(t^B), cex = annotatesize)

#Annotate m
#points(4.389995,  5.703502, pch = 16, col = "black", cex = 1.5)
#text(4.6, 5.9, expression(m))

#Label economic rents
#Arrows(6.68, 5.12, 6.68, 7.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(4.5, 5.4, expression(paste("Bargaining")), 
     cex = annotatesize)
text(4.5, 4.7, expression(paste("set")), 
     cex = annotatesize)


#Point h
#points(4, 10, pch = 16, col = "black", cex = 1.5)
#u^A_h(6, 5) = (6^0.5)*(5^0.5) = 5.477226
#u^A_h(4, 10) = (4^0.5)*(10^0.5) = 6.324555
#points(6.12, 4, pch = 16, col = "black", cex = 1.5)
#text(5.92, 3.8, expression(h))

#Annotate Point i
#(5^0.5)*(7.5^0.5) = 6.123724
#W = (6.123724^0.5)(6.123724^0.5) = 6.123724
points(6.123724, 6.123724, pch = 16, col = "black", cex = 1.5)
text(6.123724 + 0.2, 6.123724 + 0.2, expression(i), cex = annotatesize)

#Label Participation Constraints
#Aisha's
text(11, 4.9, expression(paste("B's participation")), cex = annotatesize)
text(11, 4.2, expression(paste("constraint, ", u[z]^B)), cex = annotatesize)

#Betty's
text(4.6, 12.7, expression(paste("A's participation")), cex = annotatesize)
text(4.6, 12, expression(paste("constraint, ", u[z]^A)), cex = annotatesize)

#Arrows showing Social Planner's choices
#Arrows(5.2, 4.2, 5.9, 4.2, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = arrowwidth)
#Arrows(6.12, 4.2, 6.12, 5.7, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = arrowwidth)

dev.off()

