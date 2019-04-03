require(shape)
pdf(file = "indmarketdemand/sugar_tax.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(6, 6, 4, 4))

mrsA <- function(x, rmax = 20, xmax = 10) {
  rmax - (rmax/xmax)*x
}

uA <- function(x, y, rmax = 20, xmax = 10) {
  y + rmax*x - (1/2)(rmax/xmax)*x^2
}

Qs <- function(x, tax = 0){
  x + 5 + tax
}

Qs_tax<- function(x, tax = 3){
  x + 5 + tax
}

xlims <- c(0, 11)
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
ticksy <- c(0, 10, 12, 20, ylims[2])
ylabels <- c(NA, expression(paste(p[b] == 10)), expression(paste(p[a] == 12)), expression(paste(bar(p) == 20)), NA)
ticksx <- c(0, 2, 4, 5, 10, xlims[2])
xlabels <- c(0, expression(paste(x["m"]) == 2),expression(paste(x[a]) == 4), expression(paste(x[b]) == 5), expression(paste(bar(x)==10)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

# Tax Rev Q
xpoly <- c(4, 4, 5)
ypoly <- c(9, 10, 10)
polygon(x = xpoly, y = ypoly, col = COL[2], density=NULL, border = NA)

# Tax Rev P 
xpoly1 <- c(0, 4, 4, 0)
ypoly1 <- c(10, 10, 12, 12)
polygon(x = xpoly1, y = ypoly1, col = COLB[1], density=NULL, border = NA)

# DWL
xpoly2 <- c(4, 5, 4) 
ypoly2 <- c(10, 10, 12) # Change y1 to 10 to meet gray line
polygon(x = xpoly2, y = ypoly2, col = COLA[1], density=NULL, border = NA)

# CS 
xpoly3 <- c(0, 0, 4)
ypoly3 <- c(12, 20, 12)
polygon(x = xpoly3, y = ypoly3, col = COL[4], density=NULL, border = NA)


#Lines for mrs graph
lines(xx1, mrsA(xx1, rmax = 20, xmax = 10), col = COLA[4], lwd = graphlinewidth)

lines(xx1, Qs(xx1), col = COLB[4], lwd = graphlinewidth)
lines(xx1, Qs_tax(xx1), col = COLB[5], lwd = graphlinewidth)

# Market Price
segments(0, 12, 4, 12, lty = 2, col = "gray" , lwd = segmentlinewidth) 
segments(0, 10, 5, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)

# Vert Seg from Q 
segments(5, 0, 5, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(4, 0, 4, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)

# Q*
segments(2, 0, 2, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)

#Label axes
mtext(expression(paste("Quantity of Sugary Drinks, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-1.6, 0.5*ylims[2], expression(paste("Price per liter, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label i
points(5, 10, pch = 16, col = "black", cex = 1.5)
text(5, 10.75, expression(b))

#Label g
points(4, 12, pch = 16, col = "black", cex = 1.5)
text(4, 12.75, expression(a))

#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
text(8.5, 19, expression(paste("Supply with tax, ", p(x) == 8 + x)), cex = labelsize)
text(7.75, 10.5, expression(paste("Supply, ", p(x) == 5 + x)), cex = labelsize)
#text(7.3, 2.5, expression("Curve"))
#Arrows(7.3, 3.5, 7.3, 6.1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label mrs function
text(8.05, 8, expression(paste("Demand, ", p(x) == 20 - 2*x)), cex = labelsize)
#Arrows(10, 7.5, 10, 5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label satiation
#text(20, 3.5, expression(paste(x[max] == "Point")))
#text(20, 3, expression(paste("of Satiation")))
#Arrows(20, 2.5, 20, 0.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label highest willingness to pay
#text(1.25, 15, expression(paste("Consumer Surplus" )))
#text(1.25, 13.5, expression(paste(cs[t]==frac(1, 2)*bgroup("(",r[max] - p[t],")")*x)))
#text(2.5, 19, expression(paste(CS==frac(1, 2)*bgroup("(",20 - 12,")")*4, phantom()== 16)))
#Arrows(2, 18, 2, 13, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label tax revenue
text(1.3, 11, expression(paste("Tax Revenue, ", R==t%.%x[t])))
#text(3.5, 18, expression(paste(CS==frac(1, 2)*bgroup("(",r[max] - p,")")*x)))
#text(2, 2, expression(paste()))
#text(2.5, 0.75, expression(paste(R==2*bgroup("(",12 - 10,")")*4, phantom()== 8)))
#Arrows(2, 4, 2, 10.75, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label deadweight loss
#text(4.25, 17, expression("Deadweight Loss"))
#text(3.5, 18, expression(paste(CS==frac(1, 2)*bgroup("(",r[max] - p,")")*x)))
#text(4.25, 15.5, expression(paste(dwl==frac(1,2)*bgroup("(",x[nt] - x[t],")")*t)))
#text(4.5, 14.5, expression(paste(DWL==frac(1,2)*bgroup("(",12 - 10,")")*bgroup("(",5 - 4,")"), phantom()== 1)))
Arrows(6, 11.6, 6, 13.1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.2)
Arrows(6, 13.1, 6, 11.6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.2)
text(6.6, 12.7, expression(paste("Tax" == 3)), cex = labelsize)

# Label CS
text(1.2, 14, expression(paste("Consumer Surplus")))


dev.off()