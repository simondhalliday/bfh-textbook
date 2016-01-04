require(shape)
pdf(file = "bfh-textbook/firmmarketsupply/isocost_map.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 6, 4, 4))

isocost <- function(l, c = 10, w = 1, r = 1) {
  c - (w/r)*l
}


isoquant <- function(l, alpha = 0.5, x = 5) {
  (x / l^alpha)^(1/(1 - alpha))
}


xlims <- c(0, 13)
ylims <- c(0, 13)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(13, 16.75, 19.25)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = ylims[1], to = ylims[2], by = 1)
# ylabels <- seq(from = ylims[1], to = ylims[2], by = 1)
# ticksx <- seq(from = xlims[1], to = xlims[2], by = 1)
# xlabels <- seq(from = xlims[1], to = xlims[2], by = 1)
ticksy <- c(0, 6, 9, 12, ylims[2])
ylabels <- c(NA, expression(paste(k[1]==frac(c[1], r))), expression(paste(k[2]==frac(c[2], r))), expression(paste(k[3]==frac(c[3], r))), NA)
ticksx <- c(0, 6, 9, 12, xlims[2])
xlabels <- c(NA, expression(paste(l[1]==c[1]/w)), expression(paste(l[2])==c[2]/w), expression(paste(l[3]==c[3]/w)), NA)




axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#lines(xx1, bcA(xx1, w = 10, p = 1.5), col = COLB[3], lwd = graphlinewidth)
lines(xx1, isocost(xx1, c = 6, w = 1, r = 1), col = COLB[3], lwd = graphlinewidth)
lines(xx1, isocost(xx1, c = 9, w = 1, r = 1), col = COLB[3], lwd = graphlinewidth)
lines(xx1, isocost(xx1, c = 12, w = 1, r = 1), col = COLB[3], lwd = graphlinewidth)
#lines(xx1, isoquant(xx1, x = 5, alpha = 0.5), col = COLA[3], lwd = graphlinewidth)


#Label the axes
mtext(expression(paste("Hours of labor, ", l)), side=1, line = 2.5, cex = axislabelsize)
text(-1.8, 0.5*ylims[2], expression(paste("Amount of capital, ", k)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the isoquant curve
# text(11.25, 3.15, expression("isoquant"), cex = labelsize)
# text(11.25, 2.65, expression(paste(x == bar(x))), cex = labelsize)

#Label the price lines
text(5.4, 1, expression(paste(c[1])))
#text(8, 1.3, expression(paste(p[x] == 1)))
text(8.4, 1, expression(paste(c[2])))
#text(11.3, 3.7, expression(paste(p[x] == 0.5)))
text(11.4, 1, expression(paste(c[3])))
#text(11.3, 6.5, expression(paste(p[x] == 0.25)))

#Add the offer curve (superimposed on the indifference curves tangent to the price lines)
#xx2 <- seq(2, xlims[2], length.out = npts)
#lines(xx2, offerCurve(xx2, w = 10, rmax = 2, xmax = 12), col = COL[3], lwd = graphlinewidth)

#Annotate points on offer curve mrs = p for each of p = 1, 0.5, 0.25
#Where mrs = 2 - (1/6)*x

#Label a
# segments(2.68, 0, 2.68, isoquant(l = 2.68), lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(0, isoquant(l = 2.68) , 2.68, isoquant(l = 2.68), lty = 2, col = "gray" , lwd = segmentlinewidth)
# points(2.68, isoquant(l = 2.68), pch = 16, col = "black", cex = 1.5)
# text(2.68 + 0.25, isoquant(l = 2.68) + 0.25, expression(paste(a)), cex = labelsize)

#Label b
# segments(9.35, 0, 9.35, isoquant(9.35), lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(0, isoquant(l = 9.35) , 9.35, isoquant(l = 9.35), lty = 2, col = "gray" , lwd = segmentlinewidth)
# points(9.35, isoquant(l = 9.35), pch = 16, col = "black", cex = 1.5)
# text(9.35 + 0.25, isoquant(l = 9.35) + 0.25, expression(paste(b)), cex = labelsize)


#Label i
# segments(0, 5, 5, 5, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(5, 0, 5, 5, lty = 2, col = "gray" , lwd = segmentlinewidth)
# points(5, 5, pch = 16, col = "black", cex = 1.5)
# text(5.25, 5.25, expression(paste(i)), cex = labelsize)

#Add mrs = mrt at i
text(7, 10.25, expression(paste("Marginal rate of transformation:")), cex = labelsize)
text(7, 9.25, expression(paste(mrt(l,k)== frac(w,r))), cex = labelsize)
Arrows(7, 8.5, 7, 5.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


text(11, 6.25, expression(paste("Equation for isocost:")), cex = labelsize)
text(11, 5.25, expression(paste(k == frac(c,r) - bgroup("(",frac(w,r),")")%.%l)), cex = labelsize)
#Arrows(7, 8.5, 7, 5.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)



#text(9, 5.9, expression(paste(b)), cex = labelsize)
#text(10.5, 7.775, expression(paste(c)), cex = labelsize)

dev.off()
