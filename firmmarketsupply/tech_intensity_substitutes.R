#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "firmmarketsupply/tech_intensity_substitutes.pdf", width = 7, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 2
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 4, 4))

isocost <- function(l, m = 100, pl = 20, pk = 10){
  m/pk - (pl/pk)*l
}

isoquant <- function(l, m = 10, alpha = 0.5){
  m - alpha*l
}

xlims <- c(0, 11)
ylims <- c(0, 11)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(2, 4, 6)

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


# ticksy <- seq(from = 0, to = ylims[2], by = 1)
# ylabels <- seq(from = 0, to = ylims[2], by = 1)
# ticksx <- seq(from = 0, to = xlims[2], by = 1)
# xlabels <- seq(from = 0, to = xlims[2], by = 1)
ticksy <- c(ylims[1],  ylims[2])
ylabels <- c(NA, NA)
ticksx <- c(xlims[1], xlims[2])
xlabels <- c(NA, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(1.5, 2.5, length.out = npts)
xx4 <- seq(3, 5, length.out = npts)
xx5 <- seq(7, 9, length.out = npts)

#Draw the graphs
lines(xx1, isoquant(xx1, m = 10, alpha = 1.5), col = COLB[5], lwd = graphlinewidth)
lines(xx1, isoquant(xx1, m = 8, alpha = 0.8), col = COLB[3], lwd = graphlinewidth)
lines(xx1, isocost(xx1, m = 100, pk = 10, pl = 20), col = COLA[5], lwd = graphlinewidth)
lines(xx1, isocost(xx1, m = 100, pk = 20, pl = 10), col = COLA[3], lwd = graphlinewidth)

#Axis labels
mtext(expression(paste("Hours of labor, ", l)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.8, 5, expression(paste("Quantity of capital goods, ", k)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the isoquant curves
#text(2.3, 10.3, expression(paste("Capital-intensive")), cex = labelsize)
text(2.3, 9.8, expression(paste("Perfect substitute")), cex = labelsize)
text(2.3, 9.3, expression(paste("Technology A, ", x[1]^A,":")), cex = labelsize)
text(2.3, 8.8, expression(paste("when ", p[k] < p[l])), cex = labelsize)
text(2.3, 8.3, expression(paste("use only k")), cex = labelsize)

#text(9.5, 3.3, expression(paste("Labor-intensive ")), cex = labelsize)
text(9.5, 2.8, expression(paste("Perfect substitute ")), cex = labelsize)
text(9.5, 2.3, expression(paste("Technology B, ", x[1]^B,":")), cex = labelsize)
text(9.5, 1.8, expression(paste("when ", p[l] < p[k])), cex = labelsize)
text(9.5, 1.3, expression(paste("use only l")), cex = labelsize)


# text(8, 9.7, expression(paste(trs^B*(list(l,k)) > trs^A*(list(l,k)))), cex = labelsize)
# text(8, 8.8, expression(paste(frac(x[l]^B, x[k]^B) > frac(x[l]^A, x[k]^A))), cex = labelsize)
# text(8, 8, expression(paste("along the ray ", frac(k,l))), cex = labelsize)
# Arrows(7, 8.6, 3.7, 8.6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# Arrows(8, 3.6, 8, 2.4, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# 
# text(4, 6, expression(paste(trs(l,k) == frac(4,4), phantom()==1)), cex = labelsize)
# Arrows(4, 5.6, 4, 4.4, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# 
# text(4, 8, expression(paste(trs(l,k) == frac(8,2), phantom() == 4)), cex = labelsize)
# Arrows(3.05, 8, 2.3, 8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label feasible and infeasible
#text(8, 9, expression(paste(trs(l,k) == frac(x[l], x[k]), phantom() == frac(mp[l], mp[k]), phantom() == -frac(dk,dl))), cex = labelsize)
#text(3, 2.5, expression(paste("with plan ", (list(x, l, k)) )), cex = labelsize)

# text(8, 8.5, expression(paste("Production set:")), cex = labelsize)
# text(8, 8, expression(paste(x = bar(x), " feasible")), cex = labelsize)
# text(8, 7.5, expression(paste("with plan ", (list(x, l, k)) )), cex = labelsize)

#For alpha = 1/3; pl = 20; pk= 10
#pl/pk = 2
#trs =  1/3 k / 2/3 l = k/2l = 2 => k = 4l
#sub into isocost: 4l  = 100/10 - 2l => 6l = 10; l = 5/3; k = 20/3
segments(0, 0, 5/3, isoquant(l = 5/3, alpha = 1/3, xbar = 4.2) , lty = 1, col = "gray" , lwd = segmentlinewidth)
points(5/3, isoquant(l = 5/3, alpha = 1/3, xbar = 4.2), pch = 16, col = "black", cex = 1.5)
text(5/3 - 0.3, isoquant(l = 5/3, alpha = 1/3, xbar = 4.2), expression(paste(a)), cex = labelsize)

#For alpha = 2/3; pl = 10; pk = 20
#pl/pk = 2
#trs = - 2/3 k / 1/3 l = 2k/l = 1/2 => k = 1/4 l
#sub into isocost: 1/4 l  = 100/20 - 2l => 3l = 5; l = 100/15; k = 100/60
segments(0, 0, 100/15, isoquant(l = 100/15, alpha = 2/3, xbar = 4.2), lty = 1, col = "gray" , lwd = segmentlinewidth)
points(100/15, isoquant(l = 100/15, alpha = 2/3, xbar = 4.2), pch = 16, col = "black", cex = 1.5)
text(100/15, isoquant(l = 100/15, alpha = 2/3, xbar = 4.2) - 0.3, expression(paste(b)), cex = labelsize)

#Label the isocosts
text(8.2, 0.5, expression(paste(c[1]^B)), cex = labelsize)
text(4.4, 0.5, expression(paste(c[1]^A)), cex = labelsize)

dev.off()
