#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "specprodexch/techvariables1.pdf", width = 7, height = 7)

#Set parameters for graphics
axislabelsize <- 1.3
axistitlesize <- 1.5
labelsize <- 1.1
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 1, 1))

isoquant <- function(l, alpha = 0.7, xbar = 4) {
  (xbar / l^alpha)^(1/(1 - alpha))
}

isocost <- function(l, m = 100, pl = 20, pk = 10){
  m/pk - (pl/pk)*l
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
     cex.lab = axistitlesize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i"
)


# ticksy <- seq(from = 0, to = ylims[2], by = 1)
# ylabels <- seq(from = 0, to = ylims[2], by = 1)
# ticksx <- seq(from = 0, to = xlims[2], by = 1)
# xlabels <- seq(from = 0, to = xlims[2], by = 1)
ticksy <- c(0, isoquant(l = 6.66, alpha = 2/3, xbar = 4.22), isoquant(l = 1.8, alpha = 1/3, xbar = 4.22), isoquant(l = 2.77, alpha = 2/3, xbar = 4.22),  ylims[2])
ylabels <- c(NA, expression(paste(k[j])), expression(paste(k[h])), expression(paste(k[i])), NA)
ticksx <- c(0, 1.8, 2.77, 6.66, xlims[2])
xlabels <- c(NA, expression(paste(l[h])), expression(paste(l[i])), expression(paste(l[j])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = axislabelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = axislabelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(1.5, 2.5, length.out = npts)
xx4 <- seq(3, 5, length.out = npts)
xx5 <- seq(7, 9, length.out = npts)

#Draw the graphs
lines(xx1, isoquant(xx1, alpha = 2/3, xbar = 4.22), col = "grey22", lwd = graphlinewidth)
lines(xx1, isoquant(xx1, alpha = 1/3, xbar = 4.22), col = "grey22", lwd = graphlinewidth)
lines(xx1, isocost(xx1, m = 100, pk = 10, pl = 20), col = COLB[5], lwd = graphlinewidth)
lines(xx1, isocost(xx1, m = 100, pk = 20, pl = 10), col = "#a50f15", lwd = graphlinewidth)

#Axis labels
mtext(expression(paste("Hours of labor, ", l)), side = 1, line = 2.5, cex = axistitlesize)
text(-1.1, 5, expression(paste("Quantity of capital goods, ", k)), xpd = TRUE, cex = axistitlesize, srt = 90) 


#Label the isoquants
text(9.5, 4.5, expression(paste("Capital-intensive")), cex = labelsize)
text(9.5, 4, expression(paste("Technology A")), cex = labelsize)
text(9.5, 3.5, expression(paste(x^A*bgroup("(", list(l,k),")")) == bar(x)), cex = labelsize)


text(9.5, 2.35, expression(paste("Labor-intensive ")), cex = labelsize)
text(9.5, 1.85, expression(paste("Technology B")), cex = labelsize)
text(9.5, 1.35, expression(paste(x^B*bgroup("(", list(l,k),")")) == bar(x)), cex = labelsize)

text(8, 9.7, expression(paste(trs^B*(list(l,k)) > trs^A*(list(l,k)))), cex = labelsize)
text(8, 8.8, expression(paste(frac(x[l]^B, x[k]^B) > frac(x[l]^A, x[k]^A))), cex = labelsize)
text(8, 8, expression(paste("along the ray ", frac(k,l))), cex = labelsize)
Arrows(7, 8.6, 2.7, 8.6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

# Arrows(8, 3.6, 8, 2.4, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# 
# text(4, 6, expression(paste(trs(l,k) == frac(4,4), phantom()==1)), cex = labelsize)
# Arrows(4, 5.6, 4, 4.4, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# 
# text(4, 8, expression(paste(trs(l,k) == frac(8,2), phantom() == 4)), cex = labelsize)
# Arrows(3.05, 8, 2.3, 8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#Ray from t he origin
segments(0, 0, 3.1, ylims[2], lty = 2, col = "grey22" , lwd = segmentlinewidth)

# segments(0, 8, 2.9705, 8, lty = 2, col = "gray" , lwd = segmentlinewidth)
#segments(2.9705, 0, 2.9705, 8, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(2, 0, 2, 8, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(4, 0, 4, 4, lty = 2, col = "gray" , lwd = segmentlinewidth)



segments(0, isoquant(l = 1.8, alpha = 1/3, xbar = 4.22), 
         1.8, isoquant(l = 1.8, alpha = 1/3, xbar = 4.22), 
         lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(1.8, 0, 
         1.8, isoquant(l = 1.8, alpha = 1/3, xbar = 4.22), 
         lty = 2, col = "gray" , lwd = segmentlinewidth)
points(1.8, isoquant(l = 1.8, alpha = 1/3, xbar = 4.22), pch = 16, col = "black", cex = 1.5)
text(1.8 + 0.3, isoquant(l = 1.8, alpha = 1/3, xbar = 4.22), expression(paste(h)), cex = labelsize)


segments(0, isoquant(l = 2.77, alpha = 2/3, xbar = 4.22), 
         2.77, isoquant(l = 2.77, alpha = 2/3, xbar = 4.22), 
         lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(2.77, 0, 
         2.77, isoquant(l = 2.77, alpha = 2/3, xbar = 4.22), 
         lty = 2, col = "gray" , lwd = segmentlinewidth)
points(2.77, isoquant(l = 2.77, alpha = 2/3, xbar = 4.22), pch = 16, col = "black", cex = 1.5)
text(2.77 + 0.3, isoquant(l = 2.77, alpha = 2/3, xbar = 4.22), expression(paste(i)), cex = labelsize)


segments(0, isoquant(l = 6.66, alpha = 2/3, xbar = 4.22), 
         6.66, isoquant(l = 6.66, alpha = 2/3, xbar = 4.22), 
         lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(6.66, 0, 
         6.66, isoquant(l = 6.66, alpha = 2/3, xbar = 4.22), 
         lty = 2, col = "gray" , lwd = segmentlinewidth)
points(6.66, isoquant(l = 6.66, alpha = 2/3, xbar = 4.22), pch = 16, col = "black", cex = 1.5)
text(6.66, isoquant(l = 6.66, alpha = 2/3, xbar = 4.22) + 0.3, expression(paste(j)), cex = labelsize)

#Label isocosts
text(0.5, 8.25, expression(paste(c[1]^{H*minute})), cex = labelsize)
text(8.25, 0.5, expression(paste(c[1]^{L*minute})), cex = labelsize)
dev.off()
