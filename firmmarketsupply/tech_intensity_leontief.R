#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "firmmarketsupply/tech_intensity_leontief.pdf", width = 7, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 2.5
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 1, 1))

uFn <- function(x, y, alpha = 0.7){
  (x^alpha)*(y^(1 - alpha))
}

isoA <- function(l, alpha = 0.7, xbar = 4) {
  (xbar / l^alpha)^(1/(1 - alpha))
}

trsline <- function(x, constant = 10, slope = 4){
  constant - (slope)*x
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

#Axis labels
mtext(expression(paste("Hours of labor, ", l)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.8, 5, expression(paste("Quantity of capital goods, ", k)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Draw the isocosts
lines(xx1, isocost(xx1, m = 100, pl = 10, pk = 20), col = COLA[5], lwd = graphlinewidth)
lines(xx1, isocost(xx1, m = 100, pl = 20, pk = 10), col = COLA[3], lwd = graphlinewidth)

#Label the isoquants curves
text(9.5, 6.8, expression(paste("Capital-intensive")), cex = labelsize)
text(9.5, 6.3, expression(paste("Technology A, ", x[1]^A == bar(x))), cex = labelsize)

text(9.5, 2.8, expression(paste("Labor-intensive ")), cex = labelsize)
text(9.5, 2.3, expression(paste("Technology B, ", x[1]^B == bar(x))), cex = labelsize)

##Two Leontief Technologies 
#Capital-intensive
segments(2, 6, 2, ylims[2], lty = 1, col = COLB[3] , lwd = graphlinewidth)
segments(2, 6, xlims[2], 6, lty = 1, col = COLB[3] , lwd = graphlinewidth)

#Labor-intensive
segments(6, 2, 6, ylims[2], lty = 1, col = COLB[5] , lwd = graphlinewidth)
segments(6, 2, xlims[2], 2, lty = 1, col = COLB[5] , lwd = graphlinewidth)




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

#text(9.5, 3.15, expression(u[2]^A), cex = labelsize)
#text(9.5, 5.85, expression(u[3]^A), cex = labelsize)

#Draw rays from the origin indicating factor intensity
segments(0, 0, 2, 6, lty = 1, col = "gray" , lwd = segmentlinewidth)
segments(0, 0, 6, 2, lty = 1, col = "gray" , lwd = segmentlinewidth)

# segments(0, 8, 2.9705, 8, lty = 2, col = "gray" , lwd = segmentlinewidth)
#segments(2.9705, 0, 2.9705, 8, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(2, 0, 2, 8, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(4, 0, 4, 4, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(0, 4, 4, 4, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(8, 0, 8, 2, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(0, 2, 8, 2, lty = 2, col = "gray" , lwd = segmentlinewidth)
# 
# segments(8, 0, 8,indiffA(x = 8)  , lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(0, indiffA(x = 8) , 8,indiffA(x = 8)  , lty = 2, col = "gray" , lwd = segmentlinewidth)

#Annotate points (4,4),(2,8),(8,2) on feasibility frontier
# text(4.2, 4.2, expression(paste(b)), cex = labelsize)
points(2, 6, pch = 16, col = "black", cex = 1.5)
text(2 - 0.3, 6, expression(paste(a)), cex = labelsize)

points(6, 2, pch = 16, col = "black", cex = 1.5)
text(6, 2 - 0.3, expression(paste(b)), cex = labelsize)

#Label the isocosts
text(9.1, 0.75, expression(paste(c[1]^B)), cex = labelsize)
text(4.9, 0.75, expression(paste(c[1]^A)), cex = labelsize)


dev.off()
