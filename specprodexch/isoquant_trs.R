#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "specprodexch/isoquant_trs.pdf", width = 7, height = 7)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5


COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 1, 1))

uFn <- function(x, y, alpha = 0.5){
  (x^alpha)*(y^(1 - alpha))
}

indiffA <- function(x, alpha = 0.5, uA = 5) {
  (uA / x^alpha)^(1/(1 - alpha))
}

trsline <- function(x, constant = 10, slope = 4){
  constant - (slope)*x
}

xlims <- c(0, 10)
ylims <- c(0, 10)

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
     xaxs = "i", 
     yaxs = "i"
)


ticksy <- seq(from = 0, to = ylims[2], by = 1)
ylabels <- seq(from = 0, to = ylims[2], by = 1)
ticksx <- seq(from = 0, to = xlims[2], by = 1)
xlabels <- seq(from = 0, to = xlims[2], by = 1)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(1.5, 2.5, length.out = npts)
xx4 <- seq(3, 5, length.out = npts)
xx5 <- seq(7, 9, length.out = npts)

#Draw the graphs
lines(xx1, indiffA(xx1, alpha = 0.5, uA = 4), col = COLA[5], lwd = graphlinewidth)
lines(xx3, trsline(xx3, constant = 16), col = "grey26", lty = 2, lwd = graphlinewidth)
lines(xx4, trsline(xx4, constant = 8, slope = 1), col = "grey26", lty = 2, lwd = graphlinewidth)
lines(xx5, trsline(xx5, constant = 4, slope = 0.25), col = "grey26", lty = 2, lwd = graphlinewidth)

#Axis labels
text(0.5*xlims[2], -1.1, expression(paste("Hours of labor, ", l)), xpd = TRUE, cex = axislabelsize) 
text(-1, 0.5*ylims[2], expression(paste("Quantity of capital goods, ", k)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the indifference curves
text(8.2, 1.25, expression("Cobb-Douglas"), cex = labelsize, xpd = TRUE)
text(8.2, 0.75, expression(paste("isoquant ", underline(x) ==f(l,k))), cex = labelsize, xpd = TRUE)

text(8, 4, expression(paste(mrts(l,k) == frac(2,8), phantom() == 0.25)), cex = labelsize, xpd = TRUE)
Arrows(8, 3.6, 8, 2.4, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(4.5, 6, expression(paste(mrts(l,k) == frac(4,4), phantom()==1)), cex = labelsize)
Arrows(4, 5.6, 4, 4.4, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(4.9, 8, expression(paste(mrts(l,k) == frac(8,2), phantom() == 4)), cex = labelsize)
Arrows(3.3, 8, 2.3, 8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#Label feasible and infeasible
#Label removed for now
#text(8, 9, expression(paste(mrts(l,k) == frac(x[l], x[k]), phantom() == frac(mp[l], mp[k]), phantom() == -frac(dk,dl))), cex = labelsize, xpd = TRUE)


#Annotate points (4,4),(2,8),(8,2) on feasibility frontier
text(3.8, 3.8, expression(paste(b)), cex = labelsize)
points(4, 4, pch = 16, col = "black", cex = 1.5)

text(1.8, 7.8, expression(paste(a)), cex = labelsize)
points(2, 8, pch = 16, col = "black", cex = 1.5)

text(7.8, 1.8, expression(paste(d)), cex = labelsize)
points(8, 2, pch = 16, col = "black", cex = 1.5)


dev.off()
