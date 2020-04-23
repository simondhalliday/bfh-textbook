#Graph Designer: Scott Cohn
#Authors: Bowles and Halliday
#Title: Microeconomics: Competition, Conflict and Coordination

require(shape)
pdf(file = "firmmarketsupply/ac_mc_lr.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 8, 1, 1))


totalcost <- function(x, c0 = 10, c1 = 2, c2 = 0.5){
  c0 + c1*x + c2*x^2
}

avgcost <- function(x, c0 = 10, c1 = 2, c2 = 0.5){
  totalcost/x
}

avgvarcost <- function(x,  c1 = 0.05, c2 = 0.05){
  (c1*x + c2*x^2)/x
}

marginalcost <- function(x, c1 = 2, c2 = 0.5){
  c1 + 2*c2*x
}

mcline <- function(x, constant = 0.3181472, slope = 0.125){
  constant + slope*x
}

isocost <- function(l, c = 10, w = 1, r = 1) {
  c - (w/r)*l
}


isoquant <- function(l, alpha = 0.5, x = 5) {
  (x / l^alpha)^(1/(1 - alpha))
}


avgcost <- function(x,  c0 = 2, c1 = 0.05, c2 = 0.05){
  (c0 + c1*x + c2*x^2)/x
}

marginalcost <- function(x, c1 = 0.05, c2 = 0.05){
  c1 + 2*c2*x
}

xlims <- c(0, 10)
ylims <- c(0, 1.2)

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
     xaxs = "i", 
     yaxs = "i")

# Choose x_i

i <- c(2, 2.5, 3, 3.5, 4)
xi <- i[2]

# ticksy <- c(0, avgvarcost(x = xi), marginalcost(x = xi), avgcost(6.82456), avgcost(x = xi), ylims[2])
# ylabels <- c(NA, expression(paste(avc[h])), expression(paste(mc[g])), expression(paste(ac[a])), expression(paste(ac[f])), NA)
# ticksx <- c(0, xi, 6.32456, xlims[2])
# xlabels <- c(NA, expression(paste(x[i])), expression(paste(x[a])), NA)
ticksy <- c(0, avgcost(6.82456),  ylims[2])
ylabels <- c(NA,  expression(paste(ac[a] == mc[a])),  NA)
ticksx <- c(0, 6.32456, xlims[2])
xlabels <- c(NA, expression(paste(x[a])), NA)


axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(4, 8, length.out = npts)
xx4 <- seq(xlims[1], 6.32456, length.out = npts)
xx5 <- seq(6.32456, xlims[2], length.out = npts)

lines(xx1, avgcost(xx1, c0 = 2, c1 = 0.05, c2 = 0.05), col = COL[1], lwd = graphlinewidth)
lines(xx1, avgvarcost(xx1, c1 = 0.05, c2 = 0.05), col = COL[2], lwd = graphlinewidth)
lines(xx4, marginalcost(xx4, c1 = 0.05, c2 = 0.05), lty = 2, col = COL[3], lwd = graphlinewidth)
lines(xx5, marginalcost(xx5, c1 = 0.05, c2 = 0.05), col = COL[3], lwd = graphlinewidth)

#Label the axes
mtext(expression(paste("Quantity, ", x)), side = 1, line = 2.5, cex = axislabelsize)
text(-1.95, 0.5*ylims[2], expression(paste("Costs of production ($)")), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Draw segments for average cost
# segments(0, avgcost(x = xi, c0 = 2, c1 = 0.05, c2 = 0.05), xi, avgcost(x = xi, c0 = 2, c1 = 0.05, c2 = 0.05), lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(xi, 0, xi, avgcost(x = xi) + 10, lty = 2, col = "gray" , lwd = segmentlinewidth, xpd = TRUE)
# segments(0, marginalcost(x = xi), xi, marginalcost(x = xi), lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(0, avgvarcost(x = xi), xi, avgvarcost(x = xi), lty = 2, col = "gray" , lwd = segmentlinewidth)
#points(xi, avgcost(x = xi, c0 = 2, c1 = 0.05, c2 = 0.05), pch = 16, col = "black", cex = 1.5)
# 
# text(xi + 0.2, avgcost(x = xi) + 0.05, expression(paste("f")), cex = labelsize)
# points(xi, avgcost(x = xi), pch = 16, col = "black", cex = 1.5)
# 
# text(xi + 0.2, marginalcost(x = xi) - 0.05, expression(paste("g")), cex = labelsize)
# points(xi, marginalcost(x = xi), pch = 16, col = "black", cex = 1.5)
# 
# text(xi + 0.2, avgvarcost(x = xi) - 0.05, expression(paste("h")), cex = labelsize)
# points(xi, avgvarcost(x = xi), pch = 16, col = "black", cex = 1.5)

#Label the cost curve
text(12.7, 12, expression("Total cost"), cex = labelsize)
#text(12.5, 13.5, expression(paste(tc(x) == c[0] + c[1]*x + c[2]*x^2)), cex = annotatesize)

#Average Cost
text(4.5, avgcost(x = 3.2), expression(paste("Average costs")), cex = labelsize)
#text(3.75, avgcost(x = 3.2) + 0.67, expression(paste(ac(x) == frac(c[0],x) + c[1] + c[2]*x)), cex = annotatesize)

#Average cost minimum
segments(0, avgcost(x = 6.32456), 6.32456, avgcost(x = 6.32456), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(6.32456, 0, 6.32456, 2, lty = 2, col = "gray" , lwd = segmentlinewidth, xpd = TRUE)
text(6.1, avgcost(x = 6.32456) + 0.04, expression(paste("a")), cex = annotatesize)
points(6.32456, avgcost(x = 6.32456), pch = 16, col = "black", cex = 1.5)


#Marginal Cost
text(8.25, marginalcost(x = 8.25) + 0.15, expression(paste("Marginal cost and \n firm supply curve")), cex = annotatesize)
#text(11, marginalcost(x = 12) + 0.2, expression(paste(mc(x) == c[1] + 2*c[2]*x)), cex = annotatesize)

#Average Variable Cost
text(8.25, avgvarcost(x = 8) - 0.1, expression(paste("Average variable cost")), cex = annotatesize)
#text(12, avgvarcost(x = 12) - 0.325, expression(paste(avc(x) == c[1] + c[2]*x)), cex = annotatesize)


dev.off()
