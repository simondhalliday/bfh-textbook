#Graph Designer: Scott Cohn
#Authors: Bowles and Halliday
#Title: Microeconomics: Competition, Conflict and Coordination

require(shape)
pdf(file = "firmmarketsupply/tc_ac_mc_stacked.pdf", width = 9, height = 12)

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
par(mar =  c(4, 8, 1, 1), mfrow = c(2, 1))


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

# total cost --------------------------------------------------------------


xlims <- c(0, 18)
ylims <- c(0, 15)

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


ticksy <- c(0, 2, totalcost(x = 4, c0 = 2, c1 = 0.05, c2 = 0.05), ylims[2])
ylabels <- c(NA, expression(paste(c[0])), expression(paste(tc(x[1]))), NA)
ticksx <- c(0, 4, xlims[2])
xlabels <- c(NA, expression(paste(x[1])), NA)


axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(2, 6, length.out = npts)

lines(xx1, totalcost(xx1, c0 = 2, c1 = 0.05, c2 = 0.05), col = COLB[3], lwd = graphlinewidth)
lines(xx3, mcline(xx3, constant = totalcost(x = 4, c0 = 2, c1 = 0.05, c2 = 0.05) - 4*marginalcost(x = 4, c1 = 0.05, c2 = 0.05), slope = marginalcost(x = 4, c1 = 0.05, c2 = 0.05)), 
      col = "grey22", lty = 2, lwd = graphlinewidth)

#Label the axes
#mtext(expression(paste("Quantity of output, ", x)), side = 1, line = 2.5, cex = axislabelsize)
text(-2.7, 0.5*ylims[2], expression(paste("Total cost of production, ",tc)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the cost curve
text(11.5, 14, expression("Total Cost"), cex = annotatesize)
text(11.5, 13.5, expression(paste(tc(x) == c[0] + c[1]*x + c[2]*x^2)), cex = annotatesize)

#Average Cost
text(3.5, 10.2, expression(paste("Slope of ray from origin")), cex = annotatesize)
text(3.5, 9.4, expression(paste("equals Average Cost")), cex = annotatesize)
Arrows(2, 9, 2, 1.8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Marginal Cost
text(13.5, totalcost(x = 4, c0 = 2, c1 = 0.05, c2 = 0.05) + 0.3, expression(paste("Slope of tangent line")), cex = annotatesize)
text(13.5, totalcost(x = 4, c0 = 2, c1 = 0.05, c2 = 0.05) - 0.3, expression(paste("equals Marginal Cost")), cex = annotatesize)
Arrows(10, totalcost(x = 4, c0 = 2, c1 = 0.05, c2 = 0.05), 4.5, totalcost(x = 4, c0 = 2, c1 = 0.05, c2 = 0.05) , col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Draw segments for average cost
segments(0, totalcost(x = 4, c0 = 2, c1 = 0.05, c2 = 0.05), 4, totalcost(x = 4, c0 = 2, c1 = 0.05, c2 = 0.05), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(4, 0, 4, totalcost(x = 4, c0 = 2, c1 = 0.05, c2 = 0.05), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 0, 4, totalcost(x = 4, c0 = 2, c1 = 0.05, c2 = 0.05), lty = 2, col = COLB[4] , lwd = segmentlinewidth)
points(4, totalcost(x = 4, c0 = 2, c1 = 0.05, c2 = 0.05), pch = 16, col = "black", cex = 1.5)


# ac & mc -----------------------------------------------------------------

avgcost <- function(x,  c0 = 2, c1 = 0.05, c2 = 0.05){
  (c0 + c1*x + c2*x^2)/x
}

avgvarcost <- function(x,  c1 = 0.05, c2 = 0.05){
  (c1*x + c2*x^2)/x
}

marginalcost <- function(x, c1 = 0.05, c2 = 0.05){
  c1 + 2*c2*x
}

mcline <- Mpline <- function(x, constant = 0.3181472, slope = 0.125){
  constant + slope*x
}

xlims <- c(0, 15)
ylims <- c(0, 1.7)

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


ticksy <- c(0, avgvarcost(x = 4), marginalcost(x = 4), avgcost(6.82456), avgcost(x = 4), ylims[2])
ylabels <- c(NA, expression(paste(avc(x[1]))), expression(paste(mc(x[1]))), expression(paste(ac[min])), expression(paste(ac(x[1]))), NA)
ticksx <- c(0, 4, 6.32456, xlims[2])
xlabels <- c(NA, expression(paste(x[1])), expression(paste(x[a])), NA)


axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(4, 8, length.out = npts)

lines(xx1, avgcost(xx1, c0 = 2, c1 = 0.05, c2 = 0.05), col = COL[1], lwd = graphlinewidth)
lines(xx1, avgvarcost(xx1, c1 = 0.05, c2 = 0.05), col = COL[2], lwd = graphlinewidth)
lines(xx1, marginalcost(xx1, c1 = 0.05, c2 = 0.05), col = COL[3], lwd = graphlinewidth)

#Label the axes
mtext(expression(paste("Quantity of output, ", x)), side = 1, line = 2.5, cex = axislabelsize)
text(-2.5, 0.5*ylims[2], expression(paste("Costs of production, ",tc)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the cost curve
text(12.5, 14, expression("Total Cost"), cex = labelsize)
text(12.5, 13.5, expression(paste(tc(x) == c[0] + c[1]*x + c[2]*x^2)), cex = annotatesize)

#Average Cost
text(3.75, avgcost(x = 3.2) + 0.825, expression(paste("Average Costs")), cex = labelsize)
text(3.75, avgcost(x = 3.2) + 0.7, expression(paste(ac(x) == frac(c[0],x) + c[1] + c[2]*x)), cex = annotatesize)

#Average cost minimum
segments(0, avgcost(x = 6.32456), 6.32456, avgcost(x = 6.32456), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(6.32456, 0, 6.32456, avgcost(x = 6.32456), lty = 2, col = "gray" , lwd = segmentlinewidth)
text(6.32456, avgcost(x = 6.32456) + 0.05, expression(paste("a")), cex = annotatesize)
points(6.32456, avgcost(x = 6.32456), pch = 16, col = "black", cex = 1.5)


#Marginal Cost
text(11, marginalcost(x = 12) + 0.3, expression(paste("Marginal Cost")), cex = annotatesize)
text(11, marginalcost(x = 12) + 0.2, expression(paste(mc(x) == c[1] + 2*c[2]*x)), cex = annotatesize)

#Average Variable Cost
text(12, avgvarcost(x = 12) - 0.2, expression(paste("Average Variable Cost")), cex = annotatesize)
text(12, avgvarcost(x = 12) - 0.3, expression(paste(avc(x) == c[1] + c[2]*x)), cex = annotatesize)


#Draw segments for average cost
segments(0, avgcost(x = 4, c0 = 2, c1 = 0.05, c2 = 0.05), 4, avgcost(x = 4, c0 = 2, c1 = 0.05, c2 = 0.05), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(4, 0, 4, avgcost(x = 4), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, marginalcost(x = 4), 4, marginalcost(x = 4), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, avgvarcost(x = 4), 4, avgvarcost(x = 4), lty = 2, col = "gray" , lwd = segmentlinewidth)
points(4, avgcost(x = 4, c0 = 2, c1 = 0.05, c2 = 0.05), pch = 16, col = "black", cex = 1.5)

text(4.2, avgcost(x = 4) + 0.05, expression(paste("f")), cex = labelsize)
points(4, avgcost(x = 4), pch = 16, col = "black", cex = 1.5)

text(4.2, marginalcost(x = 4) - 0.05, expression(paste("g")), cex = labelsize)
points(4, marginalcost(x = 4), pch = 16, col = "black", cex = 1.5)

text(4.2, avgvarcost(x = 4) - 0.05, expression(paste("h")), cex = labelsize)
points(4, avgvarcost(x = 4), pch = 16, col = "black", cex = 1.5)



dev.off()
