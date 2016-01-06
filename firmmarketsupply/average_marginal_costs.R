#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "firmmarketsupply/average_marginal_costs.pdf", width = 9, height = 7)

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

totalcost <- function(x, c0 = 10, c1 = 2, c2 = 0.5){
  c0 + c1*x + c2*x^2
}

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
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = ylims[1], to = ylims[2], by = 1)
# ylabels <- seq(from = ylims[1], to = ylims[2], by = 1)
# ticksx <- seq(from = xlims[1], to = xlims[2], by = 1)
# xlabels <- seq(from = xlims[1], to = xlims[2], by = 1)
ticksy <- c(0, avgvarcost(x = 4), marginalcost(x = 4), avgcost(6.82456), avgcost(x = 4), ylims[2])
ylabels <- c(NA, expression(paste(avc(x[1]))), expression(paste(mc(x[1]))), expression(paste(ac[min])), expression(paste(ac(x[1]))), NA)
ticksx <- c(0, 4, 6.32456, xlims[2])
xlabels <- c(NA, expression(paste(x[1])), expression(paste(x[a])), NA)




axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(4, 8, length.out = npts)

#lines(xx1, bcA(xx1, w = 10, p = 1.5), col = COLB[3], lwd = graphlinewidth)
lines(xx1, avgcost(xx1, c0 = 2, c1 = 0.05, c2 = 0.05), col = COL[1], lwd = graphlinewidth)
lines(xx1, avgvarcost(xx1, c1 = 0.05, c2 = 0.05), col = COL[2], lwd = graphlinewidth)
lines(xx1, marginalcost(xx1, c1 = 0.05, c2 = 0.05), col = COL[3], lwd = graphlinewidth)
#lines(xx3, mcline(xx3, constant = totalcost(x = 4, c0 = 2, c1 = 0.05, c2 = 0.05) - 6*marginalcost(x = 4, c1 = 0.05, c2 = 0.05), slope = marginalcost(x = 4, c1 = 0.05, c2 = 0.05)), col = "gray", lty = 2, lwd = graphlinewidth)

#Label the axes
mtext(expression(paste("Quantity of output, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-1.9, 0.5*ylims[2], expression(paste("Costs of production, ",tc)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the cost curve
text(12.5, 14, expression("Total Cost"), cex = labelsize)
text(12.5, 13.5, expression(paste(tc(x) == c[0] + c[1]*x + c[2]*x^2)), cex = labelsize)

#Average Cost
text(3.3, avgcost(x = 3.2) + 0.825, expression(paste("Average Costs")), cex = labelsize)
text(3.3, avgcost(x = 3.2) + 0.7, expression(paste(ac(x) == frac(c[0],x) + c[1] + c[2]*x)), cex = labelsize)

#Average cost minimum
segments(0, avgcost(x = 6.32456), 6.32456, avgcost(x = 6.32456), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(6.32456, 0, 6.32456, avgcost(x = 6.32456), lty = 2, col = "gray" , lwd = segmentlinewidth)
text(6.32456+0.1, avgcost(x = 6.32456)+0.05, expression(paste("a")), cex = labelsize)
points(6.32456, avgcost(x = 6.32456), pch = 16, col = "black", cex = 1.5)


#Marginal Cost
text(12, marginalcost(x = 12) + 0.3, expression(paste("Marginal Cost")), cex = labelsize)
text(12, marginalcost(x = 12) + 0.2, expression(paste(mc(x) == c[1] + 2*c[2]*x)), cex = labelsize)

#Average Variable Cost
text(12, avgvarcost(x = 12) - 0.15, expression(paste("Average Variable Cost")), cex = labelsize)
text(12, avgvarcost(x = 12) - 0.25, expression(paste(avc(x) == c[1] + c[2]*x)), cex = labelsize)


#Draw segments for average cost
segments(0, avgcost(x = 4, c0 = 2, c1 = 0.05, c2 = 0.05), 4, avgcost(x = 4, c0 = 2, c1 = 0.05, c2 = 0.05), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(4, 0, 4, avgcost(x = 4), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, marginalcost(x = 4), 4, marginalcost(x = 4), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, avgvarcost(x = 4), 4, avgvarcost(x = 4), lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(0, 0, 4, avgcost(x = 4, c0 = 2, c1 = 0.05, c2 = 0.05), lty = 2, col = COLB[4] , lwd = segmentlinewidth)
points(4, avgcost(x = 4, c0 = 2, c1 = 0.05, c2 = 0.05), pch = 16, col = "black", cex = 1.5)

text(4.1, avgcost(x = 4) + 0.05, expression(paste("f")), cex = labelsize)
points(4, avgcost(x = 4), pch = 16, col = "black", cex = 1.5)

text(4.15, marginalcost(x = 4) - 0.05, expression(paste("g")), cex = labelsize)
points(4, marginalcost(x = 4), pch = 16, col = "black", cex = 1.5)

text(4.15, avgvarcost(x = 4) - 0.05, expression(paste("h")), cex = labelsize)
points(4, avgvarcost(x = 4), pch = 16, col = "black", cex = 1.5)

dev.off()
