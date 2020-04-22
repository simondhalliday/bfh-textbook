#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "firmmarketsupply/supply_curve_sum.pdf", width = 9, height = 7)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 2
labelsize <- 1.8
namesize <- 1.8
annotatesize <- 1.8
graphlinewidth <- 2.5
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 5, 1, 1))

Cost <- function(x, c0 = 6, c1 = 2, c2 = -0.5){
  c0 + c1*x + c2*x^2
}

AvgCost <- function(x, c0 = 6, c1 = 2, c2 = -1){
  c0/x + c1 + c2*x 
}

AvgVarCost <- function(x, c0 = 6, c1 = 2, c2 = -1){
  c1 + c2*x 
}

MCost <- function(x, c1 = 2, c2 = -1){
  c1 + c2*x 
}

TwoFirmMCost <- function(x, c1 = 2, c2 = -1/2){
  c1 + 2*c2*x + 3*c3*x^2
}

FiveFirmMCost <- function(x, c1 = 2, c2 = -1/5){
  c1 + 2*c2*x + 3*c3*x^2
}

MarketMCost <- function(x, c1 = 2, c2 = 1/10){
  c1 + c2*x 
}

MarketMCost_2 <- function(x, c1 = 2.1, c2 = -1/15){
  c1 + 1.2*c2*x 
}

xlims <- c(0, 135)
#xlims <- c(0, 75) 
ylims <- c(0, 16)

npts <- 501 

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
ticksy <- c(0, 2, 10, ylims[2])
ylabels <- c(NA, expression(paste(p[min])), expression(paste(p)), NA)
ticksx <- c(0, 40, 80, xlims[2])
xlabels <- c(NA, NA, NA, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, MarketMCost(xx1, c2 = 1/10), col = COLA[5], lwd = graphlinewidth)
lines(xx1, MarketMCost(xx1, c2 = 1/5), col = COLA[5], lwd = graphlinewidth)

lines(xx10, MarketMCost(xx10), col = COLA[5], lwd = graphlinewidth)
lines(xx11, MarketMCost_2(xx11), col = COLA[6], lwd = graphlinewidth)
lines(xx12, MarketMCost_2(xx12), col = COLA[6], lwd = graphlinewidth)


#Label the axes
text(0.5*xlims[2], -2.2, expression(paste("Quantity, ", X)), xpd = TRUE, cex = axislabelsize) 
text(-12, 0.5*ylims[2], expression(paste("Price per unit, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label curves
text(73, 15.5, expression(paste(S[1])), cex = labelsize)
text(127, 15.5, expression(paste(S[2])), cex = labelsize)
Arrows(47, 11, 87, 11, col = "black", code = 2, lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

segments(40, 0, 40, MarketMCost(x = 40, c2 = 1/5), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, MarketMCost(x = 80, c2 = 1/10), 80, MarketMCost(x = 80, c2 = 1/10), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(80, 0, 80, MarketMCost(x = 80, c2 = 1/10), lty = 2, col = "gray" , lwd = segmentlinewidth)

Label Points for comparison
text(40, -1, expression(paste(X[1]^{S})), xpd = TRUE, cex = labelsize)
text(80, -1, expression(paste(X[2]^{S})), xpd = TRUE, cex = labelsize)

text(82, 12, expression(paste("With more firms\n the supply\n curve flattens")), cex = labelsize)

dev.off()
