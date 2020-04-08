#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "firmmarketsupply/supply_upward_market_cube_flat.pdf", width = 9, height = 7)

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

Cost <- function(x, c0 = 6, c1 = 2, c2 = -0.5, c3 = 0.3){
  c0 + c1*x + c2*x^2 + c3*x^3 
}

AvgCost <- function(x, c0 = 6, c1 = 2, c2 = -1, c3 = 0.2){
  c0/x + c1 + c2*x + c3*x^2
}

AvgVarCost <- function(x, c0 = 6, c1 = 2, c2 = -1, c3 = 0.2){
  c1 + c2*x + c3*x^2
}

MCost <- function(x, c1 = 2, c2 = -1, c3 = 0.2){
  c1 + 2*c2*x + 2.6*c3*x^2
}

TwoFirmMCost <- function(x, c1 = 2, c2 = -1/2, c3 = 0.2/4){
  c1 + 2*c2*x + 3*c3*x^2
}

FiveFirmMCost <- function(x, c1 = 2, c2 = -1/5, c3 = 0.2/25){
  c1 + 2*c2*x + 3*c3*x^2
}

MarketMCost <- function(x, c1 = 3, c2 = -1/10, c3 = 0.2/100){
  c1 + 1.8*c2*x + 2*c3*x^2
}

MarketMCost_2 <- function(x, c1 = 2.1, c2 = -1/15, c3 = 0.2/220){
  c1 + 1.2*c2*x + 1.5*c3*x^2
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
ticksy <- c(0, 1, ylims[2])
ylabels <- c(NA,  expression(paste(p[min])), NA)
ticksx <- c(0, 2.5*15, xlims[2])
xlabels <- c(NA, NA, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(5/2, 3.63514, length.out = npts)
xx3 <- seq(3.63514, xlims[2], length.out = npts)
xx4 <- seq(3.63514*2, xlims[2], length.out = npts)
xx5 <- seq(3.63514*5, xlims[2], length.out = npts)
xx6 <- seq(3.63514*10, xlims[2], length.out = npts)
xx11 <- seq(3.63514*15, xlims[2], length.out = npts)

xx7 <- seq(2.5, 3.63514, length.out = npts)
xx8 <- seq(2.5*2, 3.63514*2, length.out = npts)
xx9 <- seq(2.5*5, 3.63514*5, length.out = npts)
xx10 <- seq(2.5*10, 3.63514*10, length.out = npts)
xx12 <- seq(2.5*15, 3.63514*15, length.out = npts)

#lines(xx5, FiveFirmMCost(xx5), col = COLA[4], lwd = graphlinewidth)
#lines(xx7, MCost(xx7), col = COLA[2], lwd = graphlinewidth)
#lines(xx8, TwoFirmMCost(xx8), col = COLA[3], lwd = graphlinewidth)
#lines(xx9, FiveFirmMCost(xx9), col = COLA[4], lwd = graphlinewidth)
lines(xx6, MarketMCost(xx6), col = COLA[5], lwd = graphlinewidth)
lines(xx10, MarketMCost(xx10), col = COLA[5], lwd = graphlinewidth)

lines(xx11, MarketMCost_2(xx11), col = COLA[6], lwd = graphlinewidth)
lines(xx12, MarketMCost_2(xx12), col = COLA[6], lwd = graphlinewidth)


#Label the axes
mtext(expression(paste("Quantity, ", X)), side = 1, line = 2.5, cex = axislabelsize)
text(-9, 0.5*ylims[2], expression(paste("Price per unit, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label curves
text(78, 15.5, expression(paste(S[1])), cex = labelsize)
text(128, 15.5, expression(paste(S[2])), cex = labelsize)
Arrows(74, 11, 112, 11, col = "black", code = 2, lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

segments(25, 0, 25, MarketMCost(x = 2.5*10), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, MarketMCost(x = 2*10), 2.5*15, MarketMCost(x = 2.5*10), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(2.5*15, 0, 2.5*15, MarketMCost(x = 2.5*10), lty = 2, col = "gray" , lwd = segmentlinewidth)

#Label Points for comparison
text(2*12, -1, expression(paste(X[1]^{S})), xpd = TRUE, cex = labelsize)
text(2.5*15, -1, expression(paste(X[2]^{S})), xpd = TRUE, cex = labelsize)

text(100, 12, expression(paste("With more firms\n the supply\n curve flattens")), cex = labelsize)

dev.off()
