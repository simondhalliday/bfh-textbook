#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "firmmarketsupply/supply_upward_market_cube.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 7, 4, 4))

AvgRevenue <- function(x, rmax = 12, xmax = 12){
  rmax - (rmax/xmax)*x
}

MRevenue <- function(x, rmax = 12, xmax = 12){
  rmax - 2*(rmax/xmax)*x
}

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
  c1 + 2*c2*x + 3*c3*x^2
}

TwoFirmMCost <- function(x, c1 = 2, c2 = -1/2, c3 = 0.2/4){
  c1 + 2*c2*x + 3*c3*x^2
}

FiveFirmMCost <- function(x, c1 = 2, c2 = -1/5, c3 = 0.2/25){
  c1 + 2*c2*x + 3*c3*x^2
}

MarketMCost <- function(x, c1 = 2, c2 = -1/10, c3 = 0.2/100){
  c1 + 2*c2*x + 3*c3*x^2
}

xlims <- c(0, 75)
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
ticksy <- c(0, MCost(x = 2.5), ylims[2])
ylabels <- c(NA,  expression(paste(p[min]^{SR})), NA)
ticksx <- c(0, 2.5, 2.5*2, 2.5*5, 2.5*10, xlims[2])
xlabels <- c(NA, expression(paste(x[m]^{1})),  expression(paste(x[m]^{2})),  expression(paste(x[m]^{5})), expression(paste(x[m]^{Market})), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(5/2, 3.63514, length.out = npts)
xx3 <- seq(3.63514, xlims[2], length.out = npts)
xx4 <- seq(3.63514*2, xlims[2], length.out = npts)
xx5 <- seq(3.63514*5, xlims[2], length.out = npts)
xx6 <- seq(3.63514*10, xlims[2], length.out = npts)


xx7 <- seq(2.5, 3.63514, length.out = npts)
xx8 <- seq(2.5*2, 3.63514*2, length.out = npts)
xx9 <- seq(2.5*5, 3.63514*5, length.out = npts)
xx10 <- seq(2.5*10, 3.63514*10, length.out = npts)


#lines(xx1, AvgCost(xx1), col = COLA[6], lwd = graphlinewidth)
#lines(xx1, AvgVarCost(xx1), col = COLA[5], lwd = graphlinewidth)
lines(xx3, MCost(xx3), col = COLA[2], lwd = graphlinewidth)
lines(xx4, TwoFirmMCost(xx4), col = COLA[3], lwd = graphlinewidth)
lines(xx5, FiveFirmMCost(xx5), col = COLA[4], lwd = graphlinewidth)
lines(xx6, MarketMCost(xx6), col = COLA[5], lwd = graphlinewidth)
lines(xx7, MCost(xx7), col = COLA[2], lwd = graphlinewidth)
lines(xx8, TwoFirmMCost(xx8), col = COLA[3], lwd = graphlinewidth)
lines(xx9, FiveFirmMCost(xx9), col = COLA[4], lwd = graphlinewidth)
lines(xx10, MarketMCost(xx10), col = COLA[5], lwd = graphlinewidth)

#Label the axes
mtext(expression(paste("Quantity of output, ", x)), side = 1, line = 2.5, cex = axislabelsize)
text(-7, 0.5*ylims[2], expression(paste("Price per unit of output, ", p[x])), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label curves
# text(6.1, 4.9, expression(paste(ac(x))), cex = labelsize)
# text(7.5, 4.9, expression(paste(avc(x))), cex = labelsize)
text(8, 15.5, expression(paste(S[1])), cex = labelsize)
text(15, 15.5, expression(paste(S[2])), cex = labelsize)
text(35, 15.5, expression(paste(S[5])), cex = labelsize)
text(62, 15.5, expression(paste(S[Market])), cex = labelsize)

#text(9, 8.5, expression(paste("curve for")), cex = labelsize)
#text(9, 9, expression(paste("1 firm")), cex = labelsize)

#text(8, 8, expression(paste(mc(x))), cex = labelsize)


text(42, 10, expression(paste("With more firms")), cex = labelsize)
text(42, 9.2, expression(paste("the supply curve")), cex = labelsize)
text(42, 8.4, expression(paste("flattens")), cex = labelsize)
Arrows(6.5 ,11, 10, 11, col = "black", code = 2, lty = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
Arrows(13 ,11, 27, 11, col = "black", code = 2, lty = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
Arrows(31 ,11, 57, 11, col = "black", code = 2, lty = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
#Draw segments for total costs
# segments(0, MCost(x = 5/2), 3.63514, MCost(x = 5/2), lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(5/2, 0, 5/2, MCost(x = 5/2), lty = 2, col = "gray" , lwd = segmentlinewidth)
# 

#segments(0, MarketMCost(x = 3.63514*10), 3.63514*10, MarketMCost(x = 3.63514*10), lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(3.63514, 0, 3.63514, MCost(x = 3.63514), lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(3.63514*2, 0, 3.63514*2, TwoFirmMCost(x = 3.63514*2), lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(3.63514*5, 0, 3.63514*5, FiveFirmMCost(x = 3.63514*5), lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(36.3514, 0, 36.3514, MarketMCost(x = 36.3514), lty = 2, col = "gray" , lwd = segmentlinewidth)

segments(0, MarketMCost(x = 2.5*10), 2.5*10, MarketMCost(x = 2.5*10), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(2.5, 0, 2.5, MCost(x = 2.5), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(2.5*2, 0, 2.5*2, TwoFirmMCost(x = 2.5*2), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(2.5*5, 0, 2.5*5, FiveFirmMCost(x = 2.5*5), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(2.5*10, 0, 2.5*10, MarketMCost(x = 2.5*10), lty = 2, col = "gray" , lwd = segmentlinewidth)


#Label Points for comparison
# points(3.63514, MCost(x = 3.63514), pch = 16, col = "black", cex = 1.5)
# text(3.63514 + 1, MCost(x = 3.63514) - 0.65, expression(a), cex = labelsize)
# points(3.63514*2, TwoFirmMCost(x = 3.63514*2), pch = 16, col = "black", cex = 1.5)
# text(3.63514*2 + 1, TwoFirmMCost(x = 3.63514*2) - 0.65, expression(b), cex = labelsize)
# points(3.63514*5, FiveFirmMCost(x = 3.63514*5), pch = 16, col = "black", cex = 1.5)
# text(3.63514*5 + 1, FiveFirmMCost(x = 3.63514*5) - 0.65, expression(c), cex = labelsize)
# points(3.63514*10, MarketMCost(x = 3.63514*10), pch = 16, col = "black", cex = 1.5)
# text(3.63514*10 + 1, MarketMCost(x = 3.63514*10) - 0.65, expression(d), cex = labelsize)

# points(5/2, MCost(x = 5/2), pch = 16, col = "black", cex = 1.5)
# text(5/2 + 0.15, MCost(x = 5/2) - 0.15, expression(b), cex = labelsize)

#Arrow to for sr losses
# text(1.5, 2.22, expression(paste(p[min]^{SR} < p, phantom() < p[min]^{LR})), cex = labelsize)
# text(1.5, 2, expression(paste("short-run losses")), cex = labelsize)
# text(1.5, 1.78, expression(paste("firm supplies x")), cex = labelsize)
# text(1.5, 1.6, expression(paste("in short run")), cex = labelsize)
# text(1.5, 1.38, expression(paste("not long run")), cex = labelsize)
# 



#Arrow to for sr shutdown
# text(5.5, 0.7, expression(paste(p < p[min]^{SR})), cex = labelsize)
# text(5.5, 0.45, expression(paste("unable to cover fixed costs")), cex = labelsize)
# text(5.5, 0.25, expression(paste("firm shuts down")), cex = labelsize)
# Arrows(3.8, 0.1, 3.8, MCost(x = 5/2) - 0.1, col = "black", code = 3, lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


dev.off()
