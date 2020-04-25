require(shape)
pdf(file = "firmmarketsupply/supply_stepwise.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(5, 5, 2, 2))

xlims <- c(0, 15)
ylims <- c(0, 5)

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

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
# ticksx <- seq(from = 0, to = xlims[2], by = 2)
# xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(0, 3, 5, ylims[2])
ylabels <- c(NA,  expression(paste(p,"*")),  expression(paste(bar(p) )), NA)
ticksx <- c(0, 1, 2, 3, 4, 5, xlims[2])
xlabels <- c(NA, 1, 2, expression(paste(x,"*" == 3)), 4, 5, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)





#Draw the polygon for consumer surplus
# xpoly4 <- c(0, 2, 2, 1, 1, 0, 0)
# ypoly4 <- c(3, 3, 4, 4, 5, 5, 3)
# polygon(x = xpoly4, y = ypoly4, col=COLA[1], density=NULL, border = NA)
# text(1, 3.5, expression("Consumer Surplus"), cex = labelsize)

#Draw the polygon for producer surplus
# xpoly3 <- c(0, 2, 2, 1, 1, 0, 0)
# ypoly3 <- c(3, 3, 2, 2, 1, 1, 3)
# polygon(x = xpoly3, y = ypoly3, col=COLB[1], density=NULL, border = NA)
# text(1, 2.5, expression("Producer Surplus"), cex = labelsize)

shares <- c(0, 2, 4, 8, 10, 13, 15)
costs <- c(1, 2, 2.5, 3, 3.25, 4, 4.5)

#Line for Supply
##Firm 1
segments(shares[1], costs[1], shares[2], costs[1], lty = 1, col = COLB[4] , lwd = segmentlinewidth)
segments(shares[2], costs[1], shares[2], costs[2], lty = 1, col = COLB[4] , lwd = segmentlinewidth)

##Firm 2
segments(shares[2], costs[2], shares[3], costs[2], lty = 1, col = COLB[4] , lwd = segmentlinewidth)
segments(shares[3], costs[2], shares[3], costs[3], lty = 1, col = COLB[4] , lwd = segmentlinewidth)

##Firm 3
segments(shares[3], costs[3], shares[4], costs[3], lty = 1, col = COLB[4] , lwd = segmentlinewidth)
segments(shares[4], costs[3], shares[4], costs[4], lty = 1, col = COLB[4] , lwd = segmentlinewidth)

##Firm 4
segments(shares[4], costs[4], shares[5], costs[4], lty = 1, col = COLB[4] , lwd = segmentlinewidth)
segments(shares[5], costs[4], shares[5], costs[5], lty = 1, col = COLB[4] , lwd = segmentlinewidth)

##Firm 5
segments(shares[5], costs[5], shares[6], costs[5], lty = 1, col = COLB[4] , lwd = segmentlinewidth)
segments(shares[6], costs[5], shares[6], costs[6], lty = 1, col = COLB[4] , lwd = segmentlinewidth)

##Firm 6
segments(shares[6], costs[6], shares[7], costs[6], lty = 1, col = COLB[4] , lwd = segmentlinewidth)
segments(shares[7], costs[6], shares[7], costs[7], lty = 1, col = COLB[4] , lwd = segmentlinewidth)


# #Equilibrium price and quantity
# segments(0, 3, 3, 3, lty = 2, "gray" , lwd = segmentlinewidth)
# segments(3, 0, 3, 3, lty = 2, "gray" , lwd = segmentlinewidth)

#For market price
# segments(0, 2, 1, 2, lty = 2, "gray" , lwd = segmentlinewidth)

#Label axes
#mtext(expression(paste("Quantity of output, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], -0.6, expression(paste("Quantity of output, ", X)), xpd = TRUE, cex = axislabelsize) 
text(-1.5, 0.5*ylims[2], expression(paste("Price per unit ($), ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 
# 
# points(50, 10, pch = 16, col = "black", cex = 1.5)
# text(52, 10.5, expression(M))
# text(4.4, 0.7, expression("Buyers'"), cex = labelsize)
# text(4.4, 0.45, expression("Market Demand"), cex = labelsize)
text(12, 3, expression("Sellers'"), cex = labelsize)
text(12, 2.4, expression("Market Supply"), cex = labelsize)

# text(0.27, 2.6, expression("Equilibrium Price"), cex = labelsize)
# text(0.27, 2.4, expression("falls in this range"), cex = labelsize)
# Arrows(0.55, 2.1, 0.55, 2.9, col = "black", code =3, lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

dev.off()

#Not relevant to this figure any more
#Line for Demand
# segments(0, 5, 1, 5, lty = 1, col = COLA[4] , lwd = segmentlinewidth)
# segments(1, 5, 1, 4, lty = 1, col = COLA[4] , lwd = segmentlinewidth)
# segments(1, 4, 2, 4, lty = 1, col = COLA[4] , lwd = segmentlinewidth)
# segments(2, 4, 2, 3, lty = 1, col = COLA[4] , lwd = segmentlinewidth)
# segments(2, 3.04, 3.04, 3.04, lty = 1, col = COLA[4] , lwd = segmentlinewidth)
# segments(3.04, 3.04, 3.04, 2, lty = 1, col = COLA[4] , lwd = segmentlinewidth)
# segments(3, 2, 4, 2, lty = 1, col = COLA[4] , lwd = segmentlinewidth)
# segments(4, 2, 4, 1, lty = 1, col = COLA[4] , lwd = segmentlinewidth)
# segments(4, 1, 5, 1, lty = 1, col = COLA[4] , lwd = segmentlinewidth)
# segments(5, 1, 5, 0, lty = 1, col = COLA[4] , lwd = segmentlinewidth)
