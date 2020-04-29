require(shape)
pdf(file = "firmmarketsupply/supply_stepwise_firmk.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 2
labelsize <- 1.8
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(5, 5, 0.2, 0.2))

xlims <- c(0, 4.1)
ylims <- c(0, 3)

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
shares <- c(0, 2, 4, 8, 10, 13, 15)
costs <- c(1.25, 1.75, 2.25, 3, 3.25, 3.75, 4.25)
#smoothed line
#segments(shares[1], costs[1] - 0.1, shares[7], costs[6] + 0.25, lty = 1, col = COLB[4] , lwd = segmentlinewidth)

ticksy <- c(0, costs[1], costs[2], costs[3], ylims[2])
ylabels <- c(NA, expression(paste(c^I)), expression(paste(c^J)), expression(paste(c^K)), NA)
ticksx <- c(0, shares[4] - shares[3], xlims[2])
#xlabels <- c(NA, expression(paste(x[I])), expression(paste(x[J])), expression(paste(x[K])), expression(paste(x[L])), expression(paste(x[M])), expression(paste(x[N])), NA)
#xlabels <- c(NA, expression(paste(x[I])), expression(paste(x[I] + x[J])), expression(paste(x[I] + x[J]+ x[K])), expression(paste(x[I] + x[J]+ x[K] + x[L])), expression(paste(x[I] + x[J]+ x[K] + x[L]+ x[M])), expression(paste(x[I] + x[J]+ x[K] + x[L]+ x[M] + x[N])), NA)
xlabels <- c(NA, expression(paste(underline(x)^K)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)





#Line for Supply
##Firm J
segments(shares[1], costs[3], shares[4] - shares[3], costs[3], lty = 1, col = COLB[4] , lwd = segmentlinewidth)
segments(shares[4] - shares[3], costs[3], shares[4] - shares[3], ylims[2], lty = 1, col = COLB[4] , lwd = segmentlinewidth)
#text((shares[1] + shares[2])/2, costs[1] - 0.2, expression("Firm I"), cex = labelsize)

##Firm J
# segments(shares[2], costs[2], shares[3], costs[2], lty = 1, col = COLB[4] , lwd = segmentlinewidth)
# segments(shares[3], costs[2], shares[3], costs[3], lty = 1, col = COLB[4] , lwd = segmentlinewidth)
# text((shares[2] + shares[3])/2, costs[2] - 0.2, expression("Firm J"), cex = labelsize)

##Firm 3
# segments(shares[3], costs[3], shares[4], costs[3], lty = 1, col = COLB[4] , lwd = segmentlinewidth)
# segments(shares[4], costs[3], shares[4], costs[4], lty = 1, col = COLB[4] , lwd = segmentlinewidth)
# text((shares[3] + shares[4])/2, costs[3] - 0.2, expression("Firm K"), cex = labelsize)

##Firm 4
# segments(shares[4], costs[4], shares[5], costs[4], lty = 1, col = COLB[4] , lwd = segmentlinewidth)
# segments(shares[5], costs[4], shares[5], costs[5], lty = 1, col = COLB[4] , lwd = segmentlinewidth)
# text((shares[4] + shares[5])/2, costs[4] - 0.2, expression("Firm L"), cex = labelsize)

##Firm 5
# segments(shares[5], costs[5], shares[6], costs[5], lty = 1, col = COLB[4] , lwd = segmentlinewidth)
# segments(shares[6], costs[5], shares[6], costs[6], lty = 1, col = COLB[4] , lwd = segmentlinewidth)
# text((shares[5] + shares[6])/2, costs[5] - 0.2, expression("Firm M"), cex = labelsize)

##Firm 6
# segments(shares[6], costs[6], shares[7], costs[6], lty = 1, col = COLB[4] , lwd = segmentlinewidth)
# segments(shares[7], costs[6], shares[7], costs[7], lty = 1, col = COLB[4] , lwd = segmentlinewidth, xpd = TRUE)
# text((shares[6] + shares[7])/2, costs[6] - 0.2, expression("Firm N"), cex = labelsize)

# #Equilibrium price and quantity
# segments(0, 3, 3, 3, lty = 2, "gray" , lwd = segmentlinewidth)
# segments(3, 0, 3, 3, lty = 2, "gray" , lwd = segmentlinewidth)

#For market price
# segments(0, 2, 1, 2, lty = 2, "gray" , lwd = segmentlinewidth)

#Label axes
#mtext(expression(paste("Quantity of output, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], -0.3, expression(paste("Firm K's output, ", x^K)), xpd = TRUE, cex = axislabelsize) 
text(-.35, 0.5*ylims[2], expression(paste("Price per unit and cost ($), ", p, " and ", c)), xpd = TRUE, cex = axislabelsize, srt = 90) 
# 
# points(50, 10, pch = 16, col = "black", cex = 1.5)
# text(52, 10.5, expression(M))
# text(4.4, 0.7, expression("Buyers'"), cex = labelsize)
# text(4.4, 0.45, expression("Market Demand"), cex = labelsize)
text(3.7, 2.8, expression("Firm K's"), cex = labelsize)
text(3.7, 2.65, expression("supply"), cex = labelsize)

# text(0.27, 2.6, expression("Equilibrium Price"), cex = labelsize)
# text(0.27, 2.4, expression("falls in this range"), cex = labelsize)
# Arrows(0.55, 2.1, 0.55, 2.9, col = "black", code =3, lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#Indicator lines
#Vertical lines
segments(shares[4] - shares[3], 0, shares[4] - shares[3], costs[3], lty = 2, col = "darkgrey", lwd = segmentlinewidth)
# segments(shares[3], 0, shares[3], costs[2], lty = 2, col = "darkgrey", lwd = segmentlinewidth)
# segments(shares[4], 0, shares[4], costs[3], lty = 2, col = "darkgrey", lwd = segmentlinewidth)
# segments(shares[5], 0, shares[5], costs[4], lty = 2, col = "darkgrey", lwd = segmentlinewidth)
# segments(shares[6], 0, shares[6], costs[5], lty = 2, col = "darkgrey", lwd = segmentlinewidth)
# segments(shares[7], 0, shares[7], costs[6], lty = 2, col = "darkgrey", lwd = segmentlinewidth)


#horizontal lines
segments(0, costs[1], xlims[2], costs[1], lty = 2, col = "darkgrey", lwd = segmentlinewidth)
segments(0, costs[2], xlims[2], costs[2], lty = 2, col = "darkgrey", lwd = segmentlinewidth)
# segments(0, costs[4], shares[4], costs[4], lty = 2, col = "darkgrey", lwd = segmentlinewidth)
# segments(0, costs[5], shares[5], costs[5], lty = 2, col = "darkgrey", lwd = segmentlinewidth)
# segments(0, costs[6], shares[6], costs[6], lty = 2, col = "darkgrey", lwd = segmentlinewidth)

dev.off()
