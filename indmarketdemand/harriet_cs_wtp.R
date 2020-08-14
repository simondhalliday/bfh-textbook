require(shape)
pdf(file = "indmarketdemand/harriet_cs_wtp.pdf", width = 9, height = 7)

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
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(5, 6, 1, 1))

xlims <- c(0, 10.25)
ylims <- c(0, 20.5)

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
shares <- seq(0,10,1)
costs <- seq(20,0,-2)
prices <- c(10, 3.5)
#smoothed line
#segments(shares[1], costs[1] - 0.1, shares[7], costs[6] + 0.25, lty = 1, col = COLA[5] , lwd = segmentlinewidth)

ticksy <- c(0, seq(2,20,2), ylims[2])
ylabels <- c(seq(0,8,2), expression(paste(p == 10)), seq(12,20,2), NA)
ticksx <- c(0, seq(1,10,1), xlims[2])
#xlabels <- c(NA, expression(paste(x[I])), expression(paste(x[J])), expression(paste(x[K])), expression(paste(x[L])), expression(paste(x[M])), expression(paste(x[N])), NA)
#xlabels <- c(NA, expression(paste(x[I])), expression(paste(x[I] + x[J])), expression(paste(x[I] + x[J]+ x[K])), expression(paste(x[I] + x[J]+ x[K] + x[L])), expression(paste(x[I] + x[J]+ x[K] + x[L]+ x[M])), expression(paste(x[I] + x[J]+ x[K] + x[L]+ x[M] + x[N])), NA)
xlabels <- c(seq(0,4,1), expression(paste(x == 5)), seq(6,10,1),NA)

#Profit rectangles
#Before 'axis' call for layering purposes

#Firm i
rect(shares[1], costs[1], shares[2], prices[1], col = COLA[1], density = NULL, border = NA)
rect(shares[2], costs[2], shares[3], prices[1], col = COLA[1], density = NULL, border = NA)
rect(shares[3], costs[3], shares[4], prices[1], col = COLA[1], density = NULL, border = NA)
rect(shares[4], costs[4], shares[5], prices[1], col = COLA[1], density = NULL, border = NA)
rect(shares[5], costs[5], shares[6], prices[1], col = COLA[1], density = NULL, border = NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize - 0.3)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize - 0.3)


#Line for wtp
##Step 1
segments(shares[1], costs[1], shares[2], costs[1], lty = 1, col = COLA[5] , lwd = segmentlinewidth)
segments(shares[2], costs[1], shares[2], costs[2], lty = 1, col = COLA[5] , lwd = segmentlinewidth)
#text((shares[1] + shares[2])/2, costs[1] - 0.2, expression("Firm I"), cex = labelsize)

##Step 2
segments(shares[2], costs[2], shares[3], costs[2], lty = 1, col = COLA[5] , lwd = segmentlinewidth)
segments(shares[3], costs[2], shares[3], costs[3], lty = 1, col = COLA[5] , lwd = segmentlinewidth)
#text((shares[2] + shares[3])/2, costs[2] - 0.2, expression("Firm J"), cex = labelsize)

##Step 3
segments(shares[3], costs[3], shares[4], costs[3], lty = 1, col = COLA[5] , lwd = segmentlinewidth)
segments(shares[4], costs[3], shares[4], costs[4], lty = 1, col = COLA[5] , lwd = segmentlinewidth)
#text((shares[3] + shares[4])/2, costs[3] - 0.2, expression("Firm K"), cex = labelsize)

##Step 4
segments(shares[4], costs[4], shares[5], costs[4], lty = 1, col = COLA[5] , lwd = segmentlinewidth)
segments(shares[5], costs[4], shares[5], costs[5], lty = 1, col = COLA[5] , lwd = segmentlinewidth)
#text((shares[4] + shares[5])/2, costs[4] - 0.2, expression("Firm L"), cex = labelsize)

##Step 5
segments(shares[5], costs[5], shares[6], costs[5], lty = 1, col = COLA[5] , lwd = segmentlinewidth)
segments(shares[6], costs[5], shares[6], costs[6], lty = 1, col = COLA[5] , lwd = segmentlinewidth)
#text((shares[5] + shares[6])/2, costs[5] - 0.2, expression("Firm M"), cex = labelsize)

##Step 6
segments(shares[6], costs[6], shares[7], costs[6], lty = 1, col = COLA[5] , lwd = segmentlinewidth)
segments(shares[7], costs[6], shares[7], costs[7], lty = 1, col = COLA[5] , lwd = segmentlinewidth, xpd = TRUE)
#text((shares[6] + shares[7])/2, costs[6] - 0.2, expression("Firm N"), cex = labelsize)

##Step 7
segments(shares[7], costs[7], shares[8], costs[7], lty = 1, col = COLA[5] , lwd = segmentlinewidth)
segments(shares[8], costs[7], shares[8], costs[8], lty = 1, col = COLA[5] , lwd = segmentlinewidth, xpd = TRUE)

##Step 8
segments(shares[8], costs[8], shares[9], costs[8], lty = 1, col = COLA[5] , lwd = segmentlinewidth)
segments(shares[9], costs[8], shares[9], costs[9], lty = 1, col = COLA[5] , lwd = segmentlinewidth, xpd = TRUE)

##Step 9
segments(shares[9], costs[9], shares[10], costs[9], lty = 1, col = COLA[5] , lwd = segmentlinewidth)
segments(shares[10], costs[9], shares[10], costs[10], lty = 1, col = COLA[5] , lwd = segmentlinewidth, xpd = TRUE)

##Step 10
segments(shares[10], costs[10], shares[11], costs[10], lty = 1, col = COLA[5] , lwd = segmentlinewidth)
segments(shares[11], costs[10], shares[11], costs[11], lty = 1, col = COLA[5] , lwd = segmentlinewidth, xpd = TRUE)


# #Equilibrium price and quantity
# segments(0, 3, 3, 3, lty = 2, "gray" , lwd = segmentlinewidth)
# segments(3, 0, 3, 3, lty = 2, "gray" , lwd = segmentlinewidth)

#For market price
segments(5, 0, 5, 10, lty = 2, col = Grays[20] , lwd = segmentlinewidth)

#Label axes
#mtext(expression(paste("Quantity of output, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], -2.6, expression(paste("Quantity, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-1.3, 0.5*ylims[2], expression(paste("Price per unit ($), ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 
# 
# points(50, 10, pch = 16, col = "black", cex = 1.5)
# text(52, 10.5, expression(M))
# text(4.4, 0.7, expression("Buyers'"), cex = labelsize)
# text(4.4, 0.45, expression("Market Demand"), cex = labelsize)
text(7.5, 3, expression("Buyer's demand or"), cex = labelsize)
text(7.5, 2, expression("willingness to pay"), cex = labelsize)


# text(0.27, 2.6, expression("Equilibrium Price"), cex = labelsize)
# text(0.27, 2.4, expression("falls in this range"), cex = labelsize)
# Arrows(0.55, 2.1, 0.55, 2.9, col = "black", code =3, lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#Indicator lines
#Vertical lines
# segments(shares[2], 0, shares[2], costs[1], lty = 2, col = "darkgrey", lwd = segmentlinewidth)
# segments(shares[3], 0, shares[3], costs[2], lty = 2, col = "darkgrey", lwd = segmentlinewidth)
# segments(shares[4], 0, shares[4], costs[3], lty = 2, col = "darkgrey", lwd = segmentlinewidth)
# segments(shares[5], 0, shares[5], costs[4], lty = 2, col = "darkgrey", lwd = segmentlinewidth)
# segments(shares[6], 0, shares[6], costs[5], lty = 2, col = "darkgrey", lwd = segmentlinewidth)
# segments(shares[7], 0, shares[7], costs[6], lty = 2, col = "darkgrey", lwd = segmentlinewidth)
# 

#horizontal lines
segments(0, prices[1], xlims[2], prices[1], lty = 1, col = COLB[4], lwd = segmentlinewidth)
# segments(0, costs[2], shares[2], costs[2], lty = 2, col = "darkgrey", lwd = segmentlinewidth)
# segments(0, costs[3], shares[3], costs[3], lty = 2, col = "darkgrey", lwd = segmentlinewidth)
# segments(0, costs[4], shares[4], costs[4], lty = 2, col = "darkgrey", lwd = segmentlinewidth)
# segments(0, costs[5], shares[5], costs[5], lty = 2, col = "darkgrey", lwd = segmentlinewidth)
# segments(0, costs[6], shares[6], costs[6], lty = 2, col = "darkgrey", lwd = segmentlinewidth)

#Label economic profit
text(2 , prices[1] + 3, expression("Consumer"), cex = labelsize)
text(2 , prices[1] + 2, expression("surplus"), cex = labelsize)

text(8.5, prices[1] + 0.7, expression(paste("Market price, ", p == 10)), cex = labelsize)
#text((shares[1] + shares[4])/2 , prices[1] - 0.2, expression("Economic profit"), cex = labelsize)

dev.off()
