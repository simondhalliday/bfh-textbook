require(ggplot2)
require(shape)
pdf(file = "society/upf_marshall.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(5, 5, 4, 2))
xlims <- c(68, 80)
ylims <- c(68, 80)

production <- function(ea, eb){
  (15 - (1/4)*eb)*ea
}

uA1 <- function(ea, eb){
  (15 - (1/4)*eb)*ea - (1/2)*(ea)^2  
}

uB1 <- function(ea, eb){
  (15 - (1/4)*ea)*eb - (1/2)*(eb)^2  
}

uAalt <- function(ea, production){
  production - (1/2)*(ea)^2  
}

eA <- function(gamma, delta = 12){
  delta/(1 + gamma)
}

eAstar <- function(gamma, delta = 12){
  delta/(1 + 2*gamma)
}

upf <- function(uA, intercept = 150, slope = 1){
  intercept - slope*uA
}


#Plot command 
plot(68, 68, xlim = xlims, ylim = ylims, 
     type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     axes = FALSE,
     # xlab = expression(paste("Bob's Payoff, ", u^B)),
     # ylab = expression(paste("Alfredo's Payoff, ", u^A)),
     #xaxt = "n", 
     #yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i"
)


#Customize ticks and labels for the plot
# ticksy <- seq(ylims[1], ylims[2], 5)
# ylabels <- seq(ylims[1], ylims[2], 5)
# ticksx <- seq(xlims[1], xlims[2], 5)
# xlabels <- seq(xlims[1], xlims[2], 5)
ticksy <- c(68,  72, 78, ylims[2])
ylabels <- c(68, 72, 78, 80)
ticksx <- c(68, 72, 78, xlims[2])
xlabels <- c(68, 72, 78, 80)
axis(1, at = ticksx, pos = 68, labels = xlabels)
axis(2, at = ticksy, pos = 68, labels = ylabels, las = 1)
mtext(expression(paste("Alfredo's payoff or utility, ", u^A)), side=1, line = 2.5, cex = axislabelsize)
text(xlims[1] - 1, ylims[2] - 0.5*(ylims[2] - ylims[1]), expression(paste("Bob's payoff or utility, ", u^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Rent polygon
xrent <- c(72, 78, 72, 72)
yrent <- c(72, 72, 78, 72)
polygon(xrent, yrent, col=COL[4], density=NULL, border = NA)

npts <- 500 
xx1 <- seq(xlims[1], 72, length.out = npts)
xx2 <- seq(72, 78, length.out = npts)
xx3 <- seq(78, xlims[2], length.out = npts)
lines(xx1, upfP1(xx1), col = COLA[4], lwd = segmentlinewidth, lty = 2)
lines(xx2, upfP1(xx2), col = COLA[4], lwd = graphlinewidth)
lines(xx3, upfP1(xx3), col = COLA[4], lwd = segmentlinewidth, lty = 2)



#Lines for Fallback positions
segments(65, 72, xlims[2], 72, lty = 2, col = "darkgrey", lwd = segmentlinewidth)
text(79.25, 72.25, expression(paste("A's fallback")), cex = labelsize)
segments(72, 65, 72, ylims[2], lty = 2, col = "darkgrey", lwd = segmentlinewidth)
text(73, 79, expression(paste("B's fallback")), cex = labelsize)

segments(68, 78, 72, 78, lty = 2, col = "darkgrey", lwd = segmentlinewidth)
segments(78, 68, 78, 72, lty = 2, col = "darkgrey", lwd = segmentlinewidth)


#Add points a, b, c and c
points(70, 78, pch = 16, col = "black", cex = 1.5)
text(69.75, 77.75, expression(paste("d")))
points(75, 75, pch = 16, col = "black", cex = 1.5)
text(75.25, 75.25, expression(paste("c")))
points(78, 70, pch = 16, col = "black", cex = 1.5)
text(77.75, 69.75, expression(paste("b")))
points(72, 72, pch = 16, col = "black", cex = 1.5)
text(71.75, 71.75, expression(paste("a")))

points(72, 78, pch = 16, col = "black", cex = 1.5)
text(71.75, 77.75, expression(paste("f")))

points(78, 72, pch = 16, col = "black", cex = 1.5)
text(77.75, 71.75, expression(paste("i")))




points(77, 73, pch = 16, col = "black", cex = 1.5)
text(77.25, 73.25, expression(paste("h")))

points(73, 77, pch = 16, col = "black", cex = 1.5)
text(73.25, 77.25, expression(paste("g")))


#Label utility possibilities frontier
text(76, 78, expression(paste("Feasible Utility")), cex = labelsize)
text(76, 77.5, expression(paste("Frontier")), cex = labelsize)
Arrows(76, 77.2, 76, 74.4, col = "black", lty = 1, lwd = 2, arr.type = "triangle")

#Arrows and economic rent label
text(74, 74,  expression(paste("Economic Rent")), cex = labelsize)

dev.off()

