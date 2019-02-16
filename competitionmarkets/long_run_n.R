require(shape)
pdf(file = "competitionmarkets/long_run_n.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(6, 6, 4, 4))

Profit <- function(n, pbar = 60, c = 10, beta = 0.5) {
  (pbar - c)^2 / (((n + 1)^2)*beta)
}

totaln <- function(pi, pbar = 60, c = 10, beta = 0.5){
  (pbar - c)/(sqrt(pi*beta)) - 1
}

cournotPrice <- function(n, pbar = 60, c1 = 10) {
  c1 + (1/(n+1))*(pbar - c1)
}


# Barriers to entry
# Inputs: Probability of BTE (b) and number of firms
# Outputs: Price Level at some n
bte <- function(n, b = 0) {
  cournotPrice(n)*(1-b)
}

nstar <- function(b, pbar = 60, c = 10){
(pbar*(1 - b) - c) / (b*c)
}

#Levels for the barriers to entry specified here
barriers <- c(0.2, 0.5, 0.71)

xlims <- c(0, 40)
ylims <- c(0, 20)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

ticksy <- seq(from = 0, to = ylims[2], by = 2)
ylabels <- seq(from = 0, to = ylims[2], by = 2)
ticksx <- seq(from = 0, to = xlims[2], by = 2)
xlabels <- seq(from = 0, to = xlims[2], by = 2)
# ticksy <- c(0, NA, 3.2, ylims[2])
# ylabels <- c(NA, NA, NA, NA)
# ticksx <- c(0, 4.5, NA, NA, NA, xlims[2])
# xlabels <- c(NA, expression(paste(n^b)), NA, NA, NA, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Lines for barrier graph
lines(xx1, bte(n = xx1, b = barriers[3]), col = COLA[3], lty = 2, lwd = segmentlinewidth)
lines(xx1, bte(n = xx1, b = barriers[2]), col = COLA[3], lty = 2, lwd = segmentlinewidth)
lines(xx1, bte(n = xx1, b = barriers[1]), col = COLA[3], lty = 2, lwd = segmentlinewidth)

#Price with no barriers
lines(xx1, cournotPrice(xx1, c = 10), col = COLA[4], lwd = graphlinewidth) # No Barrier --- Green

# lines(xx1, Profit(xx1, c = 0.25), col = COLA[4], lty = 2, lwd = segmentlinewidth)

#Label axes
mtext(expression(paste("Number of firms, ", n)), side=1, line = 2.5, cex = axislabelsize)
text(-2, 0.5*ylims[2], expression(paste("Cost and Profit, ", list(rho, pi) )), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Arrows(18, 6.8, 18, 4.5, col = "black", lty = 1, code = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# text(16, 5.75, expression(paste("Decrease in ", rho)), cex = labelsize)


text(19, 3.6, expression(paste(c)), cex = labelsize)
# text(19, 4.4, expression(paste(rho[1])), cex = labelsize)

#Arrows(4, 9, 7.8, 9, col = "black", lty = 1, code = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# text(5.7, 10, expression(paste("Decrease in ", c)), cex = labelsize)

# text(3.1, 15, expression(paste(pi[1])), cex = labelsize)
# text(6.7, 15, expression(paste(pi[2])), cex = labelsize)

#line for the marginal cost
segments(0, 10, xlims[2], 10, lty = 1, col = COLB[4] , lwd = segmentlinewidth)
# segments(0, 4, xlims[2], 4, lty = 2, col = COLB[3], lwd = segmentlinewidth)
# segments(4.5, 0, 4.5, 6.8, lty = 2, col = "grey", lwd = segmentlinewidth)
# segments(0, 6.8, 4.5, 6.8, lty = 2, col = "grey", lwd = segmentlinewidth)
# 

#This should probably have a loop, but I haven't worked it out. 
points(nstar(b = barriers[1]), bte(n = nstar(b = barriers[1]), b = barriers[1]), pch = 16, col = "black", cex = 1.5)
points(nstar(b = barriers[2]), bte(n = nstar(b = barriers[2]), b = barriers[2]), pch = 16, col = "black", cex = 1.5)
points(nstar(b = barriers[3]), bte(n = nstar(b = barriers[3]), b = barriers[3]), pch = 16, col = "black", cex = 1.5)


# text(80, 12.5, expression(paste("Excess Supply at ", p^H)), cex = labelsize)
# 
# Arrows(62, 5.5, 82, 5.5, col = "black", lty = 1, code = 3, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# text(72, 4.75, expression(paste("Excess")), cex = labelsize)
# text(72, 3.75, expression(paste("Demand")), cex = labelsize)
# text(72, 2.75, expression(paste("at ", p^L)), cex = labelsize)
# segments(6, 0, 6, 4, lty = 2, col = "gray" , lwd = segmentlinewidth)
# points(6, 4, pch = 16, col = "black", cex = 1.5)


# segments(9.6, 0, 9.6, 7, lty = 2, col = "gray" , lwd = segmentlinewidth)
# points(9.6, 7, pch = 16, col = "black", cex = 1.5)

# segments(13, 0, 13, 4, lty = 2, col = "gray" , lwd = segmentlinewidth)
# points(13, 4, pch = 16, col = "black", cex = 1.5)

dev.off()