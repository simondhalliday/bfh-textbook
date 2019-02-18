require(shape)
pdf(file = "competitionmarkets/LRn_Dec_Costs.pdf", width = 9, height = 7)

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

# Levels for the barriers to entry specified here
barriers <- c(0.2, 0.5, 0.71)
# Cost Levels
costs <- c(5, 10)

Profit <- function(n, pbar = 60, c = costs[2], beta = 0.5) {
  (pbar - c)^2 / (((n + 1)^2)*beta)
}

totaln <- function(pi, pbar = 60, c = costs[2], beta = 0.5){
  (pbar - c)/(sqrt(pi*beta)) - 1
}

cournotPrice <- function(n, pbar = 60, c = costs[2]) {
  c + (1/(n+1))*(pbar - c)
}

# Barriers to entry
bte <- function(n, b = 0) {
  cournotPrice(n)*(1-b)
}

# Hack function that gets around nested function scope error 
bte_unnested <- function(n, pbar = 60, c, b = 0){
  (c + (1/(n+1))*(pbar - c))*(1 - b)
}



nstar <- function(b, pbar = 60, c = costs[2]){
  (pbar*(1 - b) - c) / (b*c)
}

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

ticksy <- c(0, costs[1], bte(n = nstar(b = barriers[2], c = costs[1]), b = barriers[2]), costs[2], ylims[2])
# Asterik needs to be fixed
ylabels <- c(NA, expression(paste(c,"*")), expression(paste(P(n**b, "*"))), expression(c), NA) 
ticksx <- c(0, nstar(b = barriers[2]), nstar(b = barriers[2], c = costs[1]), xlims[2])
xlabels <- c(NA, expression(paste(n)), expression(paste(n**b)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

# Lines for barrier graph
#lines(xx1, bte(n = xx1, b = barriers[3]), col = COLA[3], lty = 2, lwd = segmentlinewidth)
#lines(xx1, bte(n = xx1, b = barriers[2]), col = COLA[3], lty = 2, lwd = segmentlinewidth)

# P(n^b)
lines(xx1, bte(n = xx1, b = barriers[2]), col = COLA[3], lty = 2, lwd = graphlinewidth) 
# P(n^{br})
lines(xx1, bte_unnested(n = xx1, c = costs[1], b = barriers[2]), col = COLA[3], lty = 1, lwd = graphlinewidth) 

#Label axes
mtext(expression(paste("Number of firms, ", n)), side=1, line = 2.5, cex = axislabelsize)
text(-4, 0.5*ylims[2], expression(paste("Costs, Price, and Expected Price, ", list(c, p, hat(p)) )), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Arrows(18, 6.8, 18, 4.5, col = "black", lty = 1, code = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# text(16, 5.75, expression(paste("Decrease in ", rho)), cex = labelsize)


#text(5, 11, expression(paste(c)), cex = labelsize)
# text(19, 4.4, expression(paste(rho[1])), cex = labelsize)

#Arrows(4, 9, 7.8, 9, col = "black", lty = 1, code = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# text(5.7, 10, expression(paste("Decrease in ", c)), cex = labelsize)

#text(34, 12, expression(paste(p(n))), cex = labelsize)
text(34, 7, expression(paste(hat(p)*(n) == (1-b)*p(n) )), cex = labelsize)

#line for the marginal cost
segments(0, 10, xlims[2], 10, 
         lty = 2, col = COLB[4], lwd = graphlinewidth
         )

segments(nstar(b = barriers[2]), 0, nstar(b = barriers[2]), bte(n = nstar(b = barriers[2]), b = barriers[2]),
         lty = 2, col = "gray", lwd = segmentlinewidth
         )

segments(nstar(b = barriers[2], c = costs[1]), 0, nstar(b = barriers[2], c = costs[1]), bte(n = nstar(b = barriers[2], c = costs[1]), b = barriers[2]),
         lty = 2, col = "gray", lwd = segmentlinewidth
         )

segments(0, bte(n = nstar(b = barriers[2], c = costs[1]), b = barriers[2]), nstar(b = barriers[2], c = costs[1]), bte(n = nstar(b = barriers[2], c = costs[1]), b = barriers[2]),
         lty = 2, col = "grey", lwd = segmentlinewidth
         )

segments(0, 5, xlims[2], 5, lty = 1, col = COLB[4] , lwd = graphlinewidth)


#This should probably have a loop, but I haven't worked it out. 
points(nstar(b = barriers[2], c = costs[2]), bte(n = nstar(b = barriers[2], c = costs[2]), b = barriers[2]),
       pch = 16, col = "black", cex = 1.5
       )

points(nstar(b = barriers[2], c = costs[1]), bte(n = nstar(b = barriers[2], c = costs[1]), b = barriers[2]), 
       pch = 16, col = "black", cex = 1.5
       )
# points(nstar(b = barriers[3]), bte(n = nstar(b = barriers[3]), b = barriers[3]), pch = 16, col = "black", cex = 1.5)


dev.off()