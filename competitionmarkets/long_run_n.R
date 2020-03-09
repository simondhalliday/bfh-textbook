require(shape)
pdf(file = "competitionmarkets/long_run_n.pdf", width = 9, height = 7)

#Set parameters for graphics
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

par(mar =  c(5, 8, 1, 1))

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
costs <- c(5, 10)

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

ticksy <- c(0, 10, cournotPrice(n = nstar(b = barriers[1]), c = costs[2]),  ylims[2])
ylabels <- c(NA, expression(c), expression(paste(p,"(n*)")), NA)
ticksx <- c(0, nstar(b = barriers[1]), xlims[2])
xlabels <- c(NA, expression(paste(n,"*")), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Lines for barrier graph
#lines(xx1, bte(n = xx1, b = barriers[3]), col = COLA[3], lty = 2, lwd = segmentlinewidth)
#lines(xx1, bte(n = xx1, b = barriers[2]), col = COLA[3], lty = 2, lwd = segmentlinewidth)
lines(xx1, bte(n = xx1, b = barriers[1]), col = COLA[3], lty = 2, lwd = graphlinewidth)

#Price with no barriers
# No Barrier --- Green
lines(xx1, cournotPrice(xx1, c = costs[2]), col = COLA[4], lwd = graphlinewidth) 


#Label axes
mtext(expression(paste("Number of firms, ", n)), side=1, line = 2.5, cex = axislabelsize)
text(-6, 0.5*ylims[2], expression(paste("Costs, Price, and Expected Price, ", list(c, p, hat(p)) )), xpd = TRUE, cex = axislabelsize, srt = 90) 

text(34, 12, expression(paste(p(n))), cex = labelsize)
text(34, 8, expression(paste(hat(p)*(n) == (1-b)*p(n) )), cex = labelsize)

#line for the marginal cost
segments(0, 10, xlims[2], 10, lty = 1, col = COLB[4] , lwd = graphlinewidth)

#segments for price and number of firms
segments(nstar(b = barriers[1]), 
         0, 
         nstar(b = barriers[1]), 
         cournotPrice(n = nstar(b = barriers[1]), c = costs[2]), 
         lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, 
         cournotPrice(n = nstar(b = barriers[1]), c = costs[2]), 
         nstar(b = barriers[1]), 
         cournotPrice(n = nstar(b = barriers[1]), c = costs[2]), 
         lty = 2, col = "gray", lwd = segmentlinewidth)


points(nstar(b = barriers[1]), 
       bte(n = nstar(b = barriers[1]), b = barriers[1]), 
       pch = 16, col = "black", cex = 1.5
       )

text(x = nstar(b = barriers[1]) - 0.5, 
     y = bte(n = nstar(b = barriers[1]), b = barriers[1]) - 0.5, 
     expression(paste(h)), cex = labelsize) 

points(nstar(b = barriers[1], c = costs[2]),
       cournotPrice(n = nstar(b = barriers[1], c = costs[2])),
       pch = 16, col = "black", cex = 1.5
       )

text(x = nstar(b = barriers[1]) + 0.5, 
     y = cournotPrice(n = nstar(b = barriers[1])) + 0.5, 
     expression(paste(e)), cex = labelsize) 


dev.off()