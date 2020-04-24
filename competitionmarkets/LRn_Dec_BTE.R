require(shape)
pdf(file = "competitionmarkets/LRn_Dec_BTE.pdf", width = 9, height = 7)

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

par(mar =  c(4, 5, 0.5, 0.5))

Profit <- function(n, pbar = 60, c = 10, beta = 0.5) {
  (pbar - c)^2 / (((n + 1)^2)*beta)
}

totaln <- function(pi, pbar = 60, c = 10, beta = 0.5){
  (pbar - c)/(sqrt(pi*beta)) - 1
}

cournotPrice <- function(n, pbar = 60, c = 10) {
  c + (1/(n+1))*(pbar - c)
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
barriers <- c(0.2, 0.45, 0.71, 0.73)

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

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
# ticksx <- seq(from = 0, to = xlims[2], by = 2)
# xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(0, 10, cournotPrice(n = nstar(b = barriers[1])), cournotPrice(n = nstar(b = barriers[2])), ylims[2])
ylabels <- c(NA, expression(c), expression(paste(p[L]^N)), expression(paste(p[H]^N)), NA)
ticksx <- c(0, nstar(b = barriers[2]), nstar(b = barriers[1]), xlims[2])
xlabels <- c(NA, expression(paste(n[H])), expression(paste(n[L])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Lines for barrier graph
lines(xx1, bte(n = xx1, b = barriers[1]), col = COLA[3], lty = 2, lwd = graphlinewidth)
lines(xx1, bte(n = xx1, b = barriers[2]), col = COLA[3], lty = 2, lwd = graphlinewidth)

#Price with no barriers
# No Barrier --- Greenbte(n = nstar(b = 0), b = 0)
lines(xx1, cournotPrice(xx1, c = 10), col = COLA[4], lwd = graphlinewidth) 

# lines(xx1, Profit(xx1, c = 0.25), col = COLA[4], lty = 2, lwd = segmentlinewidth)

#Label axes
#mtext(expression(paste("Number of firms, ", n)), side=1, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], -2, expression(paste("Number of firms, ", n)), xpd = TRUE, cex = axislabelsize) 
text(-3.8, 0.5*ylims[2], expression(paste("Costs, Price, and Expected Price, ", list(c, p, hat(p)) )), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Arrows(18, 6.8, 18, 4.5, col = "black", lty = 1, code = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# text(16, 5.75, expression(paste("Decrease in ", rho)), cex = labelsize)


# Eq labels
text(34, 12, expression(paste(p(n))), cex = labelsize)
text(32, 8, expression(paste(hat(p)[L]*(list(n, b[L])) == (1-b[L])*p(n) )), cex = labelsize)
text(32, 4.8, expression(paste(hat(p)[H]*(list(n, b[H])) == (1-b[H])*p(n) )), cex = labelsize)


# line for the marginal cost
segments(0, 10, xlims[2], 10, 
         lty = 1, col = COLB[4] , lwd = graphlinewidth
         )

#For the low price with low barriers to entry
segments(nstar(b = barriers[1]), 0, 
         nstar(b = barriers[1]), 
         cournotPrice(n = nstar(b = barriers[1])), 
         lty = 2, col = "gray", lwd = segmentlinewidth
         )

segments(0, cournotPrice(n = nstar(b = barriers[1])), 
         nstar(b = barriers[1]), 
         cournotPrice(n = nstar(b = barriers[1])), 
         lty = 2, col = "gray", lwd = segmentlinewidth
)

#For the high price with high barriers to entry
segments(nstar(b = barriers[2]), 0, 
         nstar(b = barriers[2]), 
         cournotPrice(n = nstar(b = barriers[2])), 
         lty = 2, col = "gray", lwd = segmentlinewidth
         )

segments(0, cournotPrice(n = nstar(b = barriers[2])), 
         nstar(b = barriers[2]), 
         cournotPrice(n = nstar(b = barriers[2])), 
         lty = 2, col = "gray", lwd = segmentlinewidth
)

#This should probably have a loop, but I haven't worked it out. 
#Two points for the low barriers
points(x = nstar(b = barriers[1]), 
       y = cournotPrice(n = nstar(b = barriers[1])), 
       pch = 16, col = "black", cex = 1.5)

text(x = nstar(b = barriers[1]) + 0.5, 
     y = cournotPrice(n = nstar(b = barriers[1])) + 0.5,
     expression(paste(e)), cex = labelsize) 


points(x = nstar(b = barriers[1]), 
       y = bte(n = nstar(b = barriers[1]), b = barriers[1]), 
       pch = 16, col = "black", cex = 1.5)

text(x = nstar(b = barriers[1]) - 0.5, 
     y = bte(n = nstar(b = barriers[1]), b = barriers[1]) - 0.5, 
     expression(paste(h)), cex = labelsize) 

#Two points for the high barriers
points(x = nstar(b = barriers[2]),
       y = cournotPrice(n = nstar(b = barriers[2])),
       pch = 16, col = "black", cex = 1.5)

text(x = nstar(b = barriers[2]) + 0.5, 
     y = cournotPrice(n = nstar(b = barriers[2])) + 0.5,
     expression(paste(f)), cex = labelsize) 

points(x = nstar(b = barriers[2]),
       y = bte(n = nstar(b = barriers[2]), b = barriers[2]),
       pch = 16, col = "black", cex = 1.5)

text(x = nstar(b = barriers[2]) - 0.5, 
     y = bte(n = nstar(b = barriers[2]), b = barriers[2]) - 0.5, 
     expression(paste(g)), cex = labelsize) 

# points(x = nstar(b = barriers[3]),
#        y = bte(n = nstar(b = barriers[3]), b = barriers[3]),
#        pch = 16, col = "black", cex = 1.5)
# points(x = nstar(b = barriers[4]), 
#        y = bte(n = nstar(b = barriers[4]),b = barriers[4]), 
#        pch = 16, col = "black", cex = 1.5)

dev.off()