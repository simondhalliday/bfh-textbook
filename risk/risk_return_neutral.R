require(ggplot2)
require(shape)
pdf(file = "risk/risk_return_neutral.pdf", width = 9, height = 7)

# Set parameters for graphics
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
grays <- gray.colors(25, start = 1, end = 0)

a <- c(2, 4, 6)
par(mar =  c(5, 6, 4, 2))
xlims <- c(0, 15)
ylims <- c(0, 18)

riskreturn <- function(g, int1 = 14, int2 = 4, coeff = 1/3){
  int1 - (int2 - (coeff)*g)^2
}

uA <- function(y, g, slope = 0.5){
  y+ y*g  - slope*g^2
}

indiffA <- function(g, intercept = 3, slope = 0.125){
  intercept  + slope*g^2
}


#Plot command 
plot(0, 0, xlim = xlims, ylim = ylims, 
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
     xaxs = "i", 
     yaxs = "i"
)

ticksy <- c(0,  riskreturn(g = 5.6), riskreturn(g = 12), 17, ylims[2])
ylabels <- c(NA, expression(paste(c[1])), expression(paste(bar(c) == c[2],phantom() == hat(y))), expression(paste(c[3])), NA)
ticksx <- c(0, 5.6, 12, xlims[2])
xlabels <- c(NA, expression(paste(Delta[i])), expression(paste(Delta[y])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)
mtext(expression(paste("Difference in income (good versus bad outcome), ", Delta, ", risk")), side = 1, line = 2.5, cex = axislabelsize)
text(xlims[1] - 1.5, ylims[2] - 0.55*(ylims[2] - ylims[1]), expression(paste("Expected income, ", hat(y))), xpd = TRUE, cex = axislabelsize, srt = 90) 


npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
lines(xx1, riskreturn(xx1), col = COLA[4], lwd = graphlinewidth, lty = 1)

#Add points a, b, c and c
#segments(5.6, 0, 5.6, riskreturn(g = 5.6), lty = 2, col = COLB[4], lwd = segmentlinewidth)
segments(0, riskreturn(g = 5.6), xlims[2], riskreturn(g = 5.6), lty = 1, col = COLB[4], lwd = graphlinewidth)
segments(0, riskreturn(g = 12) , xlims[2], riskreturn(g = 12) , lty = 1, col = COLB[4], lwd = graphlinewidth)
segments(0, 17, xlims[2], 17, lty = 1, col = COLB[4], lwd = graphlinewidth)

segments(5.6, 0, 5.6, riskreturn(g = 5.6), lty = 2, col = grays[20], lwd = segmentlinewidth)

segments(12, 0, 12, riskreturn(g = 12) , lty = 2, col = grays[20], lwd = segmentlinewidth)


#Label risk return schedule
text(14, 12.7, expression(paste(g(Delta))), cex = labelsize)


#Label value functions
text(2, 8.8, expression(u[1]), cex = labelsize)
text(2, 13.5, expression(u[2]), cex = labelsize)
text(2, 16.5, expression(u[3]), cex = labelsize)


points(5.6, riskreturn(g = 5.6), pch = 16, col = "black", cex = 1.5)
text(5.6 - 0.2, riskreturn(g = 5.6) + 0.5, expression(paste(a*minute)), cex = labelsize)


points(12, riskreturn(g = 12) , pch = 16, col = "black", cex = 1.5)
text(12, riskreturn(g = 12) + 0.6, expression(paste(i*minute)), cex = labelsize)


dev.off()

