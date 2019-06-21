require(ggplot2)
require(shape)
pdf(file = "risk/discount_factor.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2
a <- c(2, 4, 6)


COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 20)
ylims <- c(0, 1)

hyperbolic <- function(t, alpha = 0.25){
1 / (1 + alpha * t)  
}


exponential <- function(t, delta = 0.85){
  delta^{t - 1}
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
     xaxs="i", 
     yaxs="i"
)


#Customize ticks and labels for the plot
ticksy <- seq(ylims[1], ylims[2], 0.05)
ylabels <- seq(ylims[1], ylims[2], 0.05)
ticksx <- seq(xlims[1], xlims[2], 1)
xlabels <- seq(xlims[1], xlims[2], 1)
# ticksy <- c(0,  2, 5.6, 9, riskreturn(g = 5.6), riskreturn(g = 12) ,ylims[2])
# ylabels <- c(NA, expression(paste(c[1])), expression(paste(c[2])), expression(paste(c[3])), expression(paste(omega[i])), expression(paste(bar(omega) )), NA)
# ticksx <- c(0, 5.6, 12, xlims[2])
# xlabels <- c(NA, expression(paste(Delta[i])), expression(paste(Delta[bar(omega) ])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

mtext(expression(paste("Time, t")), side = 1, line = 2.5, cex = axislabelsize)
text(xlims[1] - 1.5, ylims[2] - 0.5*(ylims[2] - ylims[1]), expression(paste("Annual discount factor")), xpd = TRUE, cex = axislabelsize, srt = 90) 

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
lines(xx1, hyperbolic(xx1), col = COLA[4], lwd = graphlinewidth, lty = 1)
lines(xx1, exponential(xx1), col = COLB[4], lwd = graphlinewidth, lty = 1)
#lines(xx1, indiffA(xx1, intercept = 5.6), col = COLB[4], lwd = graphlinewidth, lty = 1)
#lines(xx1, indiffA(xx1, intercept = 9), col = COLB[4], lwd = graphlinewidth, lty = 1)
# 
# 
# #Add points a, b, c and c
# segments(5.6, 0, 5.6, riskreturn(g = 5.6), lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, riskreturn(g = 5.6), 5.6, riskreturn(g = 5.6), lty = 2, col = "gray", lwd = segmentlinewidth)
# points(5.6, riskreturn(g = 5.6), pch = 16, col = "black", cex = 1.5)
# text(5.6 + 0.25, riskreturn(g = 5.6) - 0.3, expression(i), cex = labelsize)
# 
# 
# segments(12, 0, 12, riskreturn(g = 12) , lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, riskreturn(g = 12) , 12, riskreturn(g = 12) , lty = 2, col = "gray", lwd = segmentlinewidth)
# points(12, riskreturn(g = 12) , pch = 16, col = "black", cex = 1.5)
# text(12 + 0.25, riskreturn(g = 12) - 0.3, expression(d), cex = labelsize)
# 
# 
# points(1.8, riskreturn(g = 1.8) , pch = 16, col = "black", cex = 1.5)
# text(1.8 + 0.25, riskreturn(g = 1.8) - 0.3, expression(a), cex = labelsize)
# 
# 
# points(9.5, riskreturn(g = 9.5) , pch = 16, col = "black", cex = 1.5)
# text(9.5 + 0.25, riskreturn(g = 9.5) - 0.3, expression(b), cex = labelsize)
# 
# 
# #Label risk return schedule
# text(14, 12.7, expression(paste(omega == g(Delta))), cex = labelsize)
# 
# #Label value functions
# text(10.6, 17, expression(u[1]), cex = labelsize)
# text(9.1, 17, expression(u[2]), cex = labelsize)
# text(7.5, 17, expression(u[3]), cex = labelsize)
# 

dev.off()

