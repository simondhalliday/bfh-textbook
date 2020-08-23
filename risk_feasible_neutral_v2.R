require(ggplot2)
require(shape)
pdf(file = "capitalism/risk_feasible_neutral_v2.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 2
labelsize <- 1.8
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5


COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)

par(mar =  c(4, 4, 1, 1))
xlims <- c(0, 15)
ylims <- c(0, 18)

riskreturn <- function(g, int1 = 14.07, int2 = 4, coeff = 1/3){
  int1 - (int2 - (coeff)*g)^2
}

uA <- function(y, g, slope = 0.5){
  y + y*g  - slope*g^2
}

indiffA <- function(g, intercept = 3, slope1 = 0.25, slope2 = 0.12){
  intercept  + slope1*g + slope2*g^2
}

indiffA2 <- function(g, intercept = 3, slope1 = 0.25, slope2 = 0.05){
  intercept  + slope1*g + slope2*g^2
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
# ticksy <- seq(ylims[1], ylims[2], 3)
# ylabels <- seq(ylims[1], ylims[2], 3)
# ticksx <- seq(xlims[1], xlims[2], 5)
# xlabels <- seq(xlims[1], xlims[2], 5)
ticksx <- c(0, 12, xlims[2])
#xlabels <- c(NA, expression(paste(Delta[A])), expression(paste(Delta[B])), NA)
xlabels <- c(NA, expression(paste(Delta[m]  ==  bar(Delta)  )), NA)
#ticksy <- c(0, 4.4, 7.15, riskreturn(5.2), riskreturn(7.5), riskreturn(g = 12), ylims[2])
ticksy <- c(0, avgwealth(x = 8.4), ylims[2])
ylabels <- c(NA, expression(paste(hat(y)[m])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#Axis labels and draw linear utility function
text(0.5*xlims[2], -1.5, expression(paste("Risk, ", Delta)), xpd = TRUE, cex = axislabelsize) 
text(-1.1, 0.5*ylims[2], expression(paste("Expected income", hat(y))), xpd = TRUE, cex = axislabelsize, srt = 90) 


npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
lines(xx1, riskreturn(xx1), col = COLA[4], lwd = graphlinewidth, lty = 1)

segments(0, avgwealth(x = 8.4), xlims[2], avgwealth(x = 8.4), lty = 1, col = COLB[4], lwd = graphlinewidth)
segments(0, avgwealth(x = 8.4) - 2, xlims[2], avgwealth(x = 8.4) - 2, lty = 1, col = COLB[4], lwd = graphlinewidth)
segments(0, avgwealth(x = 8.4) + 2, xlims[2], avgwealth(x = 8.4) + 2, lty = 1, col = COLB[4], lwd = graphlinewidth)

#add point m
text(12, avgwealth(x = 8.4) + 1, expression(paste(m)), cex = labelsize)
segments(12, 0, 12, avgwealth(x = 8.4), lty = 2, col = "gray", lwd = segmentlinewidth)
points(12, avgwealth(x = 8.4), pch = 16, col = "black", cex = 1.5)
# 

#label the three indifference curves
text(14, avgwealth(x = 8.4)  + 2.5, expression(paste(u[3])), xpd = TRUE, cex = labelsize)
text(14, avgwealth(x = 8.4) + 0.4, expression(paste(u[2])),  xpd = TRUE, cex = labelsize)
text(14,  avgwealth(x = 8.4) - 1.6 , expression(paste(u[1])),  xpd = TRUE, cex = labelsize)


text(avgwealth(x = 8.4)-8.5, avgwealth(x = 8.4) - 6, expression(paste(hat(y)(Delta))), xpd = TRUE, cex = labelsize)


dev.off()

