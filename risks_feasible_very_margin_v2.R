require(ggplot2)
require(shape)
pdf(file = "capitalism/risks_feasible_very_margin_v2.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 2
labelsize <- 1.8
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5
a <- c(2, 4, 6)


COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)

par(mar =  c(5, 5.5, 1, 1))
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
ticksx <- c(0, 5.2, 12, xlims[2])
#xlabels <- c(NA, expression(paste(Delta[A])), expression(paste(Delta[B])), NA)
xlabels <- c(NA, expression(paste(Delta[a])), expression(paste( bar(Delta) )), NA)
#ticksy <- c(0, 4.4, 7.15, riskreturn(5.2), riskreturn(7.5), riskreturn(g = 12), ylims[2])
ticksy <- c(0, riskreturn(5.2), ylims[2])
ylabels <- c(NA, expression(paste(hat(y)[a])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#Axis labels and draw linear utility function
text(0.5*xlims[2], -2.1, expression(paste("Risk, ", Delta)), xpd = TRUE, cex = axislabelsize) 
text(-1.7, 0.5*ylims[2], expression(paste("Expected income, ", hat(y))), xpd = TRUE, cex = axislabelsize, srt = 90) 


npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
lines(xx1, riskreturn(xx1), col = COLA[4], lwd = graphlinewidth, lty = 1)
lines(xx1, indiffA(xx1, intercept = 1.5), col = COLB[3], lwd = graphlinewidth, lty = 1)
lines(xx1, indiffA(xx1, intercept = 4.4), col = COLB[3], lwd = graphlinewidth, lty = 1)
lines(xx1, indiffA(xx1, intercept = 9.5), col = COLB[3], lwd = graphlinewidth, lty = 1)
#lines(xx1, indiffA(xx1, intercept = 8), col = COLB[3], lwd = graphlinewidth, lty = 1)
#lines(xx1, indiffA2(xx1, intercept = 7.15), col = COLB[5], lwd = graphlinewidth, lty = 1)
#lines(xx1, indiffA2(xx1, intercept = 5.8), col = COLB[5], lwd = segmentlinewidth, lty = 2)

#lines(xx1, indiffA2(xx1, intercept = 10), col = COLB[5], lwd = graphlinewidth, lty = 1)

#Add points a, b, c and c
segments(5.2, 0, 5.2, riskreturn(g = 5.2), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, riskreturn(g = 5.2), 5.2, riskreturn(g = 5.2), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(5.2, riskreturn(g = 5.2), pch = 16, col = "black", cex = 1.5)
text(5.2 + 0.35, riskreturn(g = 5.2) - 0.05, expression(a), cex = labelsize)


# segments(7.5, 0, 7.5, riskreturn(g = 7.5) , lty = 2, col = grays[20], lwd = segmentlinewidth)
# segments(0, riskreturn(g = 7.5) , 7.5, riskreturn(g = 7.5) , lty = 2, col = grays[20], lwd = segmentlinewidth)
# points(7.5, riskreturn(g = 7.5) , pch = 16, col = "black", cex = 1.5)
# text(7.5 + 0.35, riskreturn(g = 7.5) - 0.3, expression(b), cex = labelsize)


segments(12, 0, 12, riskreturn(g = 12) , lty = 2, col = grays[20], lwd = segmentlinewidth)
#segments(0, riskreturn(g = 12) , 12, riskreturn(g = 12) , lty = 2, col = grays[20], lwd = segmentlinewidth)
text(12 + 0.3, riskreturn(g = 12) + 0.4, expression(m), cex = labelsize)

points(12, riskreturn(g = 12) , pch = 16, col = "black", cex = 1.5)





#Label risk return schedule
text(13.8, riskreturn(g = 14) - 1.2, expression(paste(hat(y)*(Delta))), cex = labelsize, xpd =  TRUE)

#Label value functions
#text(13.2, 17, expression(u[1]^B), cex = labelsize)
text(8.7, 17, expression(u[2]), cex = labelsize)
text(10, 17, expression(u[1]), cex = labelsize)
text(6.4, 17, expression(u[3]), cex = labelsize)
#text(8.2, 17, expression(u[2]^A), cex = labelsize)



dev.off()

