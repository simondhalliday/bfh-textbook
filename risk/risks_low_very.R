require(ggplot2)
require(shape)
#library(RColorBrewer)
#pdf(file = "risk/risks_low_very.pdf", width = 9, height = 7)

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
xlims <- c(0, 10)
ylims <- c(0, 20)

## u <- function(delta, y, b = 0.1, a = 1.5, c = 15){
##   y - b * delta^(c/y + a)
## }

u <- function(y, delta, a = 1.5, b = 0.1, c = 2){
  y^a - b * delta^c
}

indiff <- function(u, delta, a = 1.5, b = 0.1, c = 2){
  (u + b * delta^c )^(1/a)
}

slopeA <- function(g, slope1 = 0.25, slope2 = 0.08){
  slope1 + 2*slope2*g
}
 
tangentline <- function(g, intercept = 1, slope = 2){
  intercept + slope*g
}
 
indiffA2 <- function(g, intercept = 7.8, slope1 = 0.25, slope2 = 0.055){
  intercept  + slope1*g + slope2*g^2
}

slopeA2 <- function(g, slope1 = 0.25, slope2 = 0.055){
  slope1 + 2*slope2*g
}


slopeA3 <- function(g, slope1 = 0.25, slope2 = 0.01){
  slope1 + 2*slope2*g
}
tangentline2 <- function(g, intercept = 1, slope = 2){
  intercept + slope*g
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
ticksx <- c(0, 3, 9, xlims[2])
xlabels <- c(NA, expression(paste(Delta[Low])), expression(paste(Delta[High])), NA)
ticksy <- c(0, 3.7, 7.8, 14, ylims[2])
ylabels <- c(NA, expression(paste(c[1])), expression(paste(c[2])), expression(paste(c[3])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = axislabelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = axislabelsize)

#Axis labels and draw linear utility function
mtext(expression(paste("Difference in wealth (good versus bad outcome), ", Delta, ", risk")), side = 1, line = 2.5, cex = axislabelsize)
text(-1, 0.5*ylims[2], expression(paste("Expected wealth, ", omega)), xpd = TRUE, cex = axislabelsize, srt = 90) 


npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(ylims[1], ylims[2], length.out = npts)
#xx3 <- seq(7.5, 10.5, length.out = npts)
#xx4 <- seq(6.7594 - 1.75, 6.7594 + 1.75, length.out = npts)

#lines(xx1, riskreturn(xx1), col = COLA[4], lwd = graphlinewidth, lty = 1)
for (u in c(1,20,50) ){
 lines(xx1, indiff(u, xx1, c = 2.7), col = COLB[4], lwd = graphlinewidth, lty = 1)  
}

## contour(xx1, xx2,
##         outer(xx1,xx2, FUN = u),
##         drawlabels = FALSE,
##         col = COLB[3],
##         lwd = graphlinewidth,
##         levels = c(5,10,15),
##         xaxs="i",
##         yaxs="i",
##         add = TRUE)

lines(xx1, indiffA2(xx1, intercept = 7.8, slope2 = 0.055), col = COLB[4], lwd = graphlinewidth, lty = 1)
lines(xx1, indiffA2(xx1, intercept = 14, slope2 = 0.01), col = COLB[4], lwd = graphlinewidth, lty = 1)

#lines(xx1, indiffA(xx1, intercept = 4.4), col = COLB[3], lwd = graphlinewidth, lty = 1)
lines(xx2, tangentline(xx2, intercept = 2.9, slope = slopeA(g = 3)), col = "#666666", lwd = segmentlinewidth, lty = 2)
lines(xx4, tangentline(xx4, intercept = -0.05, slope = slopeA(g = 6.7594)), col = "#666666", lwd = segmentlinewidth, lty = 2)

#lines(xx3, tangentline(xx3, intercept = -2.9, slope = slopeA(g = 9)), col = "#666666", lwd = segmentlinewidth, lty = 2)
#lines(xx1, indiffA2(xx1, intercept = 7.1), col = COLB[5], lwd = graphlinewidth, lty = 1)
lines(xx2, tangentline2(xx2, intercept = 7.25, slope = slopeA2(g = 3)), col = "#666666", lwd = segmentlinewidth, lty = 2)
#lines(xx3, tangentline(xx3, intercept = 3.25, slope = slopeA2(g = 9)), col = "#666666", lwd = segmentlinewidth, lty = 2)

lines(xx2, tangentline2(xx2, intercept = 13.8, slope = slopeA3(g = 3)), col = "#666667", lwd = segmentlinewidth, lty = 2)
#lines(xx3, tangentline(xx3, intercept = 13.1, slope = slopeA3(g = 9)), col = "#666667", lwd = segmentlinewidth, lty = 2)

segments(3, 0, 3, ylims[2], lty = 2, col = "gray", lwd = segmentlinewidth)
segments(9, 0, 9, ylims[2], lty = 2, col = "gray", lwd = segmentlinewidth)

segments(0, indiffA2(3), xlims[2], indiffA2(3), lty = 2, col = "gray", lwd = segmentlinewidth)

#Add points a, b, c and c
# segments(5.6, 0, 5.6, riskreturn(g = 5.6), lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, riskreturn(g = 5.6), 5.6, riskreturn(g = 5.6), lty = 2, col = "gray", lwd = segmentlinewidth)
points(6.7594, indiffA(g = 6.7594), pch = 16, col = "black", cex = 1.5)
points(9, indiffA(g = 9), pch = 16, col = "black", cex = 1.5)
points(3, indiffA(g = 3), pch = 16, col = "black", cex = 1.5)
text(3 + 0.15, indiffA(g = 3) - 0.5, expression(e), cex = labelsize)
text(6.7594 + 0.15, indiffA(g = 6.7594) - 0.5, expression(g), cex = labelsize)
text(9 + 0.15, indiffA(g = 9) - 0.5, expression(h), cex = labelsize)



points(3, indiffA2(g = 3), pch = 16, col = "black", cex = 1.5)
text(3 + 0.15, indiffA2(g = 3) - 0.5, expression(f), cex = labelsize)

points(9, indiffA2(g = 9), pch = 16, col = "black", cex = 1.5)
text(9 + 0.15, indiffA2(g = 9) - 0.5, expression(i), cex = labelsize)


points(3, indiffA2(g = 3, intercept = 14, slope2 = 0.01), pch = 16, col = "black", cex = 1.5)
text(3 + 0.15, indiffA2(g = 3, intercept = 14, slope2 = 0.01) - 0.5, expression(k), cex = labelsize)

points(9, indiffA2(g = 9, intercept = 14, slope2 = 0.01), pch = 16, col = "black", cex = 1.5)
text(9 + 0.15, indiffA2(g = 9, intercept = 14, slope2 = 0.01) - 0.5, expression(j), cex = labelsize)


#text(5.6 + 0.25, riskreturn(g = 5.6) - 0.3, expression(A), cex = labelsize)


# segments(0, riskreturn(g = 7.5) , 7.5, riskreturn(g = 7.5) , lty = 2, col = "gray", lwd = segmentlinewidth)
# points(7.5, riskreturn(g = 7.5) , pch = 16, col = "black", cex = 1.5)
# text(7.5 + 0.25, riskreturn(g = 8) - 1, expression(B), cex = labelsize)

#Segment for next best wage offer
#segments(0, 5.75, xlims[2], 5.75, lty = 2, col = "gray", lwd = segmentlinewidth)

#Label risk return schedule
#text(14, riskreturn(g = 14) - 1, expression(paste(g(r) - bar(rho)*K) ), cex = labelsize)

#Label value functions
text(11.8, 17, expression(u[1]), cex = labelsize)
text(6, 6.8, expression("very risk"), cex = labelsize)
text(6, 6, expression("averse"), cex = labelsize)

text(10.4, 17, expression(u[2]), cex = labelsize)
text(6, 12.9, expression("risk"), cex = labelsize)
text(6, 12.1, expression("averse"), cex = labelsize)


text(9.4, 17.7, expression(u[3]), cex = labelsize)
text(6, 17.4, expression("almost risk"), cex = labelsize)
text(6, 16.6, expression("neutral"), cex = labelsize)


dev.off()

