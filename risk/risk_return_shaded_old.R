require(ggplot2)
require(shape)
pdf(file = "risk/risk_return_shaded.pdf", width = 9, height = 7)

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
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)

par(mar =  c(5, 5, 0.5, 0.5))
xlims <- c(0, 15)
ylims <- c(0, 18)

riskreturn <- function(g, int1 = 14, int2 = 4, coeff = 1/3){
  int1 - (int2 - (coeff)*g)^2
}

indiffA <- function(g, intercept = 5.6, slope = 0.125){
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


ticksy <- c(0, riskreturn(g = 3.227315) , riskreturn(g = 5.6) , riskreturn(g = 12) , ylims[2])
ylabels <- c(NA, expression(paste(hat(y)[d])), expression(paste(hat(y)[a])),  expression(paste(hat(y)[m])), NA)
ticksx <- c(0, 3.227315, 5.6, 12, xlims[2])
xlabels <- c(NA, expression(Delta[d]), expression(Delta[a]), expression(Delta[m]), NA)

#mtext(expression(paste("Difference in income (good versus bad outcome), ", Delta, ", risk")), side = 1, line = 2.5, cex = axislabelsize)
text(xlims[2] - 0.5*(xlims[2] - xlims[1]), ylims[1] - 1.7, expression(paste("Difference in income (good versus bad outcome), ", Delta, ", risk")), xpd = TRUE, cex = axislabelsize) 
text(xlims[1] - 1.5, ylims[2] - 0.5*(ylims[2] - ylims[1]), expression(paste("Expected income, ", hat(y))), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Draw the polygon for shading the utility areas
xpoly1 <- seq(from = xlims[1], to = xlims[2], length.out = 500)
ypoly1 <- riskreturn(xpoly1)
polygon(x = c(xlims[2], xlims[2], rev(xpoly1), 0),
        y = c(ylims[2], 0, rev(ypoly1), ylims[2]),
        col=COLB[1], density=NULL, border = NA)

xpoly2 <- seq(from = 3*(4 - sqrt(14)), to = xlims[2], length.out = 500)
ypoly2 <- riskreturn(xpoly2)
polygon(x = c(xpoly2[1], xlims[2], rev(xpoly2)),
        y = c(ylims[1], ylims[1], rev(ypoly2)),
        col=COLA[1], density=NULL, border = NA)


text(10, 10, expression(paste("Risk-return")), cex = labelsize)
text(10, 9.1, expression(paste("schedule, ", omega = hat(y)(Delta) )), cex = labelsize)

Arrows(10, 10.5, 10, 12.8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(9, 3, expression(paste("Feasible combinations ")), cex = labelsize)
text(9, 2, expression(paste("of risk and expected income")), cex = labelsize)

text(4, 17, expression(paste("Infeasible combinations ")), cex = labelsize)
text(4, 16, expression(paste("of risk and expected income")), cex = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
lines(xx1, riskreturn(xx1), col = COLA[4], lwd = graphlinewidth, lty = 1)

segments(12, 0, 12, riskreturn(g = 12), lty = 2, lwd = segmentlinewidth, col = grays[20])
#segments(0, riskreturn(g = 12), 12, riskreturn(g = 12), lty = 2, lwd = segmentlinewidth, col = grays[20])


segments(10, riskreturn(g = 12), 14, riskreturn(g = 12), lty = 2, lwd = graphlinewidth, col = grays[22])
points(12, riskreturn(g = 12), pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(12, riskreturn(g = 12) + 0.5, expression(m), cex = labelsize)

#Add points
points(6.5, 10.6, pch = 16, col = "black", cex = 1.5)
points(4.5, 7.7, pch = 16, col = "black", cex = 1.5)
points(6.5, 7.5, pch = 16, col = "black", cex = 1.5)

#point labels
text(6.8, 10, expression(paste("g''")), cex = labelsize)
text(4.8, 7, expression(paste("g'")), cex = labelsize)
text(6.8, 7, expression(paste(g)), cex = labelsize)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)


#Add points a, b, c and c
segments(5.6, 0, 5.6, riskreturn(g = 5.6), lty = 2, lwd = segmentlinewidth, col = grays[22])
segments(0, riskreturn(5.6), 5.6, riskreturn(g = 5.6), lty = 2, lwd = segmentlinewidth, col = grays[22])
points(5.6, riskreturn(g = 5.6), pch = 16, col = "black", cex = 1.5)
text(5.6 + 0.25, riskreturn(g = 5.6) - 0.3, expression(a), cex = labelsize)


#riskreturn(g = 5.6) - 4 = 5.448889
#12 - 3*(14 - 5.448889)^(1/2) = 3.227315
segments(3.227315, 0, 3.227315, riskreturn(g = 3.227315), lty = 2, lwd = segmentlinewidth, col = grays[22])
segments(0, riskreturn(3.227315), 5.6, riskreturn(g = 3.227315), lty = 2, lwd = segmentlinewidth, col = grays[22])

points(3.227315, 5.448889 , pch = 16, col = "black", cex = 1.5)
text(3.227315 + 0.25, 5.448889 - 0.5, expression(d), cex = labelsize)
points(5.6, riskreturn(g = 5.6) - 4 , pch = 16, col = "black", cex = 1.5)
text(5.6  + 0.25, riskreturn(g = 5.6) - 4 - 0.5, expression(e), cex = labelsize)


dev.off()

