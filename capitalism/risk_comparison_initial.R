require(shape)
pdf(file = "capitalism/risk_comparison_initial.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
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
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")


par(mar =  c(5, 6, 0.5, 1))
xlims <- c(0, 15)
ylims <- c(0, 18)

riskreturn <- function(g, int1 = 14, int2 = 4, coeff = 1/3){
  int1 - (int2 - (coeff)*g)^2
}

uA <- function(omega, g, slope = 0.5){
  omega + omega*g  - slope*g^2
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
     xaxs = "i", 
     yaxs = "i"
)


#Customize ticks and labels for the plot
ticksx <- c(0, 6.3, xlims[2])
xlabels <- c(NA, expression(paste(Delta[a])), NA)
ticksy <- c(0, 3.65, 5.75, riskreturn(int1 = 12, 6.3), ylims[2])
ylabels <- c(NA, expression(paste(w[a])), expression(paste(w^c)),  expression(paste(hat(y)[a])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#Axis labels and draw linear utility function
mtext(expression(paste("Risk, ", Delta)), side = 1, line = 3, cex = axislabelsize)
text(-1.7, 0.5*ylims[2], expression(paste("Expected income or the wage, ", list(hat(y), w))), xpd = TRUE, cex = axislabelsize, srt = 90) 


npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Add points a, b, c and d
segments(6.3, 0, 6.3, riskreturn(g = 6.3, int1 = 12), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, riskreturn(g = 6.3, int1 = 12), 6.3, riskreturn(g = 6.3, int1 = 12), lty = 2, col = grays[20], lwd = segmentlinewidth)

#text(5.6 + 0.25, riskreturn(g = 5.6) - 0.3, expression(A), cex = labelsize)
# 


# segments(7.25, 0, 7.25, riskreturn(g = 7.25, int1 = 15) , lty = 2, col = grays[20], lwd = segmentlinewidth)
# segments(0, riskreturn(g = 7.25, int1 = 15) , 7.25, riskreturn(g = 7.25, int1 = 15) , lty = 2, col = grays[20], lwd = segmentlinewidth)
#text(7 + 0.25, riskreturn(g = 7.5, int1 = 15) - 1, expression(B), cex = labelsize)

#Segment for next best wage offer
# segments(0, 5.75, xlims[2], 5.75, lty = 2, col = "gray", lwd = segmentlinewidth)
# 
# 
# points(12, riskreturn(int1 = 15, g = 12), pch = 16, col = "black", cex = 1.5)
# 
# points(12, riskreturn(int1 = 5.7, g = 12), pch = 16, col = "black", cex = 1.5)

#Segment for next best wage offer
segments(0, 5.75, xlims[2], 5.75, lty = 1, col = CBCols[3], lwd = graphlinewidth)

text(12.75, 5, expression(paste("Wage from work, ", w^c)), cex = labelsize)

lines(xx1, riskreturn(xx1, int1 = 12), col = CBCols[1], lwd = graphlinewidth, lty = 1)
#lines(xx1, riskreturn(xx1, int1 = 15), col = CBCols[1], lwd = graphlinewidth, lty = 1)
#lines(xx1, indiffA(xx1, intercept = 4.4), col = COLB[3], lwd = graphlinewidth, lty = 1)
#lines(xx1, indiffA(xx1, intercept = 8), col = COLB[3], lwd = graphlinewidth, lty = 1)
lines(xx1, indiffA2(xx1, intercept = 3.65, slope2 = 0.08), col = CBCols[2], lwd = graphlinewidth, lty = 1)
#lines(xx1, indiffA2(xx1, intercept = 7.8, slope2 = 0.055), col = CBCols[2], lwd = graphlinewidth, lty = 1)
lines(xx1, indiffA2(xx1, intercept = 5.75, slope2 = 0.068), col = CBCols[2], lwd = graphlinewidth, lty = 1)

# points
# points(7.25, riskreturn(g = 7.25, int1 = 15) , pch = 16, col = "black", cex = 1.5)
points(6.3, riskreturn(g = 6.3, int1 = 12), pch = 16, col = "black", cex = 1.5)
points(0, 5.75, pch = 16, col = "black", cex = 1.5, xpd = TRUE)


#Label risk return schedule
#text(14, riskreturn(g = 14), expression(paste(g(Delta) - rho%.%K) ), cex = labelsize, xpd = TRUE)

text(14, riskreturn(int1 = 12, g = 14)-1.2, expression(paste(g(Delta) - bar(rho)%.%K) ), cex = labelsize, xpd = TRUE)

#Arrows(12.5, riskreturn(int1 = 15, g = 12) - 0.6, 12.5, riskreturn(int1 = 12, g = 12) + 0.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5, code = 3)
#text(11.1, 13.5, expression(paste(bar(rho)%.%K - rho%.%K)), cex = labelsize, xpd = TRUE)
# text(13.5, 9.7, expression(paste(m)), cex = labelsize)

#Label value functions
text(6.3, riskreturn(int1 = 12, 6.3) + 0.6, expression(a), cex = labelsize)
#text(7.25, riskreturn(int1 = 15, 7.25) + 0.8, expression(a^1), cex = labelsize)
text(12, 17, expression(u[1]), cex = labelsize)
text(10.5, 17, expression(u[2]), cex = labelsize)

text(0.3, 5.75 + 0.5, expression(e), cex = labelsize)


dev.off()

