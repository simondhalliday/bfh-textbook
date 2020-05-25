require(shape)
pdf(file = "capitalism/risk_contrast_monitoring.pdf", width = 9, height = 7)

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
grays <- gray.colors(25, start = 1, end = 0)

par(mar =  c(4, 5, 4, 2))
xlims <- c(0, 15)
ylims <- c(0, 18)

riskreturn <- function(g, int1 = 14, int2 = 4, coeff = 1/3){
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
xlabels <- c(NA, expression(paste( bar(Delta) )), NA)
ticksy <- c(0, 11, riskreturn(int1 = 15, g = 12), ylims[2])
ylabels <- c(NA, expression(paste(w,"*")), expression(paste(y(bar(Delta)))), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#Axis labels and draw linear utility function
mtext(expression(paste("Risk, ", Delta)), side = 1, line = 3, cex = axislabelsize)
text(-1.7, 0.5*ylims[2], expression(paste("Expected income or the wage, ", list(y, w))), xpd = TRUE, cex = axislabelsize, srt = 90) 


npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
#lines(xx1, riskreturn(xx1, int1 = 5.7), col = COLA[4], lwd = graphlinewidth, lty = 1)
lines(xx1, riskreturn(xx1, int1 = 11), col = COLA[4], lwd = graphlinewidth, lty = 1)
lines(xx1, riskreturn(xx1, int1 = 15), col = COLA[4], lwd = graphlinewidth, lty = 1)
# lines(xx1, indiffA(xx1, intercept = 4.4), col = COLB[3], lwd = graphlinewidth, lty = 1)
# lines(xx1, indiffA(xx1, intercept = 8), col = COLB[3], lwd = graphlinewidth, lty = 1)
# lines(xx1, indiffA2(xx1, intercept = 7.1), col = COLB[5], lwd = graphlinewidth, lty = 1)
# lines(xx1, indiffA2(xx1, intercept = 10), col = COLB[5], lwd = graphlinewidth, lty = 1)

#Add points a, b, c and d
segments(12, 0, 12, riskreturn(int1 = 11, g = 12), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(10, riskreturn(int1 = 15, g = 12), 14, riskreturn(int1 = 15, g = 12), lty = 2, col = grays[20], lwd = segmentlinewidth)
#text(5.6 + 0.25, riskreturn(g = 5.6) - 0.3, expression(A), cex = labelsize)
# 


# segments(7.5, 0, 7.5, riskreturn(g = 7.5) , lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, riskreturn(g = 7.5) , 7.5, riskreturn(g = 7.5) , lty = 2, col = "gray", lwd = segmentlinewidth)
# points(7.5, riskreturn(g = 7.5) , pch = 16, col = "black", cex = 1.5)
# text(7.5 + 0.25, riskreturn(g = 8) - 1, expression(B), cex = labelsize)

#Segment for next best wage offer
segments(0, 11, xlims[2], 11, lty = 2, col = grays[20], lwd = segmentlinewidth)


points(12, riskreturn(int1 = 15, g = 12), pch = 16, col = "black", cex = 1.5)

points(12, riskreturn(int1 = 11, g = 12), pch = 16, col = "black", cex = 1.5)




#Label risk return schedule
text(10, riskreturn(g = 14), expression(paste(g(Delta) - rho%.%K) ), cex = labelsize)

text(9.7, riskreturn(int1 = 11, g = 14)-2, expression(paste(w,"*",phantom()==g(Delta)-rho%.%K-m) ), cex = labelsize)

Arrows(12, riskreturn(int1 = 15, g = 12) - 0.75, 12, riskreturn(int1 = 11, g = 12) + 0.75, col = "black", lty = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5, code = 3)
text(14, 13, expression(paste("Monitoring costs")), cex = labelsize, xpd = TRUE)
text(14, 12, expression(paste((m))), cex = labelsize, xpd = TRUE)

#Label value functions
# text(12.2, 17, expression(v[1]^B), cex = labelsize)
# text(10.1, 17, expression(v[2]^B), cex = labelsize)
# text(8.8, 17, expression(v[1]^A), cex = labelsize)
# text(7.2, 17, expression(v[2]^A), cex = labelsize)


dev.off()

