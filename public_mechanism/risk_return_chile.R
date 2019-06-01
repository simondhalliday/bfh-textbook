#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
library(pBrackets)
pdf(file = "public_mechanism/risk_return_chile.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5
a <- c(2, 4, 6)


COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 11)
ylims <- c(0, 18)

riskreturn <- function(g, int1 = 14, int2 = 4, coeff = 1/3){
  int1 - (int2 - (coeff)*g)^2
}

uA <- function(omega, g, slope = 0.5){
  omega + omega*g  - slope*g^2
}

indiffA <- function(g, intercept = 3, slope = 0.125){
  intercept  + slope*g^2
}

insurance <- function(g, intercept = 10, slope = 0.36){
  intercept  + slope*g
}

#Plot command 
plot(0, 0, xlim = xlims, ylim = ylims, 
     type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     axes = FALSE,
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i"
)


#Customize ticks and labels for the plot
ticksy <- c(0,  1, 6, 11, indiffA(7, intercept = 6, slope = 0.125), ylims[2])
ylabels <- c(NA, expression(paste(y[1])), expression(paste(y[2])), expression(paste(y[3])), expression(paste(hat(y)^{b})), NA)
ticksx <- c(0, 7, xlims[2])
xlabels <- c(NA, expression(paste(Delta^b)),  NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = axislabelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = axislabelsize)
mtext(expression(paste("Degree of risk, ", Delta)), side=1, line = 2.5, cex = axislabelsize)
text(xlims[1] - 1, ylims[2] - 0.5*(ylims[2] - ylims[1]), expression(paste("Expected income, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 


npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
#lines(xx1, riskreturn(xx1, int1 = 14, int2 = 4, coeff = 1/3), col = COLA[4], lwd = graphlinewidth, lty = 1)
#lines(xx1, insurance(xx1), col = COL[3], lwd = graphlinewidth, lty = 1)
#lines(xx1, insurance(xx1, intercept = 7.4), col = COL[3], lwd = segmentlinewidth, lty = 2)
lines(xx1, indiffA(xx1, intercept = 1, slope = 0.15), col = COLB[4], lwd = graphlinewidth, lty = 1)
lines(xx1, indiffA(xx1, intercept = 6, slope = 0.125), col = COLB[4], lwd = segmentlinewidth, lty = 1)
lines(xx1, indiffA(xx1, intercept = 11, slope = 0.1), col = COLB[4], lwd = graphlinewidth, lty = 1)



#Add points a, b, c and c
# segments(5.6, 0, 5.6, riskreturn(g = 5.6), lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, riskreturn(g = 5.6), 5.6, riskreturn(g = 5.6), lty = 2, col = "gray", lwd = segmentlinewidth)
# points(5.6, riskreturn(g = 5.6), pch = 16, col = "black", cex = 1.5)
# text(5.6 + 0.25, riskreturn(g = 5.6) - 0.3, expression(n), cex = labelsize)


# segments(10.5, 0, 10.5, riskreturn(g = 10.5) , lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, riskreturn(g = 10.5) , 10.5, riskreturn(g = 10.5) , lty = 2, col = "gray", lwd = segmentlinewidth)
#points(10.5, riskreturn(g = 10.5) , pch = 16, col = "black", cex = 1.5)
#text(12 + 0.25, riskreturn(g = 12) - 0.3, expression(d), cex = labelsize)

# segments(0, insurance(g = 1.8), 10.5, insurance(g = 1.8), lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(1.8, 0, 1.8, insurance(g = 1.8), lty = 2, col = "gray", lwd = segmentlinewidth)
# points(1.8, insurance(g = 1.8) , pch = 16, col = "black", cex = 1.5)
# text(1.8, insurance(g = 1.8) + 0.6, expression(paste(c^2)), cex = labelsize)
# 

# Arrows(0, indiffA(0, intercept = 1, slope = 0.15), 
#          7 - 0.3, indiffA(7, intercept = 6, slope = 0.125) - 0.5, 
#          col = "black", lty = 1, arr.type = "triangle", lwd = segmentlinewidth)
# 
# Arrows(7, indiffA(7, intercept = 6, slope = 0.125), 
#        0 + 0.3, indiffA(0, intercept = 11, slope = 0.1) + 0.1, 
#        col = "black", lty = 1, arr.type = "triangle", lwd = segmentlinewidth)

segments(0, indiffA(7, intercept = 6, slope = 0.125), 
         7, indiffA(7, intercept = 6, slope = 0.125), 
         col = "gray", lty = 2, lwd = segmentlinewidth)

segments(7, 0, 
         7, indiffA(7, intercept = 6, slope = 0.125), 
         col = "gray", lty = 2, lwd = segmentlinewidth)

points(7, indiffA(7, intercept = 6, slope = 0.125), pch = 16, col = "black", cex = 1.5)
text(7 + 0.2, indiffA(7, intercept = 6, slope = 0.125)- 0.3, expression(paste(b)), cex = labelsize)

points(0, indiffA(0, intercept = 1, slope = 0.15), pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(0 + 0.2, indiffA(0, intercept = 1, slope = 0.15)- 0.3, expression(paste(a)), cex = labelsize)

points(0, indiffA(0, intercept = 11, slope = 0.1), pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(0 + 0.2, indiffA(0, intercept = 11, slope = 0.1)- 0.3, expression(paste(c)), cex = labelsize)



#text(10.5, riskreturn(g = 10.5) + 0.6, expression(paste(c^1)), cex = labelsize)
# brackets(x1 = 10.7, y1 = riskreturn(10.5)  - 0.1, 
#          x2 = 10.7, y2 = insurance(1.8),  
#          ticks = 0.5, curvature = 0.5, type = 1, h = 0.5,
#          col = "black", lwd = 1, lty = 1, xpd = TRUE)
# text(11.7, 12.1, expression(paste(p%.%i == phantom())), xpd = TRUE, cex = labelsize)
# text(13, 12.3, expression(paste("insurance")), xpd = TRUE, cex = labelsize)
# text(13, 11.7, expression(paste("premium")), xpd = TRUE, cex = labelsize)

# Arrows(2, indiffA(2, intercept = 6, slope = 0.125) + 0.3, 
#        2, indiffA(2, intercept = 11, slope = 0.1) - 0.7,
#        col = "black", lty = 1, lwd = 2, arr.type = "triangle",
#        arr.lwd = 0.5, code = 2)
# 
# text(3.2, 10, expression(paste("Increased income")), xpd = TRUE)
# text(3.2, 9.2, expression(paste("from land sale")), xpd = TRUE)
# 

# Arrows(10.4, 2, 2, 2, 
#        col = "black", lty = 1, lwd = 2, arr.type = "triangle", 
#        arr.lwd = 0.5, code = 2)
# text(3.75, 1.55, expression(paste("Decreased")), xpd = TRUE)
# text(3.75, 1, expression(paste("risk exposure")), xpd = TRUE)
# text(3.75, 0.5, expression(paste("due to insurance, i")), xpd = TRUE)

# 
# text(14, 16.2, expression(paste("insurance")), xpd = TRUE)
# text(14, 15.6, expression(paste("contract")), xpd = TRUE)


#Label risk return schedule
text(14.8, 13.8, expression(paste(g(Delta))), cex = labelsize, xpd = TRUE)


#Label value functions
text(10, 17, expression(u[1]), cex = labelsize)
text(9, 17, expression(u[2]), cex = labelsize)
text(7.4, 17, expression(u[3]), cex = labelsize)


dev.off()

