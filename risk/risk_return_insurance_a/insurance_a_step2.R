#Graph Designer: Simon Halliday, Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
library(pBrackets)
pdf(file = "risk/risk_return_insurance_a/insurance_a_step2.pdf", width = 9, height = 7)

# Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#ff7f00", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)

a <- c(2, 4, 6)
par(mar =  c(5, 5, 1, 1))
xlims <- c(0, 15)
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
     xaxs = "i", 
     yaxs = "i"
)


#Customize ticks and labels for the plot
# ticksy <- c(0,  insurance(g = 1.6, intercept = 7.4), riskreturn(g = 5.6) ,ylims[2])
# ylabels <- c(NA, expression(paste(hat(y)[a[2]])), expression(paste(hat(y)[a[1]])) , NA)
ticksy <- c(0,  NA, riskreturn(g = 5.6) ,ylims[2])
ylabels <- c(NA, NA, expression(paste(hat(y)[a[1]])) , NA)
# ticksx <- c(0, 1.6, 5.6, xlims[2])
# xlabels <- c(NA, expression(paste(Delta[a[2]])), expression(paste(Delta[a[1]])), NA)
ticksx <- c(0, NA, 5.6, xlims[2])
xlabels <- c(NA, NA, expression(paste(Delta[a[1]])), NA)


axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)
#mtext(expression(paste("Degree of risk, ", Delta)), side=1, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], - 2, expression(paste("Degree of risk, ", Delta)), xpd = TRUE, cex = axislabelsize) 
text(xlims[1] - 1.5, ylims[2] - 0.5*(ylims[2] - ylims[1]), expression(paste("Expected income, ", hat(y))), xpd = TRUE, cex = axislabelsize, srt = 90) 


npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(xlims[1], 5.6, length.out = npts)
xx3 <- seq(5.6, xlims[2], length.out = npts)

# gray segments
segments(5.6, 0, 5.6, riskreturn(g = 5.6), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, riskreturn(g = 5.6), 5.6, riskreturn(g = 5.6), lty = 2, col = grays[20], lwd = segmentlinewidth)
# segments(0, insurance(g = 1.6, intercept = 7.4), 5.6, insurance(g = 1.6, intercept = 7.4), lty = 2, col = grays[20], lwd = segmentlinewidth)
# segments(1.6, 0, 1.6, insurance(g = 1.6, intercept = 7.4), lty = 2, col = grays[20], lwd = segmentlinewidth)


lines(xx1, riskreturn(xx1, int1 = 14, int2 = 3.99, coeff = 1/3), col = COLA[4], lwd = graphlinewidth, lty = 1)
#lines(xx1, insurance(xx1), col = COL[3], lwd = graphlinewidth, lty = 1)
lines(xx2, insurance(xx2, intercept = 7.4), col = COL[3], lwd = graphlinewidth, lty = 1)
lines(xx3, insurance(xx3, intercept = 7.4), col = COL[3], lwd = segmentlinewidth, lty = 2)
lines(xx1, indiffA(xx1, intercept = 5.6), col = COLB[4], lwd = graphlinewidth, lty = 1)
#lines(xx1, indiffA(xx1, intercept = 7.7, slope = 0.115), col = COLB[4], lwd = segmentlinewidth, lty = 1)
#lines(xx1, indiffA(xx1, intercept = 10.35, slope = 0.1), col = COLB[4], lwd = graphlinewidth, lty = 1)



#Add points a, b, c and c

points(5.6, riskreturn(g = 5.6), pch = 16, col = "black", cex = 1.5)
text(5.6 - 0.2, riskreturn(g = 5.6) + 0.5, expression(paste(a[1])), cex = labelsize)


# segments(10.5, 0, 10.5, riskreturn(g = 10.5) , lty = 2, col = grays[20], lwd = segmentlinewidth)
# segments(0, riskreturn(g = 10.5) , 10.5, riskreturn(g = 10.5) , lty = 2, col = grays[20], lwd = segmentlinewidth)
#points(10.5, riskreturn(g = 10.5) , pch = 16, col = "black", cex = 1.5)
#text(12 + 0.25, riskreturn(g = 12) - 0.3, expression(d), cex = labelsize)


# points(1.6, insurance(g = 1.6, intercept = 7.4) , pch = 16, col = "black", cex = 1.5)
# text(1.6, insurance(g = 1.6, intercept = 7.4) + 0.6, expression(paste(a[2])), cex = labelsize)



# points(10.5, riskreturn(g = 10.5) , pch = 16, col = "black", cex = 1.5)
# text(10.5, riskreturn(g = 10.5) + 0.6, expression(paste(b^1)), cex = labelsize)

# brackets(x1 = 5.8, y1 = riskreturn(5.6)  - 0.1, 
#          x2 = 5.8, y2 = insurance(1.6, intercept = 7.4),  
#          ticks = 0.5, curvature = 0.5, type = 1, h = 0.5,
#          col = "black", lwd = 1.5, lty = 1, xpd = TRUE)
# text(7.2, 8.7, expression(paste(p[s]%.%s==phantom())), xpd = TRUE, cex = labelsize)
# text(8.9, 9.1, expression(paste("Insurance")), xpd = TRUE, cex = labelsize)
# text(8.9, 8.3, expression(paste("premium")), xpd = TRUE, cex = labelsize)



# Arrows(5.8, 3, 10.2, 3, 
#        col = "black", lty = 2, lwd = 2, arr.type = "triangle", 
#        arr.lwd = 0.5, code = 2)
# text(8.05, 4, expression(paste("Increased risk-taking")), xpd = TRUE)
# text(8.05, 3.5, expression(paste("due to insurance")), xpd = TRUE)
# 

# Arrows(5.4, 0.45, 2, 0.45, 
#        col = "black", lty = 1, lwd = 2, arr.type = "triangle", 
#        arr.lwd = 0.5, code = 2)
# text(3.6, 3.3, expression(paste("Decreased")), cex = labelsize, xpd = TRUE)
# text(3.6, 2.5, expression(paste("risk exposure")), cex = labelsize, xpd = TRUE)
# text(3.6, 1.8, expression(paste("due to insurance")), cex = labelsize, xpd = TRUE)
# text(3.6, 1, expression(paste(s == Delta[a[1]] - Delta[a[2]])), cex = labelsize, xpd = TRUE)
# 
# 
text(13.5, 11.4, expression(paste("Insurance")), cex = labelsize, xpd = TRUE)
text(13.5, 10.7, expression(paste("contract")), cex = labelsize, xpd = TRUE)
text(13.5, 9.95, expression(paste("slope", phantom()==p[s])), cex = labelsize, xpd = TRUE)


# text(3, 17.5, expression(paste("Insurance reduces")), cex = labelsize, xpd = TRUE)
# text(3, 16.8, expression(paste("risk exposure")), cex = labelsize, xpd = TRUE)
# text(3, 16.1, expression(paste("and increases,")), cex = labelsize, xpd = TRUE)
# text(3, 15.4, expression(paste("but does not ")), cex = labelsize, xpd = TRUE)
# text(3, 14.7, expression(paste("maximize, utility ")), cex = labelsize, xpd = TRUE)

#Label risk return schedule
text(14.6, 14, expression(paste(hat(y)(Delta))), cex = labelsize, xpd = TRUE)

#Label value functions
text(10, 17, expression(u[1]), cex = labelsize)
#text(8.58, 17, expression(u[2]), cex = labelsize)


dev.off()

