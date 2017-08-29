#Graph Designer: Simon Halliday & Madeleine Wettach '20
#Authors: Bowles, Foley and Halliday
#Title: Microeconomics: Competition, Conflict and Coordination

require(shape)
library(pBrackets)
pdf(file = "risk/step_by_step_graphs_4/risk_return_investment_step2.pdf", width = 9, height = 7)

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

par(mar =  c(5, 5.5, 5, 2))
xlims <- c(0, 15)
ylims <- c(0, 20)

riskreturn <- function(g, int1 = 18, int2 = 4, coeff = 0.4){
  int1 - (int2 - (coeff)*g)^2
}

uA <- function(omega, g, slope = 0.5){
  omega + omega*g  - slope*g^2
}

indiffA <- function(g, intercept = 2, slope = 0.2){
  intercept  + slope*g^2
}

insurance <- function(g, intercept = 10, slope = 0.36){
  intercept  + slope*g
}

#Find the optima of the risk return schedule for plotting
retopt <- optimize(riskreturn, interval = xlims, maximum = TRUE)
retmax <- retopt$maximum[1]

#Find the intersection of the riskreturn schedule and indiffA
rt <- uniroot(function(g)  riskreturn(g) - indiffA(g), c(0.01, 15), tol = 1e-8) 
int1 <- rt$root

# rt2 <- uniroot(function(g)  riskreturn(g) - indiffA(g, intercept = 10.35, slope = 0.12), c(1, 10), tol = 1e-8) 
# tang <- rt2$root
# tang  

#Plot command 
plot(0, 0, xlim = xlims, ylim = ylims, 
     type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     axes = FALSE,
     cex.lab = labelsize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i"
)


#Customize ticks and labels for the plot
ticksy <- c(0,  2, 10.35, riskreturn(g = 5.2), riskreturn(g = retmax), ylims[2])
ylabels <- c(NA, expression(paste(c[1] == R[1])), expression(paste(c[2] == R[2])), expression(paste(R[a])), expression(paste(c[3] == R[m])), NA)
ticksx <- c(0, 5.2, retmax, xlims[2])
xlabels <- c(NA, expression(paste(Delta[a])), expression(paste(Delta[m])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)
mtext(expression(paste("Degree of risk, ", Delta)), side=1, line = 2.5, cex = axislabelsize)
text(xlims[1] - 1.8, ylims[2] - 0.5*(ylims[2] - ylims[1]), expression(paste("Expected wealth, ", omega)), xpd = TRUE, cex = axislabelsize, srt = 90) 

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)


#Notice, I use the earlier calculated intercept here for the polygon
xx3 <- seq(xlims[1], int1, length.out = npts)
polygon(x = c(xx3, rev(xx3)), 
        y = c(riskreturn(xx3), indiffA(rev(xx3))), 
        col = COLA[1], density=NULL, border = NA)

lines(xx1, riskreturn(xx1), col = COLA[4], lwd = graphlinewidth, lty = 1)
###cut
lines(xx1, indiffA(xx1), col = COLB[4], lwd = graphlinewidth, lty = 1)
#lines(xx1, indiffA(xx1, intercept = 10.35, slope = 0.15), col = COLB[4], lwd = graphlinewidth, lty = 1)

####cut
#Add points a, b, c and c
#segments(5.2, 0, 5.2, riskreturn(g = 5.2), lty = 2, col = "gray", lwd = segmentlinewidth)
#segments(0, riskreturn(g = 5.2), 5.2, riskreturn(g = 5.2), lty = 2, col = "gray", lwd = segmentlinewidth)
#points(5.2, riskreturn(g = 5.2), pch = 16, col = "black", cex = 1.5)
#text(5.2 + 0.25, riskreturn(g = 5.2) - 0.3, expression(a), cex = labelsize)


#####Risk neutral segment line
#segments(0, riskreturn(retmax), xlims[2], riskreturn(g = retmax) , lty = 2, col = COLB[4], lwd = graphlinewidth)

#segments(retmax, 0, retmax, riskreturn(g = retmax) , lty = 2, col = "gray", lwd = segmentlinewidth)
#points(retmax, riskreturn(g = retmax), pch = 16, col = "black", cex = 1.5)
#text(retmax + 0.25, riskreturn(g = retmax) - 0.5, expression(m), cex = labelsize)
#####cut

# segments(0, insurance(g = 1.8), 10.5, insurance(g = 1.8), lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(1.8, 0, 1.8, insurance(g = 1.8), lty = 2, col = "gray", lwd = segmentlinewidth)
# points(1.8, insurance(g = 1.8) , pch = 16, col = "black", cex = 1.5)
# text(1.8, insurance(g = 1.8) + 0.6, expression(paste(b^2)), cex = labelsize)
# 
# 
# 
# points(10.5, riskreturn(g = 10.5) , pch = 16, col = "black", cex = 1.5)
# text(10.5, riskreturn(g = 10.5) + 0.6, expression(paste(b^1)), cex = labelsize)
# 
# brackets(x1 = 10.7, y1 = riskreturn(10.5)  - 0.1, 
#          x2 = 10.7, y2 = insurance(1.8),  
#          ticks = 0.5, curvature = 0.5, type = 1, h = 0.5,
#          col = "black", lwd = 1, lty = 1, xpd = TRUE)
# text(11.4, 12.1, expression(paste(p)), xpd = TRUE, cex = labelsize)
# 
# Arrows(5.8, 3, 10.2, 3, 
#        col = "black", lty = 2, lwd = 2, arr.type = "triangle", 
#        arr.lwd = 0.5, code = 2)
# text(8.05, 4, expression(paste("Increased risk-taking")), xpd = TRUE)
# text(8.05, 3.5, expression(paste("due to insurance")), xpd = TRUE)
# 
# 
# Arrows(10.4, 2, 2, 2, 
#        col = "black", lty = 1, lwd = 2, arr.type = "triangle", 
#        arr.lwd = 0.5, code = 2)
# text(3.75, 1.55, expression(paste("Decreased net")), xpd = TRUE)
# text(3.75, 1, expression(paste("risk exposure")), xpd = TRUE)
# text(3.75, 0.5, expression(paste("due to insurance")), xpd = TRUE)
# 
# 
#text(14, 19.6, expression(paste("Risk-neutral")), xpd = TRUE, cex = labelsize)
#text(14, 18.7, expression(paste("indifference curve")), xpd = TRUE, cex = labelsize)
# 
# 
# #Label risk return schedule
text(14, 14.2, expression(paste(R(Delta))), cex = labelsize)
text(14, 13.4, expression(paste("Risk-return")), cex = labelsize)
text(14, 12.6, expression(paste("schedule")), cex = labelsize)
#

####cut
# #Label value functions
text(9.8, 19.5, expression(u[1]), cex = labelsize)
#text(7.4, 19.5, expression(u[2]), cex = labelsize)
#text(14, 17.5, expression(u[3]), cex = labelsize)

####cut
#text(6, 22.7, expression(paste("Risk-averse")), xpd = TRUE, cex = labelsize) 
#text(6, 22, expression(paste("investor's")), xpd = TRUE, cex = labelsize) 
#text(6, 21.2, expression(paste("highest feasible")), xpd = TRUE, cex = labelsize) 
#text(6, 20.4, expression(paste("indifference curve")), xpd = TRUE, cex = labelsize) 

text(10, 22.7, expression(paste("Risk-averse")), xpd = TRUE, cex = labelsize) 
text(10, 22, expression(paste("investor's")), xpd = TRUE, cex = labelsize) 
text(10, 21.2, expression(paste("fallback")), xpd = TRUE, cex = labelsize) 
text(10, 20.4, expression(paste("indifference curve")), xpd = TRUE, cex = labelsize) 




dev.off()

