#Graph Designer: Simon Halliday, Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
library(pBrackets)
pdf(file = "risk/risk_return_investment.pdf", width = 9, height = 7)

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
grays <- gray.colors(25, start = 1, end = 0)

a <- c(2, 4, 6)
par(mar =  c(5, 7, 4.5, 1))
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
     xaxs = "i", 
     yaxs = "i"
)


#Customize ticks and labels for the plot
ticksy <- c(0,  2, 10.35, riskreturn(g = 5.2), riskreturn(g = retmax), ylims[2])
ylabels <- c(NA, expression(paste(c[1] == hat(y)[1])), expression(paste(c[2] == hat(y)[2])), expression(paste(hat(y)[a])), expression(paste(c[3] == hat(y)[m])), NA)
ticksx <- c(0, 5.2, retmax, xlims[2])
xlabels <- c(NA, expression(paste(Delta[a])), expression(paste(Delta[m])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)
mtext(expression(paste("Degree of risk, ", Delta)), side=1, line = 2.5, cex = axislabelsize)
text(xlims[1] - 2.4, ylims[2] - 0.5*(ylims[2] - ylims[1]), expression(paste("Expected income, ", hat(y))), xpd = TRUE, cex = axislabelsize, srt = 90) 

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Notice, I use the earlier calculated intercept here for the polygon
xx3 <- seq(xlims[1], int1, length.out = npts)

segments(5.2, 0, 5.2, riskreturn(g = 5.2), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, riskreturn(g = 5.2), 5.2, riskreturn(g = 5.2), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(retmax, 0, retmax, riskreturn(g = retmax) , lty = 2, col = grays[20], lwd = segmentlinewidth)

lines(xx1, riskreturn(xx1), col = COLA[4], lwd = graphlinewidth, lty = 1)
lines(xx1, indiffA(xx1), col = COLB[4], lwd = graphlinewidth, lty = 1)
lines(xx1, indiffA(xx1, intercept = 10.35, slope = 0.15), col = COLB[4], lwd = graphlinewidth, lty = 1)


#Add points a, b, c and c

points(5.2, riskreturn(g = 5.2), pch = 16, col = "black", cex = 1.5)
text(5.2 + 0.3, riskreturn(g = 5.2) - 0.35, expression(a), cex = labelsize)


#Risk neutral segment line
#segments(0, riskreturn(retmax), xlims[2], riskreturn(g = retmax) , lty = 2, col = COLB[4], lwd = graphlinewidth)

points(retmax, riskreturn(g = retmax), pch = 16, col = "black", cex = 1.5)
text(retmax + 0.3, riskreturn(g = retmax) - 0.65, expression(m), cex = labelsize)


# #Label risk return schedule
text(13, 14.2, expression(paste(y(Delta))), cex = labelsize)
text(13, 13.3, expression(paste("Risk-return")), cex = labelsize)
text(13, 12.5, expression(paste("schedule")), cex = labelsize)
# 
# #Label value functions
text(9.8, 19.5, expression(u[1]), cex = labelsize)
text(7.3, 19.5, expression(u[2]), cex = labelsize)
# text(14, 17.5, expression(u[3]), cex = labelsize)


text(4, 22.7, expression(paste("Risk-averse")), xpd = TRUE, cex = labelsize) 
text(4, 21.8, expression(paste("investor's")), xpd = TRUE, cex = labelsize) 
text(4, 21.0, expression(paste("highest feasible")), xpd = TRUE, cex = labelsize) 
text(4, 20.1, expression(paste("indifference curve")), xpd = TRUE, cex = labelsize) 

text(13, 22.7, expression(paste("Risk-averse")), xpd = TRUE, cex = labelsize) 
text(13, 21.8, expression(paste("investor's")), xpd = TRUE, cex = labelsize) 
text(13, 21.0, expression(paste("fallback")), xpd = TRUE, cex = labelsize) 
text(13, 20.1, expression(paste("indifference curve")), xpd = TRUE, cex = labelsize) 




dev.off()

