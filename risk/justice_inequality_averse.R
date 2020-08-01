#Graph Designer: Simon Halliday
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "risk/justice_inequality_averse.pdf", width = 10, height = 8)


#Set parameters for graphics
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

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 8, 1, 1))

#Indifference curves of a risk-averse homo economicus (2nd graph out of the two for 4.7)

indiffA <- function(x, uA = 2, slope = 1/10) {
  uA + slope*(x)^2
}

#Average wealth function 
a <- 0.25
b <- 1
c <- 0.2

avgwealth <- function(delta, a = 0.25, b = 1, c = 0.2){
  a + b*delta - c*delta^2
}



#Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 2)
xlims <- c(0, 4)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs = "i", 
     yaxs = "i"
)

#x and y limits with plain axes without ticks/numbers to match previous graph

# ticksx <- c(xlims[1], (b - 0.5)/(2*c),  (b)/(2*c), (b + 0.5)/(2*c), xlims[2])
# xlabels <- c(NA, expression(paste(Delta^P)), expression(paste(Delta[m] )), expression(paste(Delta^R)), NA)
ticksx <- c(xlims[1], 1.65, xlims[2])
xlabels <- c(NA, expression(paste(Delta[a])), NA)


#ticksy <- seq(from = 0, to = ylims[2]+1, by = 4)
# ticksy <- c(ylims[1], avgwealth((b - 0.5)/(2*c)), avgwealth((b)/(2*c)), ylims[2])
# ylabels <- c(NA, expression(paste(underline(y)^P == underline(y)^R )), expression(paste(underline(y)[m])),  NA)
ticksy <- c(ylims[1], avgwealth(1.65), ylims[2])
ylabels <- c(NA, expression(paste(underline(y)[a])), NA)



axis(1,at = ticksx,  pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2,at = ticksy,  pos = xlims[1], labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Indifference curves
lines(xx1, indiffA(xx1, uA = 0.75), col = COLB[4], lwd = graphlinewidth)
lines(xx1, indiffA(xx1, uA = 1.086), col = COLB[4], lwd = graphlinewidth)
lines(xx1, indiffA(xx1, uA = 1.43), col = COLB[4], lwd = graphlinewidth)

#Feasible frontier
lines(xx1, avgwealth(xx1), col = COLA[4], lwd = graphlinewidth)


#Axis labels and draw linear utility function
mtext(expression(paste("Inequality of income, ", Delta, ", (difference between rich and poor)")), side = 1, line = 3, cex = axislabelsize)
text(-0.6, 0.5*ylims[2], expression(paste("Average income, ", underline(y))), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Inequality-neutral indifference curves
# segments(0, avgwealth((b - 0.5)/(2*c)), xlims[2], avgwealth((b - 0.5)/(2*c)), lty = 1, col = COLB[4], lwd = graphlinewidth)
# segments(0, avgwealth((b)/(2*c)) , xlims[2], avgwealth((b)/(2*c)) , lty = 1, col = COLB[4], lwd = graphlinewidth)
# segments(0, avgwealth((b)/(2*c)) + 0.3, xlims[2], avgwealth((b)/(2*c)) + 0.3, lty = 1, col = COLB[4], lwd = graphlinewidth)

#Label various points on line
#segments(0, avgwealth(b/(2*c)), b/(2*c), avgwealth(b/(2*c)), lty = 2, col = grays[20], lwd = segmentlinewidth)
#segments(b/(2*c), 0, b/(2*c), avgwealth(b/(2*c)), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(b/(2*c), avgwealth(delta = b/(2*c)), pch = 16, col = "black", cex = 1.5)
text(b/(2*c), avgwealth(delta = b/(2*c)) + 0.07, expression(paste(m)), cex = labelsize)


#segments(0, avgwealth(delta = (b + 0.5)/(2*c)), (b + 0.5)/(2*c), avgwealth(delta = (b + 0.5)/(2*c)), lty = 2, col = grays[20], lwd = segmentlinewidth)
#segments((b + 0.5)/(2*c), 0, (b + 0.5)/(2*c), avgwealth(delta = (b + 0.5)/(2*c)), lty = 2, col = grays[20], lwd = segmentlinewidth)
points((b + 0.5)/(2*c), avgwealth(delta = (b + 0.5)/(2*c)), pch = 16, col = "black", cex = 1.5)
text((b + 0.5)/(2*c), avgwealth(delta = (b + 0.5)/(2*c)) + 0.07, expression(paste(R)), cex = labelsize)

#segments(0, avgwealth((b - 0.5)/(2*c)), (b - 0.5)/(2*c), avgwealth((b - 0.5)/(2*c)), lty = 2, col = grays[20], lwd = segmentlinewidth)
#segments((b - 0.5)/(2*c), 0, (b - 0.5)/(2*c), avgwealth((b - 0.5)/(2*c)), lty = 2, col = grays[20], lwd = segmentlinewidth)
points( (b - 0.5)/(2*c), avgwealth(delta = (b - 0.5)/(2*c)), pch = 16, col = "black", cex = 1.5)
text((b - 0.5)/(2*c) + 0.08, avgwealth(delta = (b - 0.5)/(2*c)) - 0.05, expression(paste(P)), cex = labelsize)

segments(0, avgwealth(1.65), 1.65, avgwealth(1.65), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(1.65, 0, 1.65, avgwealth(1.65), lty = 2, col = grays[20], lwd = segmentlinewidth)
text(1.65, avgwealth(delta = 1.65) + 0.05, expression(paste(a)), cex = labelsize)
points( 1.65, avgwealth(delta = 1.65), pch = 16, col = "black", cex = 1.5)

text(0.62, avgwealth(delta = 0.62) + 0.05, expression(paste(b)), cex = labelsize)
points(0.62, avgwealth(delta = 0.62), pch = 16, col = "black", cex = 1.5)

text(2.7 + 0.05, avgwealth(delta = 2.7) - 0.05, expression(paste(c)), cex = labelsize)
points( 2.72, avgwealth(delta = 2.72), pch = 16, col = "black", cex = 1.5)


#label the three indifference curves

text(0.25, 0.8, expression(paste(u[1])), xpd = TRUE, cex = labelsize)
text(0.25, 1.15, expression(paste(u[2])),  xpd = TRUE, cex = labelsize)
text(0.25, 1.5, expression(paste(u[3])),  xpd = TRUE, cex = labelsize)

#Label average wealth curve and indifference curves generally

text(0.8, avgwealth(0.8)-0.2, expression(paste("Average")), xpd = TRUE, cex = labelsize)
text(0.8, avgwealth(0.8)-0.27, expression(paste("income")), xpd = TRUE, cex = labelsize)
text(0.8, avgwealth(0.8)-0.35, expression(paste("curve")), xpd = TRUE, cex = labelsize)
text(0.8, avgwealth(0.8)-0.44, expression(paste(underline(y)(Delta) )), xpd = TRUE, cex = labelsize)

Arrows(0.9, 1.72, 0.65, 2, col = "black", lty = 1, lwd = 2, arr.type = "triangle", xpd = TRUE)
text(1.28, 2, expression(paste("Better")), xpd = TRUE, cex = labelsize)
text(1.28, 1.93, expression(paste("(inequality-averse")), xpd = TRUE, cex = labelsize)
text(1.28, 1.86, expression(paste("Spectator)")), xpd = TRUE, cex = labelsize)

dev.off()

