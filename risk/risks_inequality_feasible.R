#Graph Designer: Simon Halliday & Riley Boeth '17 + Scott Cohn
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "risk/risks_inequality_feasible.pdf", width = 10, height = 8)

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
grays <- gray.colors(25, start = 1, end = 0)

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 6, 1, 1))


#Indifference curves of a risk-averse homo economicus (2nd graph out of the two for 4.7)

indiffA <- function(x, ua=2) {
  ua + (1/12)*(x)^2
}

#Average income function 

avgwealth <- function(x,y){
  (1.08)*((-1/35)*(((-((.9)*x-36))^2))+36.8+((-1/10)*(x-4)+(-1/1000)*((x-3)^3)))
}

#Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 30)
xlims <- c(0, 40)

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

#ticksx <- seq(from = 0, to = xlims[2]+1, by = 4)
#xlabels <- seq(from = 0, to = xlims[2], by = 1)
ticksx <- c(xlims[1], 12, 20, 25, xlims[2])
xlabels <- c(NA, expression(paste(Delta^P)), expression(paste(bar(Delta) )), expression(paste(Delta^R)), NA)
#ticksy <- seq(from = 0, to = ylims[2]+1, by = 4)
#ylabels <- seq(from = 0, to = ylims[2], by = 1)
ticksy <- c(ylims[1], avgwealth(12), avgwealth(20), avgwealth(25), ylims[2])
ylabels <- c(NA, expression(paste(y^P)), expression(paste(bar(y) )), expression(paste(y^R)), NA)



axis(1,at = ticksx,  pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)


# lines(xx1, indiffA(xx1, ua = 2), col = COLB[4], lwd = graphlinewidth)
# lines(xx1, indiffA(xx1, ua = 8.2), col = COLB[4], lwd = graphlinewidth)
# lines(xx1, indiffA(xx1, ua = 14.4), col = COLB[4], lwd = graphlinewidth)
lines(xx1, avgwealth(xx1), col = COLA[4], lwd = graphlinewidth)


#Axis labels and draw linear utility function
mtext(expression(paste("Inequality of income, ", Delta , " (difference between rich and poor)")), side = 1, line = 2.5, cex = axislabelsize)
text(-4, 0.5*ylims[2], expression(paste("Average income, ",bar(y))), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label various points on line

segments(0, avgwealth(25), 25, avgwealth(25), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(25, 0, 25, avgwealth(25), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(25, avgwealth(25), pch = 16, col = "black", cex = 1.5)
text(25, avgwealth(25) + 0.75, expression(paste(R)), cex = labelsize)

segments(0, avgwealth(20), 20, avgwealth(20), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(20, 0, 20, avgwealth(20), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(20, avgwealth(20), pch = 16, col = "black", cex = 1.5)
text(20, avgwealth(20) + 0.75, expression(paste(m)), cex = labelsize)


segments(0, avgwealth(12), 12, avgwealth(12), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(12, 0, 12, avgwealth(12), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(12, avgwealth(12), pch = 16, col = "black", cex = 1.5)
text(12, avgwealth(12) + 0.75, expression(paste(P)), cex = labelsize)


#Label average wealth curve and indifference curves generally

text(34, avgwealth(30)+1, expression(paste("Average income")), xpd = TRUE, cex = labelsize)
text(34, avgwealth(30), expression(paste("function")), xpd = TRUE, cex = labelsize)
text(34, avgwealth(30)-1.5, expression(paste(bar(y) == f(Delta) )), xpd = TRUE, cex = labelsize)



dev.off()

