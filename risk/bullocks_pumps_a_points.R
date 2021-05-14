#Graph Designer(s): Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
source("braces.R")
require(shape)

pdf(file = "risk/bullock_pumps_a_points.pdf", width = 10, height = 8)

# The utility function and indifference curve
U <- function(y, delta, a = 1.5, b = 0.5, c = 2){
  y^a - b * delta^c
}

indiff <- function(u, delta, a = 1.5, b = 0.5, c = 2){
  (u + b * delta^c )^(1/a)
}
# Assign the parameters

yA  <-  3 # expected income investing on bullock
deltaA <-  1 # risk with investment on bullock
yB  <-  6 # expected income investing on pumps
deltaB  <-  5 # risk with investment on pumps
L <- 5 # lottery
yAprime <-  yA + L # expected income investing on bullock with lottery
yBprime <- yB + L  # expected income investing on pumps with lottery

# compare the utilities with (yA,deltaA), (yB,deltaB), (yAprime, deltaA), and (yBprime, deltaB)
U(yA,deltaA)
U(yB,deltaB)
U(yAprime,deltaA)
U(yBprime,deltaB)

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

#Edited the margins to cater for the larger LHS labels
#par(mfrow = c(1,2))
par(mar =  c(6, 6, 1, 0.5))
#Add limits on axes and levels of utility for each indifference curve
xlims <- c(0, deltaB + 2)
ylims <- c(0, yBprime + 2)

npts <- 500
xx1 <- seq(xlims[1], xlims[2] - 1, length.out = npts)
# the first plot

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("Risk, ",Delta)),
     ylab = expression(paste("Expected income, ", hat(y))),
     xaxt = "n",
     yaxt = "n",
     cex.lab = axislabelsize,
     bty = "n",
     xaxs = "i",
     yaxs = "i"
)

ticksx <- c(0,1,5,6.6) #seq(from = 0, to = xlims[2],by = 1)
xlabels <- c(NA, expression(paste(Delta[b])), expression(paste(Delta[i])), NA)
ticksy <- c(0, yA, yB, ylims[2]) #seq(from = 0, to = ylims[2], by = 3)
ylabels <- c(0, expression(paste(hat(y)[b])), expression(paste(hat(y)[i])), NA)

axis(1,at = ticksx,  pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

# draw the indifference curves
#lines(xx1, indiff(xx1, u = U(yA,deltaA)), col = COLA[4], lwd = graphlinewidth) # u2
# lines(xx1, indiff(xx1, u = U(yAprime,deltaA)), col = COLA[4], lwd = graphlinewidth) # u3
#lines(xx1, indiff(xx1, u = U(yB,deltaB)), col = COLA[4], lwd = graphlinewidth) # u1
# lines(xx1, indiff(xx1, u = U(yBprime,deltaB)), col = COLA[4], lwd = graphlinewidth) # u4

# add segments
segments(deltaA, 0, deltaA, yA, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(deltaB, 0, deltaB, yB, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, yA, deltaB, yA, lty = 2, col = grays[20] , lwd = segmentlinewidth)
# segments(0, yAprime, deltaA, yAprime, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, yB, deltaB, yB, lty = 2, col = grays[20] , lwd = segmentlinewidth)
# segments(0, yBprime, deltaB, yBprime, lty = 2, col = grays[20] , lwd = segmentlinewidth)
# braces(deltaA + 0.1, deltaA + 0.2, yA + 0.1, yAprime, 1/4, mid = 0.5 * (yA+yBprime) / (yA + yAprime))
# braces(deltaB - 0.1, deltaB - 0.2, yB, yBprime, 1/4, mid = 0.5 * (yA+yBprime) / (yB + yBprime))
braces(deltaB + 0.1, deltaB + 0.2, yA + 0.1, yB, 0.5)
# Arrows(2.1, (yA + yBprime) / 2, 1.4, (yA + yBprime) / 2 ,
#        col = "black", lty = 1, lwd = 2, code = 2, arr.type = "triangle", arr.lwd = 0.5)
# Arrows(3.9, (yA + yBprime) / 2, 4.6, (yA + yBprime) / 2 ,
#        col = "black", lty = 1, lwd = 2, code = 2, arr.type = "triangle", arr.lwd = 0.5)

# the four points
points(deltaB, yB, pch = 16, col = "black", cex = 1.5)
#points(deltaB, yBprime, pch = 16, col = "black", cex = 1.5)
points(deltaA, yA, pch = 16, col = "black", cex = 1.5)
#points(deltaA, yAprime, pch = 16, col = "black", cex = 1.5)

#Label 4 points
text(deltaA-0.2, yA-0.5, expression(paste(b)), cex = labelsize)
#text(deltaA-0.2, yAprime-0.5, expression(paste(b*minute)), cex = labelsize)
text(deltaB-0.1, yB-0.4, expression(paste(i)), cex = labelsize)
#text(deltaB-0.1, yBprime+0.3, expression(paste(i*minute)), cex = labelsize)
# text((deltaA + deltaB)/2, (yA + yBprime)/2, 'Non farm income', cex = labelsize)
# 
text(deltaB+1.15, ((yA + yB) / 2) + 0.25, 'Increased expected', cex = labelsize, xpd = TRUE)
text(deltaB+1.15, ((yA + yB) / 2) - 0.35, 'income from pumps', cex = labelsize, xpd = TRUE)


# text(5.6, 6.4, expression(paste(u[1])), cex = labelsize)
# text(5.6, 7.9, expression(paste(u[2])), cex = labelsize)
# text(5.6, 10.7, expression(paste(u[3])), cex = labelsize)
# text(5.6, 12, expression(paste(u[4])), cex = labelsize)

dev.off()