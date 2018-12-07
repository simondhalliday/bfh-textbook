#Graph Designer(s): Simon Halliday,  Riley Boeth '17 & Weikai Chen
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
source("braces.R")
require(shape)

pdf(file = "risk/bullock-pumps.pdf", width = 10, height = 10)

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
L <- 10 # lottery
yAprime <-  yA + L # expected income investing on bullock with lottery
yBprime <- yB + L  # expected income investing on pumps with lottery

# compare the utilities with (yA,deltaA), (yB,deltaB), (yAprime, deltaA), and (yBprime, deltaB)
U(yA,deltaA)
U(yB,deltaB)
U(yAprime,deltaA)
U(yBprime,deltaB)

                                        #Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

                                        #Edited the margins to cater for the larger LHS labels
#par(mfrow = c(1,2))
par(mar =  c(6, 8, 4, 2))
                                        #Add limits on axes and levels of utility for each indifference curve
xlims <- c(0, deltaB + 1)
ylims <- c(0, yBprime + 2)


## npts <- 501
## x <- seq(xlims[1], xlims[2], length.out = npts)
## y <- seq(ylims[1], ylims[2], length.out = npts)

#show the colors define
## for (i in 1:8){
##     points(i,i,col = COL[i], pch = 16)
## }
## for (i in 1:6){
##     points(i,i + 1, col = COLA[i],pch = 16)
## }
## for (i in 1:4){
##     points(i,i+2, col = COLB[i], pch  )
## }
npts <- 500
xx1 <- seq(xlims[1], xlims[2]-0.1, length.out = npts)
# the first plot

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("Risk, ",Delta)),
     ylab = expression(paste("Expected Income, ", y)),
     xaxt = "n",
     yaxt = "n",
     cex.lab = axislabelsize,
     bty = "n",
     xaxs="i",
     yaxs="i"
     )

ticksx <- c(0,1,5,6) #seq(from = 0, to = xlims[2],by = 1)
xlabels <- c(NA, expression(paste(Delta[B])), expression(paste(Delta[I])), NA)
ticksy <- c(0, 2.8, 12.9, ylims[2]) #seq(from = 0, to = ylims[2], by = 3)
ylabels <- c(0, expression(paste(c[1])), expression(paste(c[2])), NA)

axis(1,at = ticksx,  pos = 0, labels = xlabels)
axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1)

# draw the indifference curves
lines(xx1, indiff(xx1, u = U(yA,deltaA)), col = COLA[5], lwd = graphlinewidth)
lines(xx1, indiff(xx1, u = U(yAprime,deltaA)), col = COLA[5], lwd = graphlinewidth)


                                        # add segments
segments(deltaA, 0, deltaA, yAprime, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(deltaB, 0, deltaB, yBprime, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, yA, deltaB, yA, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, yAprime, deltaA, yAprime, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, yB, deltaB, yB, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, yBprime, deltaB, yBprime, lty = 2, col = "gray" , lwd = segmentlinewidth)
braces(deltaA, deltaA + 0.2, yA, yAprime, 1/4, mid = 0.5 * (yA+yBprime) / (yA + yAprime))
braces(deltaB, deltaB - 0.2, yB, yBprime, 1/4, mid = 0.5 * (yA+yBprime) / (yB + yBprime))
braces(deltaB, deltaB - 0.2, yA, yB, 0.5)
Arrows(2.3, (yA + yBprime) / 2, 1.4, (yA + yBprime) / 2 ,
       col = "black", lty = 1, lwd = 2, code = 2, arr.type = "triangle", arr.lwd = 0.5)
Arrows(3.7, (yA + yBprime) / 2, 4.6, (yA + yBprime) / 2 ,
       col = "black", lty = 1, lwd = 2, code = 2, arr.type = "triangle", arr.lwd = 0.5)

                                        # the four points
points(deltaB, yB, pch = 16, col = COLB[4], cex = 1.5)
points(deltaB, yBprime, pch = 16, col = COLB[4], cex = 1.5)
points(deltaA, yA, pch = 16, col = COLB[4], cex = 1.5)
points(deltaA, yAprime, pch = 16, col = COLB[4], cex = 1.5)

#Label 4 points
text(deltaA-0.2, yA-0.5, expression(paste("a")), cex = labelsize)
text(deltaA-0.2, yAprime-0.5, expression(paste("a'")), cex = labelsize)
text(deltaB+0.2, yB-0.5, expression(paste("b")), cex = labelsize)
text(deltaB+0.2, yBprime+0.5, expression(paste("b'")), cex = labelsize)
text((deltaA + deltaB)/2, (yA + yBprime)/2, 'Non Farm Income', cex = labelsize)
text(deltaB-0.9, (yA + yB) / 2, 'Increased Expected \n Income from Pumps', cex = labelsize)


text(5.5, 7, expression(paste(u[1])), cex = labelsize)
text(5.5, 15.2, expression(paste(u[2])), cex = labelsize)

dev.off()
