#Graph Designer(s): Simon Halliday,  Riley Boeth '17 & Weikai Chen
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
require(shape)

pdf(file = "risk/education.pdf", width = 20, height = 10)

# The utility function and indifference curve

U <- function(y,delta, alpha = 0.2, beta = 2){
  y - alpha * delta ^ beta
}

indiff <- function(delta, u = 2, alpha = 0.2, beta = 2){
    u + alpha * delta ^beta
}

                                        # Assign the parameters

y0  <-  5 # expected income without continuing education
delta0 <-  0 # no risk without continuing education
yR  <-  12 # expected income with higher education after paying tuition
deltaR  <-  7 # risk with education
C <- 5 # tuition
yRprime <-  yR + C # expected income with free higher education
deltaRprime  <-  deltaR
tau  <- C / yRprime # tax rate
yT  <- yRprime * (1 - tau) # expected income with free education paying with tax
deltaT  <-  deltaRprime * (1 - tau) # risk with free education paying with tax


                                        # compare the utilities with (y0,delta0), (yR,deltaR), (yT, deltaT)
## U(y0,delta0)
## U(yR,deltaR)
## U(yT,deltaT)

                                        #Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

                                        #Edited the margins to cater for the larger LHS labels
par(mfrow = c(1,2))
par(mar =  c(6, 8, 4, 2))
                                        #Add limits on axes and levels of utility for each indifference curve
xlims <- c(0, deltaR + 2)
ylims <- c(0, yRprime + 1)


npts <- 501
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts)

# show the colors define
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
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
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

ticksx <- seq(from = 0, to = xlims[2],by = 2)
xlabels <- seq(from = 0, to = xlims[2],by = 2)
ticksy <- seq(from = 0, to = ylims[2], by = 3)
ylabels <- seq(from = 0, to = ylims[2], by = 3)

axis(1,at = ticksx,  pos = 0, labels = xlabels)
axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1)

# the point (y0,delta0) and the indifference curve through it
lines(xx1, indiff(xx1, u = U(y0,delta0)), col = COLA[5], lwd = graphlinewidth)
points(delta0, y0, pch = 16, col = COLB[4], cex = 1.5)

# the point (yR, deltaR), (yRprime, deltaRprime), Add segments
segments(deltaR, 0, deltaR, yRprime, lty = 2, col = COLB[2] , lwd = segmentlinewidth)
segments(0, yR, deltaR + 1, yR, lty = 2, col = COLB[2] , lwd = segmentlinewidth)
segments(0, yRprime, deltaR + 1, yRprime, lty = 2, col = COLB[2] , lwd = segmentlinewidth)
points(deltaR, yR, pch = 16, col = COLB[4], cex = 1.5)
points(deltaRprime, yRprime, pch = 16, col = COLB[4], cex = 1.5)


#Label 5 points on line

text(deltaR-0.2, yR-0.5, expression(paste("R")), cex = labelsize)
text(deltaR-0.2, yRprime-0.5, expression(paste("R'")), cex = labelsize)
arrows(deltaR + 0.5, yR, deltaR + 0.5, yRprime, code = 3, length = 0.1, col = COLB[2], lty = 2)
text(deltaR + 0.8, (yR + yRprime)/2, expression(paste("Cost")), cex = labelsize)


# the second plot
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

ticksx <- seq(from = 0, to = xlims[2],by = 2)
xlabels <- seq(from = 0, to = xlims[2],by = 2)
ticksy <- seq(from = 0, to = ylims[2], by = 3)
ylabels <- seq(from = 0, to = ylims[2], by = 3)

axis(1,at = ticksx,  pos = 0, labels = xlabels)
axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1)
# the point (y0,delta0) and the indifference curve through it
lines(xx1, indiff(xx1, u = U(y0,delta0)), col = COLA[5], lwd = graphlinewidth)
points(delta0, y0, pch = 16, col = COLB[4], cex = 1.5)

# the point (yR, deltaR), (yRprime, deltaRprime), Add segments
segments(deltaR, 0, deltaR, yRprime, lty = 2, col = COLB[2] , lwd = segmentlinewidth)
segments(0, yR, deltaR + 1, yR, lty = 2, col = COLB[2] , lwd = segmentlinewidth)
segments(0, yRprime, deltaR + 1, yRprime, lty = 2, col = COLB[2] , lwd = segmentlinewidth)
points(deltaR, yR, pch = 16, col = COLB[4], cex = 1.5)
points(deltaRprime, yRprime, pch = 16, col = COLB[4], cex = 1.5)
# the point (yT, deltaT) and the segment
points(deltaT, yT, pch = 16, col = COLB[4], cex = 1.5)
segments(0,0, deltaR, yRprime, lty = 2, col = COLB[2], lwd = segmentlinewidth)
#Label 5 points on line

text(deltaR-0.2, yR-0.5, expression(paste("R")), cex = labelsize)
text(deltaR-0.2, yRprime-0.5, expression(paste("R'")), cex = labelsize)
arrows(deltaR + 0.5, yR, deltaR + 0.5, yRprime, code = 3, length = 0.1, col = COLB[2], lty = 2)
text(deltaR + 0.8, (yR + yRprime)/2, expression(paste("Tax")), cex = labelsize)
text(deltaT, yT - 0.5, expression(R^T), cex = labelsize)


dev.off()
