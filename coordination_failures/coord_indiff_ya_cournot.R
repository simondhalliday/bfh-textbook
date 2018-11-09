require(shape)
pdf(file = "coordination_failures/coord_indiff_ya_cournot.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.8

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 4, 4, 4))

indiffA <- function(ea, uA = 46.08) {
  uA + (1/2)*(ea)^2
}

mrsA <- function(ea){
  ea
}

slopeline <- function(ea, yint = 0.5, slope = 2){
  yint + slope*ea
}


xlims <- c(0, 12)
ylims <- c(0, 70)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(12, 46.08, 90)

#B's value when at A's bliss point
#0.35*log(5.88) + 0.35*log(8.88) + 0.5*log(10 - 5.88) + 0.5*log(15 - 8.88) 

#B's bliss point x = 4.11765; y = 6.17647
#A's value when at A's bliss point
#0.5*log(4.11765) + 0.5*log(6.17647) + 0.35*log(10 - 4.11765) + 0.35*log(15 - 6.17647) 


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
ticksx <- seq(from = 0, to = xlims[2], by = 2)
xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(0, 10, 28, 46.08, 70)
ylabels <- c(NA, expression(paste(y[1]^A)), expression(paste(y[2]^A)), expression(paste(y[3]^A)), NA)
# ticksx <- c(0, 6.9, 12, 9.6, 16, 24, 26)
# xlabels <- c(NA, expression(paste(e^A,"*")), expression(paste(1/2*beta)), expression(paste(e^{AN})), expression(paste(alpha)), expression(paste(1/beta)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, indiffA(xx1, uA = 10), col = COLA[4], lwd = graphlinewidth)
lines(xx1, indiffA(xx1, uA = 28), col = COLA[4], lwd = graphlinewidth)
lines(xx1, indiffA(xx1, uA = 46.08), col = COLA[4], lwd = graphlinewidth)
# lines(xx1, brfB(xx1, alpha = 16, beta = 1/24), col = COLB[4], lwd = graphlinewidth)
# lines(xx1, brfPEA(xx1, alpha = 16, beta = 1/24), col = COLA[4], lwd = graphlinewidth)
# lines(xx1, brfPEB(xx1, alpha = 16, beta = 1/24), col = COLB[4], lwd = graphlinewidth)

# contour(x, y, 
#         outer(x, y, uA),
#         #labels = c("v1", "v2", "v3"),
#         drawlabels = FALSE,
#         col = COLA[3],
#         #xlab = expression(paste("A's Apples, ", x)),
#         #ylab = expression(paste("A's Oranges, ", y)),
#         #cex.lab = axislabelsize,
#         lwd = graphlinewidth,
#         levels = a, 
#         xaxs="i", 
#         yaxs="i", 
#         add = TRUE)


segments(2, 0, 2, ylims[2], lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(6, 0, 6, ylims[2], lty = 2, col = "gray" , lwd = segmentlinewidth)
# 
# segments(0, 6.9, 6.9, 6.9, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(6.9, 0, 6.9, 6.9, lty = 2, col = "gray" , lwd = segmentlinewidth)

mtext(expression(paste("A's hours, ", h^A)), side=1, line = 2.5, cex = axislabelsize)
text(-1, 35, expression(paste("Output, ", y^A)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
text(4.5, 12, expression(paste("slope", phantom()==h^A, phantom() == 2)))
Arrows(3.6, 12, 2.4, 12, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(8.5, 28, expression(paste("slope", phantom()==h^A, phantom() == 6)))
Arrows(7.6, 28, 6.4, 28, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the iso-welfare functions for the HG, Aisha
text(9.7, 60, expression(u[1]^A))
text(7.6, 60, expression(u[2]^A))
text(4.8, 60, expression(u[3]^A))
#text(6.6, 8.3, expression(u[4]^A))



lowint <- c(8, 26, 44)

slopex1 <- seq(1,3,length.out = 200)
lines(slopex1, slopeline(slopex1, yint = lowint[1], slope = mrsA(ea = 2)), col = Grays[18], lty = 2, lwd = segmentlinewidth)
lines(slopex1, slopeline(slopex1, yint = lowint[2], slope = mrsA(ea = 2)), col = Grays[18], lty = 2, lwd = segmentlinewidth)
lines(slopex1, slopeline(slopex1, yint = lowint[3], slope = mrsA(ea = 2)), col = Grays[18], lty = 2, lwd = segmentlinewidth)

highint <- c(-8, 10, 28)
slopex2 <- seq(5,7,length.out = 200)
lines(slopex2, slopeline(slopex2, yint = highint[1], slope = mrsA(ea = 6)), col = Grays[18], lty = 2, lwd = segmentlinewidth)
lines(slopex2, slopeline(slopex2, yint = highint[2], slope = mrsA(ea = 6)), col = Grays[18], lty = 2, lwd = segmentlinewidth)
lines(slopex2, slopeline(slopex2, yint = highint[3], slope = mrsA(ea = 6)), col = Grays[18], lty = 2, lwd = segmentlinewidth)


firstpointsx <- c(2, 2, 2)
firstpointsy <- c(indiffA(ea = firstpointsx[1], uA = 10), indiffA(ea = firstpointsx[2], uA = 28), indiffA(ea = firstpointsx[3], uA = 46.08))
points(firstpointsx, firstpointsy, pch = 16, col = "black", cex = 1.5)

secondpointsx <- c(6, 6, 6)
secondpointsy <- c(indiffA(ea = 6, uA = 10), indiffA(ea = 6, uA = 28), indiffA(ea = 6, uA = 46.08))
points(secondpointsx, secondpointsy, pch = 16, col = "black", cex = 1.5)


dev.off()
