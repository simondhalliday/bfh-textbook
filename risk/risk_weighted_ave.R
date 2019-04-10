#Graph Designer(s): Simon Halliday & Riley Boeth '17
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "risk/risk_weighted_ave.pdf", width = 10, height = 8)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 6, 2, 2))

#Concave utility of wealth function

ConcaveU <- function(x){
  (1600-(x-40)^2)^(1/2)
}

#Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 42)
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
     xaxs="i" 
     #yaxs="i"
)

#x and y limits with plain axes without ticks/numbers to match previous graph

pointbx <- 10
pointcx <- 25

ticksy <- c(0, ConcaveU(2), ConcaveU(2) - (ConcaveU(36) - ConcaveU(2))/(36 - 2)*2 + pointbx*(ConcaveU(36) - ConcaveU(2))/(36 - 2), ConcaveU(2) - (ConcaveU(36) - ConcaveU(2))/(36 - 2)*2 + pointcx*(ConcaveU(36) - ConcaveU(2))/(36 - 2), ConcaveU(36), ylims[2])
ylabels <- c(NA, expression(paste(u(y - delta[2]))), expression(paste(v(L^underline(p)))), expression(paste(v(L^{bar(p)}))), expression(paste(u(y + delta[1]))), NA)
ticksx <- c(0, 2, pointbx, pointcx, 36, xlims[2])
xlabels <- c(NA, expression(paste(y - delta[2])), expression(paste(y^underline(p))), expression(paste(y^bar(p))), expression(paste(y + delta[1])), NA)

axis(1,at = ticksx,  pos = 0, labels = xlabels)
axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Axis labels and draw linear utility function
mtext(expression(paste("Wealth, y")), side = 1, line = 2.5, cex = axislabelsize)
text(-4.5, 0.5*ylims[2], expression(paste("Utility as a function of wealth, u(y)")), xpd = TRUE, cex = axislabelsize, srt = 90) 

#lines(xx1, ConcaveU(xx1, y), col = COLA[5], lwd = graphlinewidth)

#Add line from pt a to pt c

segments(2, ConcaveU(2), 36, ConcaveU(36), lty = 1, col = COLB[4] , lwd = graphlinewidth)

(ConcaveU(36) - ConcaveU(2))/(36 - 2)


#Label points on line

text(2.7, ConcaveU(2)-.5, expression(paste("a")), cex = labelsize)
segments(2, 0, 2, ConcaveU(2), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, ConcaveU(2), 2, ConcaveU(2), lty = 2, col = "gray", lwd = segmentlinewidth)
points(2, ConcaveU(2), pch = 16, col = "black", cex = 1.5)


# text(24.18, ConcaveU(23.18)-.5, expression(paste("c")), cex = labelsize)
# segments(23.18, 0, 23.18, ConcaveU(23.18), lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, ConcaveU(23.18), 23.18, ConcaveU(23.18), lty = 2, col = "gray", lwd = segmentlinewidth)
# points(23.18, ConcaveU(23.18), pch = 16, col = "black", cex = 1.5)

# text(24.18, ConcaveU(13)-.5, expression(paste("e")), cex = labelsize)
# segments(0, ConcaveU(13), 23.18, ConcaveU(13), lty = 2, col = "gray", lwd = segmentlinewidth)
# points(23.18, ConcaveU(13), pch = 16, col = "black", cex = 1.5)
# 

text(pointbx, 28 , expression(paste("Low probability, ", underline(p),",")), cex = labelsize)
text(pointbx, 26.5 , expression(paste("means a v(L) closer to bad")), cex = labelsize)
text(pointbx, 25 , expression(paste("outcome utility ", u(y - delta[2]))), cex = labelsize)
Arrows(pointbx, 24, pointbx, 20.3, col = "black", lty = 1, lwd = 2, code = 2, arr.type = "triangle", arr.lwd = 0.5)
text(pointbx + 1, ConcaveU(2) - (ConcaveU(36) - ConcaveU(2))/(36 - 2)*2 + pointbx*(ConcaveU(36) - ConcaveU(2))/(36 - 2) - 0.5, expression(paste("b")), cex = labelsize)
segments(0, ConcaveU(2) - (ConcaveU(36) - ConcaveU(2))/(36 - 2)*2 + pointbx*(ConcaveU(36) - ConcaveU(2))/(36 - 2), pointbx, ConcaveU(2) - (ConcaveU(36) - ConcaveU(2))/(36 - 2)*2 + pointbx*(ConcaveU(36) - ConcaveU(2))/(36 - 2), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(pointbx, 0, pointbx, ConcaveU(2) - (ConcaveU(36) - ConcaveU(2))/(36 - 2)*2 + pointbx*(ConcaveU(36) - ConcaveU(2))/(36 - 2), lty = 2, col = "gray", lwd = segmentlinewidth)
points(pointbx, ConcaveU(2) - (ConcaveU(36) - ConcaveU(2))/(36 - 2)*2 + pointbx*(ConcaveU(36) - ConcaveU(2))/(36 - 2), pch = 16, col = "black", cex = 1.5)

text(pointcx, 38.5 , expression(paste("High probability, ", bar(p), "," )), cex = labelsize)
text(pointcx, 37 , expression(paste("means a v(L) closer to good")), cex = labelsize)
text(pointcx, 35.5 , expression(paste("outcome utility ", u(y + delta[1]))), cex = labelsize)
Arrows(pointcx, 34.5, pointcx, 32.2, col = "black", lty = 1, lwd = 2, code = 2, arr.type = "triangle", arr.lwd = 0.5)

text(pointcx + 1, ConcaveU(2) - (ConcaveU(36) - ConcaveU(2))/(36 - 2)*2 + pointcx*(ConcaveU(36) - ConcaveU(2))/(36 - 2) - 0.5, expression(paste("c")), cex = labelsize)
segments(0, ConcaveU(2) - (ConcaveU(36) - ConcaveU(2))/(36 - 2)*2 + pointcx*(ConcaveU(36) - ConcaveU(2))/(36 - 2), pointcx, ConcaveU(2) - (ConcaveU(36) - ConcaveU(2))/(36 - 2)*2 + pointcx*(ConcaveU(36) - ConcaveU(2))/(36 - 2), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(pointcx, 0, pointcx, ConcaveU(2) - (ConcaveU(36) - ConcaveU(2))/(36 - 2)*2 + pointcx*(ConcaveU(36) - ConcaveU(2))/(36 - 2), lty = 2, col = "gray", lwd = segmentlinewidth)
points(pointcx, ConcaveU(2) - (ConcaveU(36) - ConcaveU(2))/(36 - 2)*2 + pointcx*(ConcaveU(36) - ConcaveU(2))/(36 - 2), pch = 16, col = "black", cex = 1.5)


text(36.8, ConcaveU(36)-.9, expression(paste("d")), cex = labelsize)
segments(36, 0, 36, ConcaveU(36), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, ConcaveU(36), 36, ConcaveU(36), lty = 2, col = "gray", lwd = segmentlinewidth)
points(36, ConcaveU(36), pch = 16, col = "black", cex = 1.5)


dev.off()