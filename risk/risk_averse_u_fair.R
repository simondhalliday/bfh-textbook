#Graph Designer(s): Simon Halliday & Riley Boeth '17
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "risk/risk_averse_u_fair.pdf", width = 10, height = 8)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 10, 4, 4))

#Concave utility of wealth function

ConcaveU <- function(x,y){
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

ticksy <- c(0, ConcaveU(2), ConcaveU(36), ylims[2])
ylabels <- c(NA, NA, NA, ylims[2])
ticksx <- c(0, 2, 9.7, 19, 36, xlims[2])
xlabels <- c(NA, expression(paste(y - delta[2])), expression(paste(y[0])), expression(paste(y)), expression(paste(y + delta[1])), NA)

axis(1,at = ticksx,  pos = 0, labels = xlabels)
axis(2,at = ticksy,  pos = 0, labels = FALSE, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Axis labels and draw linear utility function
mtext(expression(paste("Income, y")), side = 1, line = 2.5, cex = axislabelsize)
text(-9.2, 0.5*ylims[2], expression(paste("Utility, u(y)")), xpd = TRUE, cex = axislabelsize, srt = 90) 

lines(xx1, ConcaveU(xx1, y), col = COLA[5], lwd = graphlinewidth)

#Add line from pt a to pt c

segments(2, ConcaveU(2), 36, ConcaveU(36), lty = 1, col = COLB[4] , lwd = graphlinewidth)


#Label 5 points on line

text(2.7, ConcaveU(2)-.5, expression(paste("a")), cex = labelsize)
segments(2, 0, 2, ConcaveU(2), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, ConcaveU(2), 2, ConcaveU(2), lty = 2, col = "gray", lwd = segmentlinewidth)
points(2, ConcaveU(2), pch = 16, col = "black", cex = 1.5)


text(19+1, ConcaveU(19)-.5, expression(paste("c")), cex = labelsize)
segments(19, 0, 19, ConcaveU(19), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, ConcaveU(19), 19, ConcaveU(19), lty = 2, col = "gray", lwd = segmentlinewidth)
points(19, ConcaveU(19), pch = 16, col = "black", cex = 1.5)

text(19.8, ConcaveU(9.7)-.5, expression(paste("e")), cex = labelsize)
segments(0, ConcaveU(9.7), 19, ConcaveU(9.7), lty = 2, col = "gray", lwd = segmentlinewidth)
points(19, ConcaveU(9.7), pch = 16, col = "black", cex = 1.5)

text(9.7 + 0.8, ConcaveU(9.7)-.75, expression(paste("b")), cex = labelsize)
segments(9.7, 0, 9.7, ConcaveU(9.7), lty = 2, col = "gray", lwd = segmentlinewidth)
points(9.7, ConcaveU(9.7), pch = 16, col = "black", cex = 1.5)

text(36.8, ConcaveU(36)-.9, expression(paste("d")), cex = labelsize)
segments(36, 0, 36, ConcaveU(36), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, ConcaveU(36), 36, ConcaveU(36), lty = 2, col = "gray", lwd = segmentlinewidth)
points(36, ConcaveU(36), pch = 16, col = "black", cex = 1.5)

#Label y-sub,x-sub,etc. on axes

#SH: removed these because I could add them to x-axis; y-axis labels cannot be added as usual
#text(36, -.9, expression(paste(y + Delta*y[1])), xpd = TRUE, cex = labelsize)
#text(13, -.9, expression(paste(y[ce])),  xpd = TRUE,  cex = labelsize)
#text(2, -.9, expression(paste(y - Delta*y[2])),  xpd = TRUE,  cex = labelsize)
#text(23.18, -.9, expression(paste(y)),  xpd = TRUE,  cex = labelsize)


text(-2.7, ConcaveU(36), expression(paste(u(y + delta[1]))), xpd = TRUE, cex = labelsize)
text(-1.95, ConcaveU(9.7)+1.7, expression(paste(u(y[0]) == phantom() )),  xpd = TRUE, cex = labelsize)
text(-3.1, ConcaveU(9.7), expression(paste(p%.%u(y +  delta[1]) + phantom())),  xpd = TRUE, cex = labelsize)
text(-4.3, ConcaveU(9.7)-1.5, expression(paste((1 - p)%.%u(y - delta[2]))),  xpd = TRUE, cex = labelsize)
text(-1.8, ConcaveU(9.7)-3, expression(paste(phantom() == v(L))),  xpd = TRUE, cex = labelsize)
text(-2.7, ConcaveU(2), expression(paste(u(y - delta[2]))),  xpd = TRUE,  cex = labelsize)
text(-1.5, ConcaveU(19), expression(paste(u(y))),  xpd = TRUE,  cex = labelsize)

#Add risk premium distance arrow and label

Arrows(10.3, ConcaveU(2), 19 - 0.5, ConcaveU(2), col = "black", lty = 1, lwd = 2, code = 3, arr.type = "triangle", arr.lwd = 0.5)
text((19+9.7)/2, ConcaveU(2)+1, expression(paste("Risk Premium")),  xpd = TRUE,  cex = labelsize)


dev.off()