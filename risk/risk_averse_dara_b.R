#' Graph Designer(s): Simon Halliday,  Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(pBrackets)
pdf(file = "risk/risk_averse_dara_b.pdf", width = 10, height = 8)

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
par(mar =  c(6, 7, 1, 1))

#Concave utility of wealth function

ConcaveU <- function(x){
  log(x + 1)
}

#Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 4)
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

ticksy <- c(0, ConcaveU(5.15), 2.05*ConcaveU(2), ConcaveU(14.15), ConcaveU(24.5), 3.075*ConcaveU(2), ConcaveU(33.5), ylims[2])
ylabels <- c(NA, expression(paste(u(y[{e^B}] ) )), expression(v(L[e]) ), expression(paste(u(y[e^G] ))), NA, expression(v(L[f]) ), NA, NA)
ticksx <- c(0, 5.15, 9.65, 14.15, 24.5, 29, 33.5, xlims[2])
xlabels <- c(NA, expression(y[{e^B}]), NA, expression(y[{e^G}]), expression(y[{f^B}]), NA, expression(y[{f^G}]), NA) # labels are under axis title labels


axis(1, at = ticksx,  pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy,  pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Axis labels and draw linear utility function
mtext(expression(paste("Income, y")), side = 1, line = 2.5, cex = axislabelsize)
text(-5, 0.5*ylims[2], expression(paste("Utility of income, u(y)")), xpd = TRUE, cex = axislabelsize, srt = 90) 

text(-2.5, ConcaveU(33.5) + 0.05, expression(paste(u(y[f^G] ))), xpd = TRUE, cex = labelsize)
text(-2.5, ConcaveU(24.5) - 0.05, expression(paste(u(y[f^B] ))), xpd = TRUE, cex = labelsize)

# label yhat
text(9.65, -0.2, expression(paste(hat(y)[e])), xpd = TRUE, cex = labelsize)
segments(9.65, 0, 9.65, ConcaveU(9.65) - 0.1, lty = 2, col = grays[20], lwd = segmentlinewidth)

text(29, -0.2, expression(paste(hat(y)[f])), xpd = TRUE, cex = labelsize)
segments(29, 0, 29, ConcaveU(29), lty = 2, col = grays[20], lwd = segmentlinewidth)

# util
lines(xx1, ConcaveU(xx1), col = COLA[5], lwd = graphlinewidth)

#Points eB to eG
segments(5.15, ConcaveU(5.15), 14.15, ConcaveU(14.15), lty = 2, col = COLB[4], lwd = graphlinewidth)

#Point fB to fG
segments(24.5, ConcaveU(24.5), 33.5, ConcaveU(33.5), lty = 2, col = COLB[4], lwd = graphlinewidth)

#Label 4 points on line
#Point eB
text(6, ConcaveU(5.15) - .12, expression(paste(e^B)), cex = labelsize)
segments(5.15, ConcaveU(5.15), 5.15, 0, lty = 2, col = grays[20], lwd = segmentlinewidth)
points(5.15, ConcaveU(5.15), pch = 16, col = "black", cex = 1.5)

#Point eG
text(15, ConcaveU(14.15) - .1, expression(paste(e^G)), cex = labelsize)
segments(14.15, ConcaveU(14.15), 14.15, 0, lty = 2, col = grays[20], lwd = segmentlinewidth)
points(14.15, ConcaveU(14.15), pch = 16, col = "black", cex = 1.5)

#Point fB
text(24.5 + 0.8, ConcaveU(24.5) - .1, expression(paste(f^B)), cex = labelsize)
segments(24.5, ConcaveU(24.5), 24.5, 0, lty = 2, col = grays[20], lwd = segmentlinewidth)
points(24.5, ConcaveU(24.5), pch = 16, col = "black", cex = 1.5)

#Point fG
text(33.5 + 0.8, ConcaveU(33.5) - .1, expression(paste(f^G)), cex = labelsize)
segments(33.5, ConcaveU(33.5), 33.5, 0, lty = 2, col = grays[20], lwd = segmentlinewidth)
points(33.5, ConcaveU(33.5), pch = 16, col = "black", cex = 1.5)

text(xlims[2] - 2, ConcaveU(xlims[2] - 2) + 0.2, expression(paste(u(y))),  xpd = TRUE,  cex = labelsize)

segments(0, 2.05*ConcaveU(2), 9.65, 2.05*ConcaveU(2), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(9.65, 2.05*ConcaveU(2), pch = 16, col = "black", cex = 1.5)

segments(0, 3.075*ConcaveU(2), 29, 3.075*ConcaveU(2), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(29, 3.075*ConcaveU(2), pch = 16, col = "black", cex = 1.5)

# points(13, 1.725*ConcaveU(3), pch = 16, col = "black", cex = 1.5)
# segments(13 - 4.5, ConcaveU(6), 13 + 4.5, ConcaveU(17.5), lty = 2, col = COLB[4], lwd = graphlinewidth)

# delta e
text(9.65, -0.6, expression(paste(Delta[e] == Delta[f])),  xpd = TRUE,  cex = labelsize)
brackets(14.15, -0.3, 5.15, -0.3, h = 0.15, lwd = segmentlinewidth, xpd = TRUE)

# delta f
text(29, -0.6, expression(paste(Delta[f] == Delta[e])),  xpd = TRUE,  cex = labelsize)
brackets(33.5, -0.3, 24.5, -0.3, h = 0.15, lwd = segmentlinewidth, xpd = TRUE)


dev.off()
