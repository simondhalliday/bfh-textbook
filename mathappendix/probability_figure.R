#Graph Designer: HBG
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics
library(shape)
pdf(file = "mathappendix/park_vs_zoo.pdf", width = 10, height = 8)
#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 5, 2, 2))

#proportion of wealth functions

expected_pay <- function(x){
  1 +  5*x
}

expected_dont <- function(x){
  2 +  3*x
}


#Add limits on axes and levels of utility for each function 
ylims <- c(0, 6)
xlims <- c(0, 1)

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
     xaxs="i", 
     yaxs="i"
)

#x and y limits with plain axes without ticks/numbers to match previous graph; y axes on both sides

ticksx <- c(0, 1/3, 2/3, 1)
ticksy <- c(0,  1, 2, 2.66, 3, 4, 4.33, 5, 6)
ylabels <- c(0,  1, 2, 2.66, 3, 4, 4.33, 5, 6)
ticksy2 <- seq(ylims[1], ylims[2], 1)
ylabels2 <- seq(ylims[1], ylims[2], 1)
xlabels1 <- c(0, expression(paste(p == frac(1,3))), 1)
xlabels2 <- c(0, expression(paste(p == frac(2,3))), 1)
#ylabels1 <- c(0, expression(paste(p == 4.33), 1))
#ylabels2 <- c(0, expression(paste(p == 4.33), 1))

#text(x = c(0, 12/27, 1/2, 1), par("usr")[3] - 0.2, labels = xlabels, srt = 45, pos = 1, xpd = TRUE)

axis(1, at = ticksx,  pos = 0, labels = FALSE)
text(x = c(0, 0.66, 1), par("usr")[3] - 0.2, labels = xlabels2, pos = 1, xpd = TRUE)
#text(x = c(0, 0.33, 1), par("usr")[3] - 0.2, labels = xlabels1, pos = 1, xpd = TRUE)
axis(2, at = ticksy,  pos = 0, labels = ylabels, las = 1)
axis(4, at = ticksy2, pos = xlims[2], labels = ylabels2, las=1)
#text(y = c(0, 4.33, 0, 6), par("usr")[3] - 0.2, labels = ylabels1, pos = 1, xpd = TRUE)

 
npts <- 503 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Axis labels and draw linear utility functions
text(xlims[2]*0.5, -0.75, expression(paste("Probability or population proportion, p")), xpd = TRUE, cex = axislabelsize)
text(-0.08, 3, expression(paste("Expected Utility, EU")), xpd = TRUE, cex = axislabelsize, srt = 90)


#text(41, ylims[2], expression(paste(4)), xpd = TRUE, cex = labelsize) 
#text(41, 0.75*ylims[2], expression(paste(3)), xpd = TRUE, cex = labelsize) 

lines(xx1, expected_pay(xx1), col = COLB[4], lwd = graphlinewidth)
lines(xx1, expected_dont(xx1), col = COLA[5], lwd = graphlinewidth)

segments(1/3, 0, 1/3, expected_dont(1/3), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(2/3, 0, 2/3, expected_pay(2/3), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, 3, 0.33, 3, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, 4.33, 0.66, 4.33, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, 2.66, 0.33, 2.66, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, 4, 0.666, 4, lty = 2, col = "gray", lwd = segmentlinewidth)


text(0.9, 5.8, expression(paste("EU(a)")), xpd = TRUE, cex = labelsize)
text(0.9, 4.4, expression(paste("EU(z)")), xpd = TRUE, cex = labelsize)
points(1/3, expected_dont(1/3), pch = 16, col = "black", cex = 1)
points(2/3, expected_pay(2/3), pch = 16, col = "black", cex = 1)
points(2/3, expected_dont(2/3), pch = 16, col = "black", cex = 1)
points(1/3, expected_pay(1/3), pch = 16, col = "black", cex = 1)




dev.off()
