#Graph Designer: Simon Halliday & Riley Boeth '17
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(plotrix)
pdf(file = "risk/inequality_households.pdf", width = 10, height = 8)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.5
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(1, 1, 1, 2))

#Indifference curves of a risk-averse homo economicus (2nd graph out of the two for 4.7)

#Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 36)
xlims <- c(1, 39)

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


#axis(1,at = ticksx,  pos = 0, labels = xlabels)
#axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1)


draw.circle(35, 5, 2, nv = 1000, border = COLA[5], col = COLA[1], lty = 1, lwd = 3)
text(35, 6, expression(paste(HH^C)), xpd = TRUE, cex = labelsize)
text(35, 4, expression(paste(y^C == 2)), xpd = TRUE, cex = labelsize)


#Arrow between 2 and 4
Arrows(9, 5, 32, 5,  col = COLB[5], lty = 1, lwd = 3, code = 3, arr.type = "triangle", arr.lwd = 0.5)

draw.circle(5, 5, 3, nv = 1000, border = COLA[5], col = COLA[1], lty = 1, lwd = 3)
text(5, 6, expression(paste(HH^B)), xpd = TRUE, cex = labelsize)
text(5, 4, expression(paste(y^B == 4)), xpd = TRUE, cex = labelsize)

#Arrow between 4 and 10
Arrows(6, 9, 16, 25,  col = COLB[5], lty = 1, lwd = 3, code = 3, arr.type = "triangle", arr.lwd = 0.5)

draw.circle(20, 30, 5, nv = 1000, border = COLA[5], col = COLA[1], lty = 1, lwd = 3)
text(20, 31, expression(paste(HH^A)), xpd = TRUE, cex = labelsize)
text(20, 29, expression(paste(y^A == 10)), xpd = TRUE, cex = labelsize)

#Arrow between 2 and 10
Arrows(24, 25, 34, 8,  col = COLB[5], lty = 1, lwd = 3, code = 3, arr.type = "triangle", arr.lwd = 0.5)

#Difference between A and C
text(33, 18, expression(paste(Delta^{AC} == phantom(), y^A - y^C == 8)), xpd = TRUE, cex = labelsize)
#Difference between A and B
text(7, 18, expression(paste(Delta^{AB} == phantom(), y^A - y^B == 6)), xpd = TRUE, cex = labelsize)
#Difference between B and C
text(20, 4, expression(paste(Delta^{BC} == phantom(), y^B - y^C == 2)), xpd = TRUE, cex = labelsize)



text(20, 16, expression(paste(Gini== bgroup("(", frac(Sigma*Delta^{ij}, (n^2 - n)/2),")")*bgroup("(",frac(1,bar(y)),")")*bgroup("(",frac(1,2),")"))), cex = labelsize)
text(20, 12, expression(paste(phantom() == bgroup("(", frac(16, 3),")")*bgroup("(",frac(1,16/3),")")*bgroup("(",frac(1,2),")"), phantom() == 0.5 )), cex = labelsize)

#Bottom of frame
segments(14, 10, 26, 10, lty = 1, col = "black" , lwd = 1)
#Top of frame
segments(14, 18, 26, 18, lty = 1, col = "black" , lwd = 1)
#Left of frame
segments(14, 10, 14, 18, lty = 1, col = "black" , lwd = 1)
#Right of frame
segments(26, 10, 26, 18, lty = 1, col = "black" , lwd = 1)


dev.off()


