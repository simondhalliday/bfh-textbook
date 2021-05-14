#Graph Designer: Simon Halliday
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(plotrix)
#pdf(file = "capitalism/inequality_households_quiz.pdf", width = 10, height = 8)
jpeg(file = "capitalism/inequality_households_quiz.jpg", width = 6*120, height = 5*120)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 2.2
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(1, 1, 1.5, 2))

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


draw.circle(35, 5, 2.5, nv = 1000, border = COLA[5], col = COLA[1], lty = 1, lwd = 3)
text(35, 6, expression(paste(C)), xpd = TRUE, cex = labelsize)
text(35, 4.6, expression(paste(y^C == 7)), xpd = TRUE, cex = labelsize)


#Arrow between 2 and 3
Arrows(9, 5, 32, 5,  col = COLB[5], lty = 1, lwd = 3, code = 3, arr.type = "triangle", arr.lwd = 0.5)

draw.circle(5, 5, 3.5, nv = 1000, border = COLA[5], col = COLA[1], lty = 1, lwd = 3)
text(5, 6, expression(paste(B)), xpd = TRUE, cex = labelsize)
text(5, 4, expression(paste(y^B == 13)), xpd = TRUE, cex = labelsize)

#Arrow between 3 and 10
Arrows(6.2, 9.4, 16.2, 25.2,  col = COLB[5], lty = 1, lwd = 3, code = 3, arr.type = "triangle", arr.lwd = 0.5)

draw.circle(20, 30, 5, nv = 1000, border = COLA[5], col = COLA[1], lty = 1, lwd = 3)
text(20, 31, expression(paste(A)), xpd = TRUE, cex = labelsize)
text(20, 29, expression(paste(y^A == 25)), xpd = TRUE, cex = labelsize)

#Arrow between 2 and 10
Arrows(23.8, 25.3, 34, 8.4,  col = COLB[5], lty = 1, lwd = 3, code = 3, arr.type = "triangle", arr.lwd = 0.5)

#Difference between A and C
text(34, 18, expression(paste(Delta^{AC} == phantom(), y^A - y^C)), xpd = TRUE, cex = labelsize)
#Difference between A and B
text(6, 18, expression(paste(Delta^{AB} == phantom(), y^A - y^B)), xpd = TRUE, cex = labelsize)
#Difference between B and C
text(20, 3, expression(paste(Delta^{BC} == phantom(), y^B - y^C)), xpd = TRUE, cex = labelsize)



#text(20, 16, expression(paste(Gini== bgroup("(", frac(Sigma*Delta^{ij}, (n^2 - n)/2),")")*bgroup("(",frac(1,bar(y)),")")*bgroup("(",frac(1,2),")"))), cex = labelsize)
#text(20, 14, expression(paste(Gini == bgroup("(", frac(16, 3),")")*bgroup("(",frac(1,16/3),")")*bgroup("(",frac(1,2),")"), phantom() == 0.5 )), cex = labelsize)

#Bottom of frame
# segments(14, 12, 26, 12, lty = 1, col = "black" , lwd = 1)
# #Top of frame
# segments(14, 16, 26, 16, lty = 1, col = "black" , lwd = 1)
# #Left of frame
# segments(14, 12, 14, 16, lty = 1, col = "black" , lwd = 1)
# #Right of frame
# segments(26, 12, 26, 16, lty = 1, col = "black" , lwd = 1)


dev.off()


