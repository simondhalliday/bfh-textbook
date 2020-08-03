#Graph Designer: Simon Halliday & Riley Boeth '17
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
require(shape)
pdf(file = "capitalism/risks_feasible_neutral.pdf", width = 7, height = 5)


#Set parameters for graphics
axislabelsize <- 2
labelsize <- 1.8
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0)
#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 6, 0.5, 0.5))

#Indifference curves of a risk-averse homo economicus (1st graph out of the two for 4.7)

indiffA1 <- function(x, uA = 5, rmax=  .7, xmax = 15) {
  uA + (1/2)*(rmax/xmax)*(x^2)
}

#rmax*x
#indiffA <- function(x, ua=2) {
#  ua + (1/12)*(x)^2
#}

#Average wealth function 

avgwealth <- function(x){
  (1.08)*((-1/35)*(((-((.9)*x-36))^2))+36.8+((-1/10)*(x-4)+(-1/1000)*((x-3)^3)))
}

#Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 35)
xlims <- c(0, 30)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     bty = "n", 
     xaxs="i", 
     yaxs="i"
)

#x and y limits with plain axes without ticks/numbers to match previous graph

ticksx <- c(0, 20, xlims[2])
xlabels <- c(NA, expression(paste(Delta[m] == bar(Delta))), NA)
ticksy <- c(0, avgwealth(20), ylims[2])
ylabels <- c(NA, expression(paste(hat(y)[m])), NA)



axis(1,at = ticksx,  pos = 0, labels = xlabels, cex.axis = axislabelsize)
axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1, cex.axis = axislabelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)


# lines(xx1, indiffA1(xx1, uA= 8.3), col = COLB[4], lwd = graphlinewidth)
# lines(xx1, indiffA1(xx1, uA = 16), col = COLB[4], lwd = graphlinewidth)
# lines(xx1, indiffA1(xx1, uA = 23.7), col = COLB[4], lwd = graphlinewidth)
lines(xx1, avgwealth(xx1), col = COLA[4], lwd = graphlinewidth)


#Axis labels and draw linear utility function
mtext(expression(paste("Risk, ", Delta)), side = 1, line = 2.5, cex = axislabelsize)
text(-5, 0.5*ylims[2], expression(paste("Expected income, ", hat(y))), xpd = TRUE, cex = axislabelsize, srt = 90) 


text(29-1.5, avgwealth(29) - 1, expression(paste(hat(y)(Delta))), xpd = TRUE, cex = labelsize)

#Label various points on line

segments(0, avgwealth(x = 20), xlims[2], avgwealth(x = 20), lty = 1, col = COLB[4], lwd = graphlinewidth)
segments(0, avgwealth(x = 20) - 5, xlims[2], avgwealth(x = 20) - 5, lty = 1, col = COLB[4], lwd = graphlinewidth)
segments(0, avgwealth(x = 20) + 5, xlims[2], avgwealth(x = 20) + 5, lty = 1, col = COLB[4], lwd = graphlinewidth)

#points(25, avgwealth(25), pch = 16, col = "black", cex = 1.5)
# 
# text(23.63, avgwealth(23.63)+1.1, expression(paste("c")), cex = labelsize)
# points(23.63, avgwealth(23.63), pch = 16, col = "black", cex = 1.5)
# 
text(20, avgwealth(20) + 1.5, expression(paste(m)), cex = labelsize)
segments(20, 0, 20, avgwealth(20), lty = 2, col = "gray", lwd = segmentlinewidth)
points(20, avgwealth(20), pch = 16, col = "black", cex = 1.5)
# 

#label the three indifference curves
text(29, avgwealth(20) + 6.5, expression(paste(u[3])), xpd = TRUE, cex = labelsize)
text(29, avgwealth(20) + 1.5, expression(paste(u[2])),  xpd = TRUE, cex = labelsize)
text(29, avgwealth(20) - 3.5, expression(paste(u[1])),  xpd = TRUE, cex = labelsize)

#text(17, 38.5, expression(paste("Inequality Averse")), xpd = TRUE, cex = labelsize)
# text(20, 32, expression(paste("Risk-neutral")), xpd = TRUE, cex = labelsize)
# text(20, 30.5, expression(paste("indifference curves")), xpd = TRUE, cex = labelsize)



dev.off()