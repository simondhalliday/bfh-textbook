#Graph Designer: Simon Halliday & Riley Boeth '17
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "risk/risks_inequality_neutral.pdf", width = 10, height = 8)


#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 10, 4, 4))

#Indifference curves of a risk-averse homo economicus (2nd graph out of the two for 4.7)

indiffA <- function(x, ua = 2, slope = 1/12) {
  ua + slope*(x)^2
}

#Average wealth function 

avgwealth <- function(x){
  (1.08)*((-1/35)*(((-((.9)*x-36))^2))+36.8+((-1/10)*(x-4)+(-1/1000)*((x-3)^3)))
}

#Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 40)
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
     xaxs="i", 
     yaxs="i"
)

#x and y limits with plain axes without ticks/numbers to match previous graph

#ticksx <- seq(from = 0, to = xlims[2]+1, by = 4)
#xlabels <- seq(from = 0, to = xlims[2], by = 1)
ticksx <- c(xlims[1], 12,  20, 25, xlims[2])
xlabels <- c(NA, expression(paste(Delta[Rawls])), expression(paste(Delta[max])), expression(paste(Delta, ""[Wealthiest], " "[Rich])), NA)
#ticksy <- seq(from = 0, to = ylims[2]+1, by = 4)
#ylabels <- seq(from = 0, to = ylims[2], by = 1)
ticksy <- c(ylims[1], avgwealth(11), avgwealth(20), 28, ylims[2])
ylabels <- c(NA, expression(paste(omega[1])), expression(paste(omega[2] == omega[max])), expression(paste(omega[3])), NA)
#text(-1, avgwealth(8)+7, expression(paste(omega, ""[R],"'")), xpd = TRUE, cex = labelsize)
#text(-1, avgwealth(8), expression(paste(omega, ""[e],"'")),  xpd = TRUE, cex = labelsize)
#text(-1, avgwealth(8)-7, expression(paste(omega, ""[P],"'")),  xpd = TRUE, cex = labelsize)


#text(8, -.9, expression(paste(Delta, ""[e],"`")),  xpd = TRUE,  cex = labelsize)
#text(12, -.9, expression(paste(Delta, ""[rawls])), xpd = TRUE, cex = labelsize)
#text(15, -.9, expression(paste(Delta, ""[e])),  xpd = TRUE,  cex = labelsize)
#text(20, -.9, expression(paste(Delta,""[max])),  xpd = TRUE,  cex = labelsize)
#text(25, -.9, expression(paste(Delta, ""[Wealthiest], " "[Rich])),  xpd = TRUE,  cex = labelsize)




axis(1,at = ticksx,  pos = 0, labels = xlabels)
axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)


# lines(xx1, indiffA(xx1, ua = 2), col = COLB[4], lwd = graphlinewidth)
# lines(xx1, indiffA(xx1, ua = 8.2), col = COLB[4], lwd = graphlinewidth)
# lines(xx1, indiffA(xx1, ua = 14.4), col = COLB[4], lwd = graphlinewidth)
lines(xx1, avgwealth(xx1), col = COLA[4], lwd = graphlinewidth)


#Axis labels and draw linear utility function
mtext(expression(paste("Inequality of Wealth, ", Delta)), side = 1, line = 2.5, cex = axislabelsize)
text(-6, 0.5*ylims[2], expression(paste("Average Wealth, ",omega)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label various points on line

segments(0, avgwealth(x = 11), xlims[2], avgwealth(x = 11), lty = 1, col = COLB[4], lwd = graphlinewidth)
segments(0, avgwealth(x = 20) , xlims[2], avgwealth(x = 20) , lty = 1, col = COLB[4], lwd = graphlinewidth)
segments(0, 28, xlims[2], 28, lty = 1, col = COLB[4], lwd = graphlinewidth)



# text(8, avgwealth(8)+1, expression(paste(e*minute)), cex = labelsize)
# segments(8, 0, 8, avgwealth(8), lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, avgwealth(8), 8, avgwealth(8), lty = 2, col = "gray", lwd = segmentlinewidth)
# points(8, avgwealth(8), pch = 16, col = "black", cex = 1.5)
# 
# segments(25, 0, 25, avgwealth(25), lty = 2, col = "gray", lwd = segmentlinewidth)
# points(25, avgwealth(25), pch = 16, col = "black", cex = 1.5)
# 
text(20, avgwealth(20)+1, expression(paste(m)), cex = labelsize)
segments(20, 0, 20, avgwealth(20), lty = 2, col = "gray", lwd = segmentlinewidth)
points(20, avgwealth(20), pch = 16, col = "black", cex = 1.5)

segments(0, avgwealth(12), 12, avgwealth(12), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(12, 0, 12, avgwealth(12), lty = 2, col = "gray", lwd = segmentlinewidth)
points(12, avgwealth(12), pch = 16, col = "black", cex = 1.5)
text(12, avgwealth(12) + 1, expression(paste(R)), cex = labelsize)

segments(0, avgwealth(25), 25, avgwealth(25), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(25, 0, 25, avgwealth(25), lty = 2, col = "gray", lwd = segmentlinewidth)
points(25, avgwealth(25), pch = 16, col = "black", cex = 1.5)
text(25, avgwealth(25) + 1, expression(paste(W)), cex = labelsize)


# text(14.7, avgwealth(15)+1, expression(paste("e")), cex = labelsize)
# segments(15, 0, 15, avgwealth(15), lty = 2, col = "gray", lwd = segmentlinewidth)
# points(15, avgwealth(15), pch = 16, col = "black", cex = 1.5)
# 
# segments(12, 0, 12, avgwealth(12), lty = 2, col = "gray", lwd = segmentlinewidth)
# points(12, avgwealth(12), pch = 16, col = "black", cex = 1.5)

#Label relevant points on axes

#Add arrow beside y axis and points on axis

# arrows(-1, avgwealth(8)+6, -1, avgwealth(8)+1, col = "black", code =3, arr.type = "simple", xpd = TRUE, length = 0.1, angle = 40, lwd = 3)
# arrows(-1, avgwealth(8)-6, -1, avgwealth(8)-1, col = "black", code = 3, arr.type = "simple", xpd = TRUE, length = 0.1, angle = 40, lwd = 3)

#points(0, avgwealth(8)+7, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
#points(0, avgwealth(8), pch = 16, col = "black", cex = 1.5,xpd = TRUE)
#points(0, avgwealth(8)-7, pch = 16, col = "black", cex = 1.5,xpd = TRUE)

#label the three indifference curves

text(38, 18.2, expression(paste(u[1])), xpd = TRUE, cex = labelsize)
text(38, 23.5, expression(paste(u[2])),  xpd = TRUE, cex = labelsize)
text(38, 28.8, expression(paste(u[3])),  xpd = TRUE, cex = labelsize)



#Label average wealth curve and indifference curves generally

text(33, avgwealth(30)+1.5, expression(paste("Average Wealth")), xpd = TRUE, cex = labelsize)
text(33, avgwealth(30), expression(paste("Function")), xpd = TRUE, cex = labelsize)
text(33, avgwealth(30)-1.5, expression(paste(omega == g(Delta) )), xpd = TRUE, cex = labelsize)

text(30, 31, expression(paste("Inequality-neutral")), xpd = TRUE, cex = labelsize)
text(30, 29.5, expression(paste("indifference curves")), xpd = TRUE, cex = labelsize)



dev.off()

