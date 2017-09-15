#Graph Designer: Simon Halliday & Riley Boeth '17
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(pBrackets)
library(ineq)
jpeg(file = "risk/halliday_inequality.jpg", width = 1024, height = 768)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 6, 4, 6))


#Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 1)
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

#Calculate the Gini
#Area B:
#((0.5*(0.25*0.5)) + 0.25*0.5 + 0.5*(0.75*0.5))
#Therefore Area A: 
#0.5 - Area B
#Therefore Gini: 
# (0.5 - ((0.5*(0.25*0.5)) + 0.25*0.5 + 0.5*(0.75*0.5)))/0.5 = 0.25

#ticksx <- seq(from = 0, to = xlims[2], by = 1)
#xlabels <- seq(from = 0, to = xlims[2], by = 1)
ticksx <- seq(from = 0, to = ylims[2], by = 0.1)
xlabels <- seq(from = 0, to = ylims[2], by = 0.1)
ticksy <- seq(from = 0, to = ylims[2], by = 0.25)
ylabels <- seq(from = 0, to = ylims[2], by = 0.25)

axis(1,at = ticksx,  pos = 0, labels = xlabels)
axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Axis labels and draw linear utility function
#mtext(expression(paste("Cumulative population proportion, ", F(n))), side = 1, line = 2.5, cex = axislabelsize)
text(-0.1, 0.5*ylims[2], expression(paste("Cumulative income, ", F(y))), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(0.5*xlims[2], -0.1, expression(paste("Cumulative population proportion, ", F(n))), xpd = TRUE, cex = axislabelsize) 



incomes <- c(0, 1, 1, 2, 2, 8, 8, 14, 15, 19, 30)
hh <- seq(0, 10, by = 1)
hhperc <- hh/10
incprop <- incomes/100 
inccum <- cumsum(incprop)
incomes2 <- c(1, 1, 2, 2, 8, 8, 14, 15, 19, 30)
incprop2 <- incomes2/100
inccum2 <- cumsum(incprop2)
equality2 <- seq(0.1, 1, 0.1)
areaA2 <- equality2 - inccum2
sum(areaA2)/5
hh2 <- seq(1, 10, by = 1)
round(Gini(incomes2), 2)

#Shaded Areas A and B
#Area A
xpoly2 <- c(0, 1, 1, 0)
ypoly2 <- c(0, 0, 1, 0)
polygon(x = xpoly2,
        y = ypoly2,
        col = COLA[1], density = NULL, border = NA)

#Area B
polygon(x = hhperc,
        y = inccum,
        col = COLB[1], density = NULL, border = NA)


# Lorenz curve
lines(x = hhperc, y = inccum,  lty = 1, col = COLA[5], lwd = graphlinewidth)
# segments(0, 0, 0.5, 0.25, lty = 1, col = COLA[5], lwd = graphlinewidth)
# segments(0.5, 0.25, 1, 1, lty = 1, col = COLA[5], lwd = graphlinewidth)
text(0.78, 0.6, expression(paste(Lorenz)), cex = labelsize)
text(0.78, 0.56, expression(paste(curve)), cex = labelsize)

#Line of equality
segments(0, 0, 1, 1, lty = 1, col = COLB[5], lwd = graphlinewidth)
text(0.8, 0.9, expression(paste("Line of")), cex = labelsize)
text(0.8, 0.86, expression(paste(equality)), cex = labelsize)

#Label areas A and B
text(0.5, 0.375, expression(paste(A)), cex = labelsize)
text(0.75, 0.125, expression(paste(B)), cex = labelsize)

#Label and provide Gini value
#Bottom of frame
segments(0.08, 0.75, 0.32, 0.75, lty = 1, col = "black" , lwd = 1)
#Top of frame
segments(0.08, 0.85, 0.32, 0.85, lty = 1, col = "black" , lwd = 1)
#Left of frame
segments(0.08, 0.75, 0.08, 0.85, lty = 1, col = "black" , lwd = 1)
#Right of frame
segments(0.32, 0.75, 0.32, 0.85, lty = 1, col = "black" , lwd = 1)
#Gini equation
text(0.18, 0.8, expression(paste(Gini == frac(A, A + B), phantom() == phantom())), cex = labelsize)
text(0.28, 0.8, paste(round(Gini(incomes2), 2)),  cex = labelsize)


# brackets(x1 = 1.01, y1 = 0.248, x2 = 1.01, y2 = 0,  
#          ticks = 0.5, curvature = 0.5, type = 1, 
#          col = "black", lwd = 2, lty = 1, xpd = TRUE)
# text(1.11, 0.125, expression(paste("Cumulative income")), xpd = TRUE, srt = 270)
# text(1.08, 0.125, expression(paste("of the poor")), xpd = TRUE, srt = 270)
# 
# brackets(x1 = 1.01, y1 = 1, x2 = 1.01, y2 = 0.252,  
#          ticks = 0.5, curvature = 0.5, type = 1, 
#          col = "black", lwd = 2, lty = 1, xpd = TRUE)
# text(1.11, 0.625, expression(paste("Cumulative income")), xpd = TRUE, srt = 270)
# text(1.08, 0.625, expression(paste("of the rich")), xpd = TRUE, srt = 270)


dev.off()

