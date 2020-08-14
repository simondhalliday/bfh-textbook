#Graph Designer: Simon Halliday, Scott Cohn
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "credit/contractual_risk.pdf", width = 8, height = 6)

#Set parameters for graphics
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
par(mar =  c(5, 7, 0.5, 0.5))

PCFn <- function(delta, q = 0.5) {
  delta/q
}

isoreturnFn <- function(delta, pi=0.125) {
  1 - (pi)/delta
}

xlims <- c(0, 0.64)
ylims <- c(0, 1.05)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(2, 4, 6)

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


ticksy <- c(ylims[1], isoreturnFn(delta= 0.25), ylims[2])
ylabels <- c(NA, expression(paste(f^C, phantom()==frac(1,2))), NA)
ticksx <- c(xlims[1], 0.25, xlims[2])
xlabels <- c(NA, NA, NA)
text(0.25, -0.06, expression(paste(delta^C, phantom()==frac(q, 2))), xpd = TRUE, cex = labelsize) 


axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(0.01, xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(1.5, 2.5, length.out = npts)
xx4 <- seq(3, 5, length.out = npts)
xx5 <- seq(7, 9, length.out = npts)



#Draw the polygon for feasibility
xpoly1 <- c(0.01, xx1, 0.01, 0.01)
ypoly1 <- c(0.01, PCFn(xx1), ylims[2], 0.01)
polygon(x = xpoly1, y = ypoly1, col = COLA[1], density = NULL, border = NA)


#Draw the graphs
lines(xx1, PCFn(xx1), col = COLA[4], lwd = graphlinewidth)
lines(xx1, isoreturnFn(xx1), col = COLB[4], lwd = graphlinewidth)
lines(xx1, isoreturnFn(xx1, pi = 0.075), col = COLB[4], lwd = graphlinewidth)
lines(xx1, isoreturnFn(xx1, pi = 0.175), col = COLB[4], lwd = graphlinewidth)


#Label the feasible frontier
#text(0.14, 0.82, expression("Lower interest factor"), cex = labelsize)
text(0.14, 0.77,  expression("Better for borrower"), cex = labelsize)
Arrows(0.19, 0.72, 0.1, 0.72, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#text(0.14, 0.82, expression("Feasible combinations"), cex = labelsize)
#text(0.14, 0.77,  expression("of risk and interest"), cex = labelsize)
# text(3.2, 0.5, expression("(production possibilities frontier)"), cex = labelsize)
# Arrows(4.35, 0.95, 8.1, 0.95, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Axis labels
#mtext(expression(paste("Interest factor, ", delta)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.09, 0.5*(ylims[2]), expression(paste("Risk, ", f)), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(0.5*(xlims[2]), -0.15, expression(paste("Interest factor, ", delta)), xpd = TRUE, cex = axislabelsize) 


# Arrows(0.38, 0.18, 0.58, 0.09, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# text(0.48, 0.3, expression(paste('More profits')), cex = labelsize, xpd = TRUE)
# text(0.48, 0.23, expression(paste('better for principal')), cex = labelsize, xpd = TRUE)

Arrows(0.36, 0.45, 0.46, 0.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
#text(0.48, 0.3, expression(paste('More profits')), cex = labelsize, xpd = TRUE)
text(0.46, 0.25, expression(paste('Better for lender')), cex = labelsize, xpd = TRUE)


#Label participation constraint
text(0.26, 0.985, expression(paste("Participation constraint, ")), cex = labelsize)
text(0.41, 0.985, expression(paste(f == frac(delta, q))), cex = labelsize, xpd = TRUE)
text(0.26, 0.9, expression(paste("Slope" == frac(1, q))), cex = labelsize)

#Label Iso-profit
text(0.57, 0.96, expression(paste("Iso-expected")), cex = labelsize, xpd = TRUE)
text(0.57, 0.9, expression(paste("profits")), cex = labelsize, xpd = TRUE)
text(0.53, 0.82, expression(paste(hat(pi)[0])), cex = labelsize)
text(0.53, 0.72, expression(paste(hat(pi)[1] == hat(pi)^C)), cex = labelsize)
text(0.53, 0.62, expression(paste(hat(pi)[2])), cex = labelsize)

segments(0, isoreturnFn(0.25), 0.25, isoreturnFn(0.25), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0.25, 0, 0.25, isoreturnFn(0.25), lty = 2, col = grays[20] , lwd = segmentlinewidth)
text(0.25, isoreturnFn(0.25) + 0.035, expression(paste(c)), cex = labelsize)
points(0.25, isoreturnFn(0.25), pch = 16, col = "black", cex = 1.5)

points(0.092, isoreturnFn(0.092, 0.075), pch = 16, col = "black", cex = 1.5)
text(0.092 - 0.006, isoreturnFn(0.092, 0.075) + 0.03, expression(paste(g)), cex = labelsize)

points(0.41, isoreturnFn(0.41, 0.075), pch = 16, col = "black", cex = 1.5)
text(0.41 - 0.006, isoreturnFn(0.41, 0.075) + 0.035, expression(paste(h)), cex = labelsize)

# slope of indiff
text(0.35, 0.1, expression(paste("Slope ", phantom() == frac((1 - f),delta ))), cex = labelsize, xpd = TRUE)
Arrows(0.265, 0.09, 0.21, 0.09, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

dev.off()
