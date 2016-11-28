#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
require(plotrix)
pdf(file = "credit/credit_labor_macro.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 3

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

WageFn <- function(H, delta = 5) {
  delta /(1 - H)
}

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")
par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 1.2)
ylims <- c(0, 40)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("Total hours of employment as a proportion of labor supply, ", H)),
     ylab = expression(paste("Wage, ", w)),
     xaxt = "n", 
     yaxt = "n",
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i")

npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], 0.9, length.out = npts)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
xx4 <- seq(xlims[1], 25, length.out = npts2)

#Draw the lines for the graphs
lines(xx1, WageFn(xx1), col = COL[1], lwd = 4)

#Customize ticks and labels for the plot
ticksy <- c(0, 5, 20, 25, 30, 40)
ylabels <- c(0, NA, expression(paste(w[0])), expression(paste(w[1])), expression(paste(q)), NA)
ticksx <- c(0, 0.75, 0.8, 1, xlims[2])
xlabels <- c(0, expression(paste(H[0],"*")), expression(paste(H[1],"*")), 1.0, NA)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Annotation of the  graphs
text(0.73, 35, expression(paste("Wage Curve ", w^N*(H))))

#segments(1, 0, 1, 42, lty = 2, lwd = 3, col = "darkgray")
segments(0.75, 0, 0.75, 20, lty = 2, lwd = 2, col = "darkgray")

#Arrows(0.85, 15, 0.85, 24, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#Arrows(0.85, 15, 0.85, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#text(0.98, 14.5, expression(paste("Employment rent")))
#text(0.98, 12.5, expression(paste("increases with union")))

#Initial Zero profit condition 
segments(0, 20, 1.2, 20, lty = 1, lwd = graphlinewidth, col = COLB[3])

#Secondary Zero profit condition 
segments(0, 25, 1.2, 25, lty = 2, lwd = segmentlinewidth, col = COLB[3])

#Labor Productivity
segments(0, 30, 1.2, 30, lty = 2, lwd = segmentlinewidth, col = "gray")
text(1.02, 31, expression(paste("Labor productivity")))


points(0.75, 20, pch = 16, col = "black", cex = 1.5)
text(0.74, 21, expression(paste(n[0])))




Arrows(0.15, 20.5, 0.15, 23.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(0.36, 23.5, expression(paste("Central bank lowers interest rate")))
text(0.36, 21.5, expression(paste("so zero profit condition shifts up")))

segments(0.8, 0, 0.8, 25, lty = 2, lwd = 2, col = "darkgray")
points(0.8, 25, pch = 16, col = "black", cex = 1.5)
text(0.79, 26, expression(paste(n[1])))
text(1.02, 26, expression(paste(zpc[1], ", ", w == w[1])))


#Unemployment benefits & a
#segments(0, 5, 1.2, 5, lty = 2, lwd = 2, col = "darkgray")
#segments(0, 2.5, 1.2, 2.5, lty = 2, lwd = 2, col = "darkgray")

#Zero profit condition
text(1.02, 21, expression(paste(zpc[0], ", ", w == w[0])))
#text(0.97, 6, expression(paste(B + a)))
#text(0.97, 3.5, expression(paste(B, " (unemployment benefits)")))
#text(1.08, 36, expression(paste("level of")))
#text(1.08, 34, expression(paste("employment, ", bar(H))))


dev.off()
