require(ggplot2)
require(shape)
require(plotrix)
pdf(file = "employment/real_wage_profit.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 3

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

WageFn <- function(H, delta = 5) {
  delta /(1 - H)
}

#COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")
par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 1.2)
ylims <- c(0, 40)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("Total Hours of Employment, ", H)),
     ylab = expression(paste("Real Wage, ", w/p, ", and profit, ", pi)),
     xaxt = "n", 
     yaxt = "n",
     cex.lab = axislabelsize, 
     line = 2.5,
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
#lines(xx1, WageFn(xx1), col = COL[1], lwd = 4)
#lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 4)
#lines(xx2, solowInfeas(xx2, delta = 5), col = COL[1], lwd = 4, lty = 2)

#Customize ticks and labels for the plot
ticksy <- c(0, 20, 30, 40)
ylabels <- c(0, expression(paste(frac(w[0], p))),  expression(paste(q[0])), NA)
ticksx <- c(0, 1, xlims[2])
xlabels <- c(0, 1.0, NA)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Annotation of the  graphs
text(1.05, 22, expression(paste("Real Wage, ", frac(w,p) == pi, ", profit")))


text(1.05, 31, expression(paste("Output per worker, ", q)))
#Line for the absolute maximum quality
#segments(1, 0, 1, 42, lty = 2, lwd = 3, col = "darkgray")
#segments(0.75, 0, 0.75, 20, lty = 2, lwd = 2, col = "darkgray")

#Arrow to Slope of BRF
#Arrows(0.2, 19, 0.2, 10, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(0.3, 18, expression(paste("If the real wage is below the profit")))
text(0.3, 16, expression(paste("curve, the strong incentive to invest ")))
text(0.3, 14, expression(paste("leads to a rise in capital stock")))
text(0.3, 12, expression(paste("and an increase in employment")))
Arrows(0.55, 15, 0.8, 15, col = "black", lty = 1, lwd = 2, arr.type = "triangle")

# Arrows(0.6, 19, 0.6, 10, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(0.65, 28, expression(paste("If the real wage is above the profit")))
text(0.65, 26, expression(paste("curve, the weak incentive to invest")))
text(0.65, 24, expression(paste("leads to a fall in capital stock and")))
text(0.65, 22, expression(paste("there are fewer jobs")))
Arrows(0.4, 25, 0.15, 25, col = "black", lty = 1, lwd = 2, arr.type = "triangle")

#Arrows(0.8, 15, 0.8, 19, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#Arrows(0.8, 15, 0.8, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#text(0.92, 12.5, expression(paste("Employment Rent")))

#Text to indicate delta = 5
#text(0.2, 38, expression(paste("Wage Function")))
#text(0.2, 36, expression(paste("set to ", delta, " = 5")))


#Zero profit condition 
segments(0, 20, xlims[2], 20, lty = 1, lwd = 2, col = COLB[2])
segments(0, 30, xlims[2], 30, lty = 2, lwd = 2, col = COLA[2])
# segments(0.75, 20, 1.2, 20, lty = 2, lwd = 2, col = "darkgray")

#Unemployment benefits & a
# segments(0, 5, 1.2, 5, lty = 2, lwd = 2, col = "darkgray")
# segments(0, 2.5, 1.2, 2.5, lty = 2, lwd = 2, col = "darkgray")

#Zero profit condition
# text(1.02, 21, expression(paste("Zero profit condition, ", w == w[0])))
# text(0.97, 6, expression(paste(b + a)))
# text(0.97, 3.5, expression(paste(b, " (unemployment benefits)")))
#text(1.08, 36, expression(paste("level of")))
#text(1.08, 34, expression(paste("employment, ", bar(H))))


dev.off()