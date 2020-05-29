# TO DO

# MAKE BRACKET NORMAL SIZED

require(ggplot2)
require(shape)
require(plotrix)
library(pBrackets)
pdf(file = "employment/real_wage_profit.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

WageFn <- function(H, delta = 5) {
  delta /(1 - H)
}

#COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")
par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 1.5)
ylims <- c(0, 40)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("Total Hours of Employment, ", H)),
     ylab = expression(paste("Real wage, w, and labor productivity")),
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
ylabels <- c(0, expression(paste(w^c)),  expression(paste(gamma[0])), NA)
ticksx <- c(0, 1.305, xlims[2])
xlabels <- c(0, 1.0, NA)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#Annotation of the  graphs
text(1, 19, expression(paste("Competition Condition")), cex = labelsize)


text(1.05, 31, expression(paste("Output per worker, ", gamma)), cex = labelsize)
#Line for the absolute maximum quality
#segments(1, 0, 1, 42, lty = 2, lwd = 3, col = "darkgray")
#segments(0.75, 0, 0.75, 20, lty = 2, lwd = 2, col = "darkgray")

#Arrow to Slope of BRF
#Arrows(0.2, 19, 0.2, 10, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(0.325, 18, expression(paste("If the ", w<w^c, " new firms will"  )), cex = labelsize)
text(0.325, 16, expression(paste("be able to make economic")), cex = labelsize)
text(0.325, 14, expression(paste("profits and the number of")), cex = labelsize)
text(0.325, 12, expression(paste("firms will grow")), cex = labelsize)
Arrows(0.63, 15, 0.9, 15, col = "black", lty = 1, lwd = 2, arr.type = "triangle")



# Arrows(0.6, 19, 0.6, 10, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(0.42, 28, expression(paste("If the ", w>w^c,  " profits will")), cex = labelsize)
text(0.42, 26, expression(paste("be insufficient and firms")), cex = labelsize)
text(0.42, 24, expression(paste("will leave")), cex = labelsize)
Arrows(0.15, 26, 0.03, 26, col = "black", lty = 1, lwd = 2, arr.type = "triangle")


#Arrows(0.8, 15, 0.8, 19, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#Arrows(0.8, 15, 0.8, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#text(0.92, 12.5, expression(paste("Employment Rent")))

#Text to indicate delta = 5
#text(0.2, 38, expression(paste("Wage Function")))
#text(0.2, 36, expression(paste("set to ", delta, " = 5")))


#Zero profit condition 
segments(0, 20, xlims[2] - 0.2, 20, lty = 1, lwd = 2, col = COLB[2])
segments(0, 30, xlims[2] - 0.2, 30, lty = 2, lwd = 2, col = COLA[2])
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
brackets(x1 = 1.31, y1 = 29.9, x2 = 1.31, y2 = 0.1,  ticks = 0.5, curvature = 0.5, type = 1, 
         col = "black", lwd = 1, lty = 1, xpd = TRUE, h = 0.1)
brackets(x1 = 1.3, y1 = 20.1, x2 = 1.3, y2 = 29.9,  ticks = 0.5, curvature = 0.5, type = 1, 
         col = "black", lwd = 1, lty = 1, xpd = TRUE, h = 0.1)
brackets(x1 = 1.3, y1 = 0.1, x2 = 1.3, y2 = 20.1,  ticks = 0.5, curvature = 0.5, type = 1, 
         col = "black", lwd = 1, lty = 1, xpd = TRUE, h = 0.1)

text(1.47, 17, expression(paste("Output")), cex = labelsize, xpd = TRUE)
text(1.47, 15, expression(paste("per")), cex = labelsize, xpd = TRUE)
text(1.47, 13, expression(paste("worker")), cex = labelsize, xpd = TRUE)
text(1.07, 10, expression(paste("Real wage")), cex = labelsize)
text(0.95, 25, expression(paste("Proft per worker hour")), cex = labelsize)


dev.off()