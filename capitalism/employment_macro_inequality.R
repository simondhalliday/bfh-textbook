require(shape)
require(plotrix)
require(pBrackets)
pdf(file = "capitalism/employment_macro_inequality.pdf", width = 9, height = 7)

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
par(mar =  c(8, 5, 4, 2))
xlims <- c(0, 1.1)
ylims <- c(0, 40)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")),
     xaxt = "n", 
     yaxt = "n",
     cex.lab = axislabelsize, 
     line = 2.5,
     bty = "n", 
     xaxs="i", 
     yaxs="i")

text(-0.1, 0.5*ylims[2], expression(paste("Wage, ", w)), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(0.5*xlims[2], -8, expression(paste("Number of workers, ", n)), xpd = TRUE, cex = axislabelsize) 


npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], 0.9, length.out = npts)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
xx4 <- seq(xlims[1], 25, length.out = npts2)

#Draw the lines for the graphs
lines(xx1, WageFn(xx1), col = COL[1], lwd = 4)
#lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 4)
#lines(xx2, solowInfeas(xx2, delta = 5), col = COL[1], lwd = 4, lty = 2)

#Customize ticks and labels for the plot
ticksy <- c(0, WageFn(0.8),  40)
ylabels <- c(0, expression(paste(w[0])), NA)
ticksx <- c(0, 0.8, 0.9, xlims[2])
xlabels <- c(0, 80, 90, NA)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Annotation of the  graphs
text(0.74, 35, expression(paste("Wage Curve ", w^N*(H))))

#segments(1, 0, 1, 42, lty = 2, lwd = 3, col = "darkgray")
segments(0.8, 0, 0.8, WageFn(0.8), lty = 2, lwd = 2, col = "darkgray")

#Arrows(0.85, 6, 0.85, WageFn(0.8) - 1, col = "black", lty = 1, code = 3, lwd = 2, arr.type = "triangle")
#text(0.99,  5 + 0.5*(WageFn(0.8) - 5), expression(paste("Employment Rent")))

#Zero profit condition 
segments(0, WageFn(0.8), 0.8, WageFn(0.8), lty = 1, lwd = graphlinewidth, col = COLB[3])
segments(0.8, WageFn(0.8), 1.2, WageFn(0.8), lty = 2, lwd = segmentlinewidth, col = COLB[3])

points(0.8, WageFn(0.8), pch = 16, col = "black", cex = 1.5)
text(0.79, WageFn(0.8) + 1, expression(paste("n")))


brackets(x1 = 0.795, y1 = -3.5, x2 = 0, y2 = -3.5,  
         ticks = 0.5, curvature = 0.5, type = 1, h = 1,  
         col = "black", lwd = 2, lty = 1, xpd = TRUE)
text(0.4, -5.5, expression(paste("employed")), xpd = TRUE)

brackets(x1 = 0.895, y1 = -3.5, x2 = 0.805, y2 = -3.5,  
         ticks = 0.5, curvature = 0.5, type = 1, h = 1,
         col = "black", lwd = 2, lty = 1, xpd = TRUE)
text(0.85, -5.5, expression(paste("unemployed")), xpd = TRUE)

#Unemployment benefits & a
#segments(0, 5, 1.2, 5, lty = 2, lwd = 2, col = "darkgray")
#segments(0, 2.5, 1.2, 2.5, lty = 2, lwd = 2, col = "darkgray")

#Zero profit condition
text(0.98, WageFn(0.8) + 1, expression(paste("Profit curve, ", w == w[0])))
#text(0.97, 6, expression(paste(B + a/t)))
#text(0.97, 3.5, expression(paste(B, " (unemployment benefits)")))
#text(1.08, 36, expression(paste("level of")))
#text(1.08, 34, expression(paste("employment, ", bar(H))))


dev.off()

