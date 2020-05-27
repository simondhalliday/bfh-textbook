require(shape)
require(plotrix)
require(pBrackets)
pdf(file = "employment/employment_macro_inequality_wage.pdf", width = 9, height = 7)

#Set parameters for graphics
pointsize <- 1.8
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
grays <- gray.colors(25, start = 1, end = 0)

WageFn <- function(H, delta = 5) {
  delta / (1 - H)
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
     xaxs = "i", 
     yaxs = "i")

text(-0.1, 0.5*ylims[2], expression(paste("Wage, ", w)), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(0.5*xlims[2], -11, expression(paste("Number of workers, ", n)), xpd = TRUE, cex = axislabelsize) 


npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], 0.9, length.out = npts)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
xx4 <- seq(xlims[1], 25, length.out = npts2)

#Draw the lines for the graphs
lines(xx1, WageFn(xx1), col = COL[1], lwd = graphlinewidth)
#lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 4)
#lines(xx2, solowInfeas(xx2, delta = 5), col = COL[1], lwd = 4, lty = 2)

#Customize ticks and labels for the plot
ticksy <- c(0, WageFn(0.8),  40)
ylabels <- c(0, expression(paste(w[0])), NA)
ticksx <- c(0, 0.8, 0.9, xlims[2])
xlabels <- c(0, 80, 90, NA)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#Annotation of the  graphs
text(0.7, 35, expression(paste("Wage Curve ", w^N*(H))), cex = labelsize)

#segments(1, 0, 1, 42, lty = 2, lwd = 3, col = "darkgray")
segments(0.8, 0, 0.8, WageFn(0.8), lty = 2, lwd = segmentlinewidth, col = grays[20])

#Arrows(0.85, 6, 0.85, WageFn(0.8) - 1, col = "black", lty = 1, code = 3, lwd = 2, arr.type = "triangle")
#text(0.99,  5 + 0.5*(WageFn(0.8) - 5), expression(paste("Employment Rent")))

#Zero profit condition 
segments(0, WageFn(0.8), 0.8, WageFn(0.8), lty = 1, lwd = graphlinewidth, col = COLB[3])
segments(0.8, WageFn(0.8), 1.2, WageFn(0.8), lty = 2, lwd = segmentlinewidth, col = COLB[3])

points(0.8, WageFn(0.8), pch = 16, col = "black", cex = 1.5)
text(0.79, WageFn(0.8) + 1, expression(paste("n")), cex = labelsize)


brackets(x1 = 0.795, y1 = -3.5, x2 = 0, y2 = -3.5,  
         ticks = 0.5, curvature = 0.5, type = 1, h = 1,  
         col = "black", lwd = 2, lty = 1, xpd = TRUE)
text(0.4, -5.5, expression(paste("employed")), xpd = TRUE, cex = labelsize)

brackets(x1 = 0.895, y1 = -3.5, x2 = 0.805, y2 = -3.5,  
         ticks = 0.5, curvature = 0.5, type = 1, h = 1,
         col = "black", lwd = 2, lty = 1, xpd = TRUE)
text(0.85, -5.5, expression(paste("unemployed")), xpd = TRUE, cex = labelsize)


#Zero profit condition
text(0.99, WageFn(0.8) + 5.6, expression(paste("Competition")), cex = labelsize, xpd = TRUE)
text(0.99, WageFn(0.8) + 3.6, expression(paste("condition determines")), cex = labelsize, xpd = TRUE)
text(0.98, WageFn(0.8) + 1.3, expression(paste("the wage, ", w^C == w[0])), cex = labelsize, xpd = TRUE)


dev.off()

