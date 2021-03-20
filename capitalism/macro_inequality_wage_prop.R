require(shape)
require(plotrix)
require(pBrackets)
pdf(file = "capitalism/macro_inequality_wage_prop.pdf", width = 9, height = 7)

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
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")

WageFn <- function(h, ubar = 1.7, B = 1, t = 0.8) {
  B + ubar + (ubar - ubar*t +  ubar*t*h )/(t*(1 - h))
}

#COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")
par(mar =  c(8, 5, 1, 6))
xlims <- c(0, 1)
ylims <- c(0, 45)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")),
     xaxt = "n", 
     yaxt = "n",
     cex.lab = axislabelsize,
     bty = "n", 
     xaxs = "i", 
     yaxs = "i")

text(-0.11, 0.5*ylims[2], expression(paste("Wage, ", w)), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(0.5*xlims[2], -10, expression(paste("Number of workers, ", H)), xpd = TRUE, cex = axislabelsize) 


npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], 1, length.out = npts)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
xx4 <- seq(xlims[1], 25, length.out = npts2)

#Draw the lines for the graphs
lines(xx1, WageFn(xx1), col = COL[1], lwd = graphlinewidth)
#lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 4)
#lines(xx2, solowInfeas(xx2, delta = 5), col = COL[1], lwd = 4, lty = 2)

#Customize ticks and labels for the plot
ticksy <- c(0, 20, 40, ylims[2])
ylabels <- c(0, expression(paste(w^c)), expression(paste(gamma)), NA)
ticksx <- c(0, 0.8/0.9, 1, xlims[2])
xlabels <- c(0, 80, 90, NA)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#Annotation of the  graphs
text(0.77, 35, expression(paste("Wage curve, ", w^N*(H))), cex = labelsize)

segments(1, 0, 1, ylims[2], lty = 2, lwd = segmentlinewidth, col = grays[20], xpd = TRUE)
segments(0.8/0.9, 0, 0.8/0.9, WageFn(0.8/0.9), lty = 2, lwd = segmentlinewidth, col = grays[20])

#Competition condition 
segments(0, 20, 0.8/0.9, 20, lty = 1, lwd = graphlinewidth, col = COLB[3])
segments(0.8, 20, xlims[2], 20, lty = 2, lwd = segmentlinewidth, col = COLB[3])

#Gamma segment 
segments(0, 40, xlims[2], 40, lty = 2, lwd = segmentlinewidth, col = CBCols[3], xpd = TRUE)

#Point n
points(0.8/0.9, WageFn(0.8/0.9), pch = 16, col = "black", cex = 1.5)
text(0.8/0.9 - 0.015, WageFn(0.8/0.9) + 1.25, expression(paste(n)), cex = labelsize)

#Braces and labels for unemployed and employed
brackets(x1 = 0.8/0.9 - 0.005, y1 = -3.5, x2 = 0, y2 = -3.5,  
         ticks = 0.5, curvature = 0.5, type = 1, h = 1,  
         col = "black", lwd = 2, lty = 1, xpd = TRUE)
text(0.5*((0.8/0.9)), -5.7, expression(paste("employed, n")), xpd = TRUE, cex = labelsize)

brackets(x1 = 1 - 0.005, y1 = -3.5, x2 = 0.8/0.9 + 0.005, y2 = -3.5,  
         ticks = 0.5, curvature = 0.5, type = 1, h = 1,
         col = "black", lwd = 2, lty = 1, xpd = TRUE)
text(0.8/0.9 + 0.5*(0.1/0.9), -5.7, expression(paste("unemployed, u")), xpd = TRUE, cex = labelsize)


#Competition condition label
text(0.2, 20 + 5.6, expression(paste("Competition")), cex = labelsize, xpd = TRUE)
text(0.2, 20 + 3.6, expression(paste("condition determines")), cex = labelsize, xpd = TRUE)
text(0.2, 20 + 1.3, expression(paste("the wage, ", w^c)), cex = labelsize, xpd = TRUE)

#Supply of labor label
text(1.07, 39-34.5, expression(paste("Supply")), cex = labelsize, xpd = TRUE)
text(1.07, 37.4-35, expression(paste("of labor")), cex = labelsize, xpd = TRUE)

#Label gamma output per worker hour
text(0.2, 43, expression(paste("Output per")), cex = labelsize, xpd = TRUE)
text(0.2, 41, expression(paste("worker hour,", gamma)), cex = labelsize, xpd = TRUE)


dev.off()

