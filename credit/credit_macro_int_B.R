#' Graph Designer: Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics


# CHANGE COLOR AND TRANSPARENCY OF RECT

require(shape)
pdf(file = "credit/credit_macro_int_B.pdf", width = 9, height = 7)

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

par(mar =  c(6, 6, 6, 2))

xlims <- c(0, 45)
ylims <- c(0, 30)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(46.08, 55, 64)
b <- c(46.08, 55, 64)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs = "i", 
     yaxs = "i")

ticksy <- c(0, 5, 10, 15, 20, 25, ylims[2])
ylabels <- c(NA, expression(paste("1 %")), expression(paste("2 %")), expression(paste("3 %")), expression(paste("4 %")), expression(paste("5 %")), NA)
ticksx <- c(0, xlims[2])
xlabels <- c(NA, expression(paste(I[A] + I[B])))


axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

text(-5.1, 0.5*(ylims[2] + ylims[1]), expression(paste("Interest rate, profit rate")), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(0.5*(xlims[2]), -4.5, expression(paste("Aggregate economy: Both firms")), xpd = TRUE, cex = axislabelsize) 


# I_A RECT
rect(0, 0, 5, 29,
     col=COLA[1], border=TRUE, lty=1)

rect(5, 0, 15, 22.5,
     col=COLA[1], border=TRUE, lty=1)

rect(33, 0, 36, 5, # width = 3
     col=COLA[1], border=TRUE, lty=1)

# I_B RECT
rect(15, 0, 20, 20, # width = 5
     col=COLB[1], border=TRUE, lty=1)

rect(20, 0, 30, 15, # width = 10
     col=COLB[1], border=TRUE, lty=1)

rect(30, 0, 33, 10.5, # width = 3
     col=COLB[1], border=TRUE, lty=1)


# Segments
segments(0, 10, xlims[2], 10, col = grays[20], lty = 2, lwd = segmentlinewidth)
segments(0, 25, xlims[2], 25, col = grays[20], lty = 2, lwd = segmentlinewidth)

# Labels
text(32, 17.5, expression(paste("Fall in interest rate")), cex = labelsize)
Arrows(39, 11, 39, 24, col = "black", code = 3, lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
#Arrows(39, 23, 39, 12, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(17.5, -2, expression(paste("Increase in investment")), cex = labelsize, xpd = TRUE)
Arrows(6, -1, 29, -1, col = "black", code = 3, lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5, xpd = TRUE)


Arrows(3, 34, 3, 30, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5, xpd = TRUE)
text(3, 36.5, expression(paste("Firm A's project 1")), cex = labelsize, xpd = TRUE)
text(3, 35, expression(paste("funded at 5% rate")), cex = labelsize, xpd = TRUE)

#Brackets
brackets(x1 = 5.5, y1 = 29, x2 = 35.5, y2 = 29,  ticks = 0.5, curvature = 0.5, type = 1, 
         col = "black", lwd = 2, lty = 1, xpd = TRUE)
text(21, 34, expression(paste("5 other projects funded")), cex = labelsize, xpd = TRUE)
text(21, 32, expression(paste("with a decreased interest rate")), cex = labelsize, xpd = TRUE)


dev.off()