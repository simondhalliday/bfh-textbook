#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "specprodexch/ppf_eos_fish.pdf", width = 9, height = 9)

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
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")
grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00")


#Edited the margins to cater for the larger LHS labels
par(mar =  c(1.5, 3, 1, 0.5))


fishProd <- function(l, k = 0.5){
  (k)*l
}

xlims <- c(0, 10)
ylims <- c(0, 11)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(13, 15, 17)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n"
)

ticksy <- c(0, 5, 10, ylims[2])
ylabels <- c(NA, 5, 10, NA)
ticksx <- c(0, 2.5, 5, xlims[2])
xlabels <- c(NA, 2.5, 5, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis =annotatesize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = annotatesize)

npts <- 500 
xx1 <- seq(0, 5, length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)


#Draw the graphs
lines(xx2, fishProd(xx2, k = 2), col = COLB[3], lwd = graphlinewidth)

#lines(xx4, manufactureProd(xx4, k = 0.1, alpha = 2), col = COLB[4], lwd = graphlinewidth)

#mtext(expression(paste("Quantity of fish, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-0.8, 8, expression(paste("Labor for fish, ", l^f)), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(8, -0.5, expression(paste("Fish, ", x)), xpd = TRUE, cex = axislabelsize)

#label the points on the axes we want
# text(-0.6, 2.8, expression(paste(2.5)), xpd = TRUE, cex = annotatesize)
# text(-0.5, 10.5, expression(paste(10)), xpd = TRUE, cex = annotatesize)
# text(2.9, -0.3, expression(paste(2.5)), xpd = TRUE, cex = annotatesize)
# text(5.2, -0.3, expression(paste(5)), xpd = TRUE, cex = annotatesize)
# text(-5.3, -0.3, expression(paste(5)), xpd = TRUE, cex = annotatesize)
# text(-0.3, -5.3, expression(paste(5)), xpd = TRUE, cex = annotatesize)
# text(-10.4, -0.3, expression(paste(10)), xpd = TRUE, cex = annotatesize)
# text(-0.5, -10.3, expression(paste(10)), xpd = TRUE, cex = annotatesize)

#Label the two production functions
#Clothing

#Fishing
text(6.5, 5.3, expression(paste("Fish production")), xpd = TRUE, cex = annotatesize)
text(6.5, 6, expression(paste(x == frac(1,2)(l^f))), xpd = TRUE, cex = annotatesize)
Arrows(5.8, 6, 3.5, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Draw segments for the 50/50 split of time
segments(0, 5, 2.5, 5, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(2.5, 0, 2.5, 5, lty = 2, col = grays[20], lwd = segmentlinewidth)

#Annotate Max time on fishing
segments(0, 10, 5, 10, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(5, 10, 5, 0, lty = 2, col = grays[20], lwd = segmentlinewidth)
text(7.8, 9.9, expression(paste("10 hrs of labor")), cex = annotatesize)
text(7.8, 9.5, expression(paste("for fishing produces")),cex = annotatesize)
text(7.8, 9.1, expression(paste("5 kgs of fish")), cex = annotatesize)
points(5,10, pch = 16, col = "black", cex = 1.5)
text(4.7, 10.3, expression(paste("c")), cex = annotatesize)

points(2.5, 5, pch = 16, col = "black", cex = 1.5)
text(2.3, 5.3, expression(paste("f")), cex = annotatesize)




dev.off()
