require(shape)
require(pBrackets)

pdf(file = "indmarketdemand/demand_giffen.pdf", width = 7, height = 7)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 4, 1, 1))

demand1 <- function(x, a = 5, b = 2, c = 0.25) {
   (b - (4*a*c + b^2 - 4*c*x)^0.5)/(2*c)
}

demand2 <- function(x, a = 5, b = 2, c = 0.25) {
  (b + (4*a*c + b^2 - 4*c*x)^0.5)/(2*c)
}



xlims <- c(0, 13)
ylims <- c(0, 11)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(13, 16.75, 19.25)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs = "i", 
     yaxs = "i")

ticksy <- c(0, 10, ylims[2])
ylabels <- c(NA, expression(paste(bar(p))), NA)
ticksx <- c(0, 5, xlims[2])
xlabels <- c(NA, expression(paste(bar(x))), NA)


axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 501
xx2 <- seq(xlims[1], 9, length.out = npts)
xx3 <- seq(xlims[1], 9, length.out = npts)

lines(xx2, demand1(xx2), col = COLA[3], lwd = graphlinewidth)
lines(xx3, demand2(xx3), col = COLA[3], lwd = graphlinewidth)

# Label the axes
mtext(expression(paste("Quantity of the good, ", x)), side = 1, line = 2.5, cex = axislabelsize)
text(-1, 0.5*ylims[2], expression(paste("Price per unit, $, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 

# text labels
text(9, 5.9, expression(paste(b)), cex = labelsize)
text(3, 7.5, expression(paste("Giffen demand")), cex = annotatesize)


brackets(9.2, 10, 9.2, 4.1,  h = 1,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = segmentlinewidth, lty = 1, xpd = TRUE)

text(11.4, 7.5, expression(paste("Downward-")), cex = annotatesize, xpd = TRUE)
text(11.4, 7, expression(paste("sloping")), cex = annotatesize, xpd = TRUE)
text(11.4, 6.5, expression(paste("demand")), cex = annotatesize, xpd = TRUE)

brackets(9.2, 3.9, 9.2, 0.1,  h = 1,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = segmentlinewidth, lty = 1, xpd = TRUE)

text(11.4, 2.5, expression(paste("Upward-")), cex = annotatesize)
text(11.4, 2, expression(paste("sloping")), cex = annotatesize)
text(11.4, 1.5, expression(paste("demand")), cex = annotatesize)

dev.off()
