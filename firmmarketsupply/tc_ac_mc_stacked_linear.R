#Graph Designer: Scott Cohn
#Authors: Bowles and Halliday
#Title: Microeconomics: Competition, Conflict and Coordination

require(shape)
pdf(file = "firmmarketsupply/tc_ac_mc_stacked_linear.pdf", width = 9, height = 12)

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

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 8, 1, 1), mfrow = c(2, 1))


totalcost <- function(x, c0 = 10, c1 = 2){
  c0 + c1*x
}

avgcost <- function(x, c0 = 10, c1 = 2){
  totalcost/x
}

avgvarcost <- function(x,  c1 = 2){
  c1
}

marginalcost <- function(x, c1 = 2){
  c1
}

mcline <- function(x, constant = 0.3181472, slope = 0.125){
  constant + slope*x
}



# total cost --------------------------------------------------------------


xlims <- c(0, 10)
ylims <- c(0, 8)

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

# Choose x_i

i <- c(2, 2.5, 3, 3.5, 4)
xi <- i[2]

ticksy <- c(0, 2, totalcost(x = xi, c0 = 10, c1 = 2), ylims[2])
ylabels <- c(NA, expression(paste(c[0])), expression(paste(c[i])), NA)
ticksx <- c(0, xi, xlims[2])
xlabels <- c(NA, expression(paste(x[i])), NA)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(xi - 1.5, xi + 1.5, length.out = npts)
xx4 <- seq(4.82456, 7.82456, length.out = npts)

# Total Cost curve
lines(xx1, totalcost(xx1, c0 = 2, c1 = 0.5), col = COLB[3], lwd = graphlinewidth)

# Label the axes
text(-1.25, 0.5*ylims[2], expression(paste("Total cost of production, ", c)), xpd = TRUE, cex = axislabelsize, srt = 90) 

# Segments
segments(xi, -50, xi, totalcost(xi, c0 = 2, c1 = 0.5), lty = 2, col = "grey", lwd = segmentlinewidth, xpd = TRUE)
segments(0, totalcost(xi, c0 = 2, c1 = 0.5), xi, totalcost(xi, c0 = 2, c1 = 0.5), lty = 2, col = "grey", lwd = segmentlinewidth)

# Add point and label
points(xi, totalcost(x = xi, c0 = 2, c1 = 0.5), pch = 16, col = "black", cex = 1.5)
text(xi - 0.15, totalcost(x = xi, c0 = 2, c1 = 0.5) + 0.25, expression("i"), cex = annotatesize)

#Label the cost curve
text(7.5, 6.5, expression("Total cost"), cex = annotatesize)

# Axis Labels
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

# ac & mc -----------------------------------------------------------------

avgcost <- function(x,  c0 = 2, c1 = 0.05){
  (c0 + c1*x)/x
}

marginalcost <- function(x, c1 = 0.05){
  0*x + c1 
}

xlims <- c(0, 10)
ylims <- c(0, 2)

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


ticksy <- c(0, 0.5, ylims[2])
ylabels <- c(NA, expression(paste(c[0])), NA)
ticksx <- c(0, xi, xlims[2])
xlabels <- c(NA, expression(paste(x[i])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(4, 8, length.out = npts)
xx4 <- seq(xlims[1], 6.32456, length.out = npts)
xx5 <- seq(6.32456, xlims[2], length.out = npts)

text(-1.25, 0.5*ylims[2], expression(paste("Cost of production, ", c)), xpd = TRUE, cex = axislabelsize, srt = 90) 
mtext(expression(paste("Quantity, ", x)), side = 1, line = 2.5, cex = axislabelsize)

# segments
segments(xi, 0, xi, ylims[2] + 1, lty = 2, col = "grey", lwd = segmentlinewidth, xpd = TRUE)

# AC
lines(x, avgcost(x, c0 = 2, c1 = 0.5), col = COLA[4], lwd = graphlinewidth)

# AVC = MC
lines(x, marginalcost(x, c1 = 0.5), col = COL[2], lwd = graphlinewidth)

# Add point and label
points(xi, marginalcost(xi, c1 = 0.5), pch = 16, col = "black", cex = 1.5)
text(xi + 0.15, marginalcost(xi, c1 = 0.5) - 0.1, expression("h"), cex = annotatesize)

points(xi, avgcost(xi, c0 = 2, c1 = 0.5), pch = 16, col = "black", cex = 1.5)
text(xi + 0.15, avgcost(xi, c0 = 2, c1 = 0.5) + 0.1, expression("g"), cex = annotatesize)

# Label curves
text(0.8*xlims[2], 0.85, expression("Average costs"), cex = annotatesize)
text(0.8*xlims[2], 0.45, expression(AVC == MC), cex = annotatesize)

dev.off()