require(shape)
pdf(file = "indmarketdemand/carbon_tax.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(6, 6, 2, 2))

mrsA <- function(x, rmax = 10, xmax = 20) {
  rmax - (rmax/xmax)*x
}


uA <- function(x, y, rmax = 3, xmax = 12) {
  y - rmax*x + (1/2)*(rmax/xmax)*x^2
}

indiffA1 <- function(x, uA = 10, rmax = 2.5, xmax = 10) {
  uA - rmax*x + (1/2)*(rmax/xmax)*(x^2)
}

offerCurve <- function(x, w, rmax = 2, xmax = 12) {
  w - rmax*x + (rmax/xmax)*x^2
}

bcA <- function(x, w, p) {
  w - p*x
}


xlims <- c(0, 13)
ylims <- c(0, 26)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
#a <- c(13, 16.75, 19.25)
a <- c(25.65, 23.5, 15)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = ylims[1], to = ylims[2], by = 1)
# ylabels <- seq(from = ylims[1], to = ylims[2], by = 1)
# ticksx <- seq(from = xlims[1], to = xlims[2], by = 1)
# xlabels <- seq(from = xlims[1], to = xlims[2], by = 1)
ticksy <- c(0, 1.492, 5.784, 10.2, 10.709, 18.9, ylims[2])
ticksy_mod <- c(10, 10.8)
ylabels_mod <- c(expression(paste(m[a] == m[b])), expression(paste(y[3])))
ylabels <- c(NA, expression(paste(y[1])), expression(paste(y[2])), NA, NA, expression(paste(m[c])), NA)
ticksx <- c(0, 6.533, 10.493, xlims[2])
xlabels <- c(NA, expression(paste(x[2] == x[1])), expression(paste(x[3])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy_mod, pos = 0, labels = ylabels_mod, tick = FALSE, las = 1, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, las = 1, labels = ylabels, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, uA(xx1, y = 15.65), col = COLA[3], lwd = graphlinewidth)
lines(xx1, uA(xx1, y = 23.5), col = COLA[3], lwd = graphlinewidth)
lines(xx1, uA(xx1, y = 25), col = COLA[3], lwd = graphlinewidth)

#lines(xx1, bcA(xx1, w = 10, p = 1.5), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 18.9, p = 1.25), col = COLB[5], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 10.2, p = 1.35), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, w = 10.2, p = 0.418), col = COLB[3], lwd = graphlinewidth)

#Label the axes
#mtext(expression(paste("Fossil fuels, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], ylims[1] - 0.15*ylims[2], expression(paste("Amount of fossil fuels, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-1.8, 0.5*ylims[2], expression(paste("Quality of life, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the indifference curves
#text(11.8, 1.3, expression(u[1]), cex = labelsize)


#Label the price lines
#text(8, 1.7, expression(paste(bc[1])))


#Add the contour plot for the indifference curves
#contour(x, y, 
#        outer(x, y, uA),
#        drawlabels = FALSE,
#        col = COLA[3],
#        lwd = graphlinewidth,
#        levels = a, 
#        xaxs="i", 
#        yaxs="i", 
#        add = TRUE)

#Segments for points on Offer curve
segments(0, 10.709, 6.553, 10.709, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(6.533, 0, 6.533, 10.709, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 1.38, 6.553, 1.38, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(10.493, 0, 10.493, 5.784, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 5.784, 10.493, 5.784, lty = 2, col = "gray" , lwd = segmentlinewidth)
#segments(10.5, 0, 10.5, 7.375, lty = 2, col = "gray" , lwd = segmentlinewidth)

#Annotate points on offer curve mrs = p for each of p = 1, 0.5, 0.25
#Where mrs = 2 - (1/6)*x
points(6.533, 10.709, pch = 16, col = "black", cex = 1.5)
points(10.493, 5.784, pch = 16, col = "black", cex = 1.5)
points(6.533, 1.38, pch = 16, col = "black", cex = 1.5)

text(10.493 + .35, 5.784 + .45, expression(paste(a)), cex = labelsize)
text(6.45  + .35, 1.38 + .45, expression(paste(b)), cex = labelsize)
text(6.553 + .35, 10.709 + .45, expression(paste(c)), cex = labelsize)

text(0.6, 15, expression(paste(u[1])), cex = labelsize)
text(0.6, 21, expression(paste(u[2])), cex = labelsize)
text(0.6, 24, expression(paste(u[3])), cex = labelsize)


text(7.75, 0.5, expression(paste(bc[b])), cex = labelsize)
text(12.75, 4.5, expression(paste(bc[a])), cex = labelsize)
text(12.75, 2.5, expression(paste(bc[c])), cex = labelsize)

dev.off()