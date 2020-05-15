require(shape)
library(pBrackets)
pdf(file = "employment/employment_mrp_effort.pdf", width = 9, height = 7)

#Set parameters for graphics
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

par(mar =  c(5, 8, 1, 1))

mrpL <- function(l, pmax = 20, s = 0.75) {
  pmax - s*l
}


xlims <- c(0, 20)
ylims <- c(0, 16)

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


ticksy <- c(0, 4, ylims[2])
ylabels <- c(NA, expression(paste(frac(w^N,e^N) == c)),  NA)
ticksx <- c(0, (10 - 4)/0.75, (15 - 4)/0.75, xlims[2])
xlabels <- c(NA, expression(paste(l[1]^{N})), expression(paste(l[2]^{N})), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Lines for mrs graph
lines(xx1, mrpL(xx1, pmax = 10), col = COLA[4], lwd = graphlinewidth)
lines(xx1, mrpL(xx1, pmax = 15), col = COLA[4], lty = 2, lwd = segmentlinewidth)

#Label axes
mtext(expression(paste("Total labor used by the employer, ", l == eh)), side = 1, line = 3, cex = axislabelsize)
text(-3.5, 0.5*ylims[2], expression(paste("Wage per unit of effort, ", c == frac(w,e) )), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Arrows(18, 4, 18, 6.5, col = "black", lty = 1, code = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
#text(16, 5.75, expression(paste("Change in ", mu)), cex = labelsize)


#text(18.3, 7.4, expression(paste(c[2], " (higher B)")), cex = labelsize, xpd =TRUE)
text(19, 5, expression(paste(c == frac(w,e))), cex = labelsize, xpd =TRUE)
text(16, 8.4, expression(paste("Marginal cost")), cex = labelsize, xpd =TRUE)
text(16, 7.7, expression(paste("of labor")), cex = labelsize)
Arrows(16, 7, 16, 4.4, col = "black", lty = 1, code = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

# Arrows(2, 9, 6.8, 9, col = "black", lty = 1, code = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# text(3.5, 10, expression(paste("Increase in ", frac(dy, dl))), cex = labelsize)

#text(9.3, 12, expression(paste(frac(dy, dl)==phantom())), cex = labelsize, xpd =TRUE)
text(1.5, 7.5, expression(paste("Marginal")), cex = labelsize)
text(1.5, 6.8, expression(paste("benefit")), cex = labelsize)
text(1.5, 6.1, expression(phantom() == phantom()), cex = labelsize)
text(3, 5.4, expression(paste("Marginal revenue")), cex = labelsize)
text(1.5, 4.7, expression(paste("product")), cex = labelsize)

#segments(0, 7, xlims[2], 7, lty = 2, col = COLB[4] , lwd = segmentlinewidth)
segments(0, 4, xlims[2], 4, lty = 1, col = COLB[3], lwd = graphlinewidth)
segments((15-4)/0.75, 0, 11/0.75, 4, lty = 2, col = "gray", lwd = segmentlinewidth)
#segments((10-7)/0.75, 0, (10-7)/0.75, 7, lty = 2, col = "gray", lwd = segmentlinewidth)
segments((10-4)/0.75, 0, (10-4)/0.75, 4, lty = 2, col = "gray", lwd = segmentlinewidth)
points((15-4)/0.75, 4, pch = 16, col = "black", cex = 1.5)
text((15-4)/0.75 + 0.5, 4 + 0.6, expression(paste(n[2])), cex = labelsize)
points((10-4)/0.75, 4, pch = 16, col = "black", cex = 1.5)
text((10-4)/0.75 - 0.5, 4 - 0.6, expression(paste(n[1])), cex = labelsize)

# bracket
# text(11.5, 6.6, expression(paste("Marginal revenue")), cex = labelsize)
# text(11.5, 6, expression(paste("product")), cex = labelsize)
# brackets(x1 = (10 - 4)/0.75, y1 = 4.5, x2 =  (15 - 4)/0.75, y2 = 4.5,  ticks = 0.5, curvature = 0.5, type = 1, 
#          col = "black", lwd = 2, lty = 1, xpd = TRUE)


#Label Demand
text(13.1, 1, expression(paste(mb[1])), cex = labelsize)
text(19.6, 1, expression(paste(mb[2])), cex = labelsize,xpd =TRUE)

dev.off()
