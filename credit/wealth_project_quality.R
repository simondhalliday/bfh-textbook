#Graph Designer: Simon Halliday, Scott Cohn
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "credit/wealth_project_quality.pdf", width = 8, height = 6)

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

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 7, 2, 2))

LWealthQ <- function(n, int = 8, slope = 6) {
  int - slope*n
}

HWealthQ <- function(n, int = 2, slope = 6) {
  int + slope*n
}

xlims <- c(0, 1)
ylims <- c(0, 10)

npts <- 501 
d1 <- seq(xlims[1], xlims[2], length.out = npts)
f1 <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(0.0625)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs = "i", 
     yaxs = "i"
)


ticksy <- c(ylims[1], HWealthQ(n = 0.3), 5, LWealthQ(n = 0.3), ylims[2])
ylabels <- c(NA, expression(paste(q^k*(n^0))), expression(paste(q[i])), expression(paste(q^0*(n^0))), NA)
ticksx <- c(xlims[1], 0.3, 0.5, xlims[2])
xlabels <- c(NA, expression(paste(n^{0})),expression(paste(n[i])), NA)
ticksy2 <- c(ylims[1], ylims[2])
ylabels2 <- c(NA, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)
axis(4, at = ticksy2, pos = 1, labels = ylabels2, las = 1, cex.axis = labelsize)

npts <- 503 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Draw the graphs
lines(xx1, LWealthQ(xx1), col = COLA[4], lwd = graphlinewidth)
lines(xx1, HWealthQ(xx1), col = COLB[4], lwd = graphlinewidth)

#Axis labels
mtext(expression(paste("Fraction of low wealth projects funded, ", n)), side = 1, line = 2.8, cex = axislabelsize)
text(-0.19, 0.5*(ylims[2]), expression(paste("Quality of project, ", q)), xpd = TRUE, cex = axislabelsize, srt = 90) 


segments(0.3, 0, 0.3, LWealthQ(n = 0.3), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, LWealthQ(n = 0.3), 0.3, LWealthQ(n = 0.3), lty = 2, col = grays[20] , lwd = segmentlinewidth)
points(0.3, LWealthQ(n = 0.3), pch = 16, col = "black", cex = 1.5)
text(0.3 + 0.02, LWealthQ(n = 0.3) + 0.35, expression(paste(a)), cex = labelsize)
segments(0, HWealthQ(n = 0.3), 0.3, HWealthQ(n = 0.3), lty = 2, col = grays[20] , lwd = segmentlinewidth)
points(0.3, HWealthQ(n = 0.3), pch = 16, col = "black", cex = 1.5)
text(0.3 + 0.02, HWealthQ(n = 0.3) - 0.35, expression(paste(b)), cex = labelsize)

points(0.5, 5, pch = 16, col = "black", cex = 1.5)
text(0.5, 5.5, expression(paste(i)), cex = labelsize)

text(0.15, 1.2, expression(paste("Low wealth")), cex = labelsize)
text(0.15, 0.6, expression(paste("projects funded")), cex = labelsize)
Arrows(0.03, 0.2, 0.27, 0.2, col = "black", code = 3, lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(0.625, 1.2, expression(paste("High wealth")), cex = labelsize)
text(0.625, 0.6, expression(paste("projects funded")), cex = labelsize)
Arrows(0.33, 0.2, 0.97, 0.2, col = "black", code = 3, lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(0.15, 9.4, expression(paste("Low wealth")), cex = labelsize)
text(0.15, 8.8, expression(paste("quality of")), cex = labelsize)
text(0.15, 8.2, expression(paste("marginal project")), cex = labelsize)


text(0.85, 9.4, expression(paste("High wealth")), cex = labelsize)
text(0.85, 8.8, expression(paste("quality of")), cex = labelsize)
text(0.85, 8.2, expression(paste("marginal project")), cex = labelsize)


dev.off()
