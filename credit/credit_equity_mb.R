#Graph Designer: Simon Halliday, Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "credit/credit_equity_mb.pdf", width = 7, height = 7)

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

par(mar =  c(5, 7, 1, 1))

mc <- function(f, mu = 2) {
  -mu + 2*mu*f
}


xlims <- c(0, 1.15)
ylims <- c(0, 3)

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


ticksy <- c(0, mc(f = 0.65), mc(f = 0.85), ylims[2])
ylabels <- c(0, expression(paste(mb[w])), expression(paste(mb[p] == delta)),  NA)
ticksx <- c(0, 0.5, 0.65, 0.85,  xlims[2])
xlabels <- c(0, 0.5, expression(paste(f[w] )), expression(paste(f[p])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

# text(x = c(0, 
#            0.5, 
#            0.68, 
#            0.92
#            ), 
# par("usr")[3] - 0.025, cex = labelsize,
# labels = xlabels, srt = 0, pos = 1, xpd = TRUE)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Lines for mrs graph
lines(xx1, mc(xx1, mu = 2), col = COLA[4], lwd = graphlinewidth)
#lines(xx1, mrpL(xx1, pmax = 15), col = COLA[4], lty = 2, lwd = segmentlinewidth)

#Label axes
#mtext(expression(paste("Speed of the machine, ", f)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.25, 0.5*(ylims[2] + ylims[1]), expression(paste("Marginal benefit and marginal costs,", list(mb, mc) )), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(0.5*(xlims[2]), -0.4, expression(paste("Speed of the machine, ", f)), xpd = TRUE, cex = axislabelsize) 


text(0.9, 2.4, expression(paste(mc == -q*(1 - 2*f) )), cex = labelsize)
text(0.25, 1.5, expression(paste(mb[p] == delta )), cex = labelsize)
text(0.25, 0.7, expression(paste(mb[w] == delta*(1 - k) )), cex = labelsize)

Arrows(2, 9, 6.8, 9, col = "black", lty = 1, code = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(3.5, 10, expression(paste("Increase in ", frac(dy, dl))), cex = labelsize)

text(9.3, 12, expression(paste(frac(dy, dl) == phantom())), cex = labelsize)
text(11, 12.3, expression(paste("marginal")), cex = labelsize)
text(11, 11.7, expression(paste("benefit")), cex = labelsize)
Arrows(11, 11.3, 11, 2.4, col = "black", lty = 1, code = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

segments(0.65, 0, 0.65, mc(f = 0.65), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, mc(f = 0.65), xlims[2], mc(f = 0.65), lty = 1, col = COLB[4] , lwd = segmentlinewidth)
segments(0.85, 0, 0.85, mc(f = 0.85), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, mc(f = 0.85), xlims[2], mc(f = 0.85), lty = 1, col = COLB[4] , lwd = segmentlinewidth)
#segments(1, 0, 1, mc(f = 1), lty = 2, col = grays[20] , lwd = segmentlinewidth)
#segments(0, mc(f = 1), xlims[2], mc(f = 1), lty = 1, col = grays[20] , lwd = segmentlinewidth)

#points(1, mc(1), pch = 16, col = "black", cex = 1.5)
points(0.85, mc(0.85), pch = 16, col = "black", cex = 1.5)
points(0.65, mc(0.65), pch = 16, col = "black", cex = 1.5)

#text(1 + 0.025, mc(1) - 0.1, expression(paste(e)), cex = labelsize)
text(0.85 + 0.03, mc(0.85) - 0.08, expression(paste(p)), cex = labelsize)
text(0.65 + 0.03, mc(0.65) - 0.08, expression(paste(w)), cex = labelsize)


#Label Demand
text(13, 1, expression(paste(mb[1])), cex = labelsize)
text(19.5, 1, expression(paste(mb[2])), cex = labelsize)

dev.off()
