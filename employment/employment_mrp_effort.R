require(shape)
pdf(file = "employment/employment_mrp_effort.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(6, 6, 4, 4))

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
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
# ticksx <- seq(from = 0, to = xlims[2], by = 2)
# xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(0, 4, 7, ylims[2])
ylabels <- c(NA, expression(paste(w[1])), expression(paste(w[2])), NA)
ticksx <- c(0, (10-7)/0.75, (10-4)/0.75, (15-4)/0.75, xlims[2])
xlabels <- c(NA, expression(paste(L^N*(B^H))), expression(paste(L^{N1})), expression(paste(L^{N2})), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Lines for mrs graph
lines(xx1, mrpL(xx1, pmax = 10), col = COLA[4], lwd = graphlinewidth)
lines(xx1, mrpL(xx1, pmax = 15), col = COLA[4], lty = 2, lwd = segmentlinewidth)

#Label axes
mtext(expression(paste("Total labor used by the firm, ", L == eh)), side=1, line = 2.5, cex = axislabelsize)
text(-2, 0.5*ylims[2], expression(paste("Wage, ", w)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Arrows(18, 4, 18, 6.5, col = "black", lty = 1, code = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
#text(16, 5.75, expression(paste("Change in ", mu)), cex = labelsize)


text(18.3, 7.4, expression(paste(mu[2], " (higher B)")), cex = labelsize)
text(19, 5, expression(paste(mu[1] == frac(w^N,e^N))), cex = labelsize)
text(16, 12.6, expression(paste("marginal cost")), cex = labelsize)
text(16, 12, expression(paste("of effort")), cex = labelsize)
Arrows(16, 11.6, 16, 4.4, col = "black", lty = 1, code = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

Arrows(2, 9, 6.8, 9, col = "black", lty = 1, code = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(3.5, 10, expression(paste("Increase in ", frac(dy, dL))), cex = labelsize)

text(9.3, 12, expression(paste(frac(dy, dL)==phantom())), cex = labelsize)
text(11, 12.3, expression(paste("marginal")), cex = labelsize)
text(11, 11.7, expression(paste("product")), cex = labelsize)
Arrows(11, 11.4, 11, 2.4, col = "black", lty = 1, code = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

segments(0, 7, xlims[2], 7, lty = 2, col = COLB[4] , lwd = segmentlinewidth)
segments(0, 4, xlims[2], 4, lty = 1, col = COLB[3], lwd = segmentlinewidth)
segments((15-4)/0.75, 0, 11/0.75, 4, lty = 2, col = "gray", lwd = segmentlinewidth)
segments((10-7)/0.75, 0, (10-7)/0.75, 7, lty = 2, col = "gray", lwd = segmentlinewidth)
segments((10-4)/0.75, 0, (10-4)/0.75, 4, lty = 2, col = "gray", lwd = segmentlinewidth)
points((15-4)/0.75, 4, pch = 16, col = "black", cex = 1.5)
points((10-7)/0.75, 7, pch = 16, col = "black", cex = 1.5)
points((10-4)/0.75, 4, pch = 16, col = "black", cex = 1.5)



# text(80, 12.5, expression(paste("Excess Supply at ", p^H)), cex = labelsize)
# 
# Arrows(62, 5.5, 82, 5.5, col = "black", lty = 1, code = 3, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# text(72, 4.75, expression(paste("Excess")), cex = labelsize)
# text(72, 3.75, expression(paste("Demand")), cex = labelsize)
# text(72, 2.75, expression(paste("at ", p^L)), cex = labelsize)



#Label Demand
text(118, 3, expression(paste("Opportunity cost'")), cex = labelsize)
text(118, 2, expression(paste("of capital")), cex = labelsize)

dev.off()
