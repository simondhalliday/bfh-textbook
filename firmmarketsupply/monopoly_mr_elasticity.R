library(pBrackets)
library(shape)

pdf(file = "firmmarketsupply/monopoly_mr_elasticity.pdf", width = 9, height = 7)

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

par(mar =  c(6, 6, 1, 1))

AvgRevenue <- function(x, rmax = 12, xmax = 12){
  rmax - (rmax/xmax)*x
}

MRevenue <- function(x, rmax = 12, xmax = 12){
  rmax - 2*(rmax/xmax)*x
}


xlims <- c(0, 12.5)
ylims <- c(0, 12.5)



npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

ticksy <- c(0, 4, AvgRevenue(x = 4), 12, ylims[2])
ylabels <- c(NA, expression(paste(c)), expression(paste(p^{m})), expression(paste(bar(p))), NA)
ticksx <- c(0, 4, 6, 12, xlims[2])
xlabels <- c(NA, expression(paste(x^{m})), expression(paste(frac(bar(p),2 *beta))), expression(paste(frac(bar(p),beta))), NA)


axis(1, at = ticksx, pos = 0, labels = FALSE)
text(x = c(0, 4, 6, 12, xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE, cex = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Label axes
#Label the axes
text(0.5*(xlims[2]), -2.1, expression(paste("Quantity, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-1.4, 0.5*ylims[2], expression(paste("Price, Revenue and Costs ($)")), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Draw segments for total costs
segments(0, AvgRevenue(x = 4), 4, AvgRevenue(x = 4), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(4, 0, 4, AvgRevenue(x = 4), lty = 2, col = "gray" , lwd = segmentlinewidth)

segments(0, 4, 4, 4, lty = 1, col = COLB[3] , lwd = graphlinewidth)
segments(4, 4, xlims[2], 4, lty = 2, col = COLB[2] , lwd = graphlinewidth)



# Demand
lines(xx1, AvgRevenue(xx1, rmax = 12, xmax = 12), col = COLA[5], lwd = graphlinewidth)
lines(xx1, MRevenue(xx1, rmax = 12, xmax = 12), col = COLA[4], lwd = graphlinewidth)

text(7, 1.8, expression(paste(mr(x) == p*bgroup("(",1 - frac(1, abs(eta[xp])),")" ) ) ) , xpd = TRUE, cex = annotatesize) 

#Label Points for comparison
points(4, MRevenue(x = 4), pch = 16, col = "black", cex = 1.5)
text(3.8, MRevenue(x = 4) - 0.4, expression(i), cex = labelsize)

points(4, AvgRevenue(x = 4), pch = 16, col = "black", cex = 1.5)
text(4 - 0.2, AvgRevenue(x = 4) - 0.4, expression(h), cex = labelsize)


#Label 
text(4, 10.76, expression(paste("Elastic")), cex = annotatesize)
text(4, 10.25, expression(paste(abs(eta), " > 1")), cex = annotatesize)
text(7, 7.6, expression(paste("Unit elastic")), cex = annotatesize)
text(7, 7, expression(paste(abs(eta), " = 1")), cex = annotatesize)
text(10, 4.8, expression(paste("Inelastic")), cex = annotatesize)
text(10, 4.3, expression(paste(abs(eta) , " < 1")), cex = annotatesize)

# Points
points(6, 6, pch = 16, col = "black", cex = 1.5)

# Braces
brackets(0.25, 12, 6, 6.35, h = NULL,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = segmentlinewidth, lty = 1, xpd = FALSE)
brackets(6.4, 6.1, 12, 0.25, h = NULL,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = segmentlinewidth, lty = 1, xpd = FALSE)

# Arrows
Arrows(6.7, 6.7, x1 = 6.25, y1 = 6.25,
       code = 2, lty = 1,
       lwd = segmentlinewidth,
       arr.type = "triangle", arr.lwd = 0.5)

dev.off()