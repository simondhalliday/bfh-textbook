require(shape)
pdf(file = "competitionmarkets/makost_market2.pdf", width = 9, height = 7)

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

par(mar =  c(5, 6.5, 0.5, 0.5))

Demand <- function(x, rmax = 20, xmax = 12, n = 10) {
  rmax - (rmax/(n*xmax))*x
}

mrsA <- function(x, rmax = 20, xmax = 10) {
  rmax - (rmax/xmax)*x
}

uA <- function(x, y, rmax = 20, xmax = 10) {
  y + rmax*x - (1/2)(rmax/xmax)*x^2
}


Supply <- function(x, c1 = 0, c2 = 0.05){
  c1 + 2*c2*x
}

xlims <- c(0, 2.5)
ylims <- c(0, 4)

npts <- 501 

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
ticksy <- c(0, 1, 2, 3, ylims[2])
ylabels <- c(NA, expression(paste(c[1])), expression(paste(bar(p)^B)), expression(paste(bar(p)^A)), NA)
ticksx <- c(0, 1, 2, xlims[2])
xlabels <- c(NA, 1, 2, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = axislabelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = axislabelsize)


#Line for Demand
segments(0, 3, 1, 3, lty = 1, col = COLA[4] , lwd = graphlinewidth)
segments(1, 2, 2, 2, lty = 1, col = COLA[4] , lwd = graphlinewidth)
segments(1, 2, 1, 3, lty = 1, col = COLA[4] , lwd = graphlinewidth)
segments(2, 2, 2, 0, lty = 1, col = COLA[4] , lwd = graphlinewidth)

#Line for Supply
segments(0, 1, 0.98, 1, lty = 1, col = COLB[4] , lwd = graphlinewidth)
segments(0.98, 1, 0.98, 4, lty = 1, col = COLB[4] , lwd = graphlinewidth)

#For market price
segments(0, 2, 1, 2, lty = 2, "gray" , lwd = graphlinewidth)

#Label axes
mtext(expression(paste("Quantity, ", x)), side=1, line = 3, cex = axislabelsize)
#text(-0.2, 0.5*ylims[2], expression(paste("Price per unit of x, ", p[X])), xpd = TRUE, cex = axislabelsize, srt = 90) 
# 
# points(50, 10, pch = 16, col = "black", cex = 1.5)
# text(52, 10.5, expression(M))
text(1.75, 0.6, expression("Demand of"), cex = labelsize)
text(1.75, 0.4, expression("two buyers"), cex = labelsize)
text(1.3, 3.7, expression("Single"), cex = labelsize)
text(1.3, 3.5, expression("Seller's Supply"), cex = labelsize)

text(0.32, 2.6, expression("Equilibrium price"), cex = labelsize)
text(0.32, 2.4, expression("falls in this range"), cex = labelsize)
Arrows(0.65, 2.1, 0.65, 2.9, col = "black", code =3, lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


dev.off()
