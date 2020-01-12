#Graph Designer: Bridget Diana
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "mathappendix/BottleCorkSystemOfEqns.pdf", width = 8, height = 6)
par(mar=c(4,7,0.75,0.75))

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

xlims <- c(0.95, 1.1)
ylims <- c(0, 0.05)

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
     yaxs="i"
)


ticksy <- seq(from = 0, to = ylims[2], by = .01)
ylabels <- seq(from = 0, to = ylims[2], by = .01)
ticksx <- seq(from = 0.9, to = xlims[2], by = .05)
xlabels <- seq(from = 0.9, to = xlims[2], by = .05)

axis(1, at = c(ticksx, 1.025), pos = 0, labels = c(xlabels,expression(paste(p^"*"))))
axis(2, at = c(ticksy,0.025), pos = 0.95, labels = c(ylabels,expression(paste(q^"*"))), las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)

#Axis labels
mtext(expression(paste("Price of bottle, ", p)), side = 1, line = 2.5, cex = axislabelsize)
text(0.925, 0.03, expression(paste("Price of cork, ", q)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#equations:
abline(a=1.05, b=-1, col = COLB[4], lwd = graphlinewidth)
abline(a=-1, b=1,col=COLA[4], lwd = graphlinewidth )

#Segments before point
segments(0.9, 0.025, 1.025, 0.025, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(1.025, 0, 1.025, 0.025, lty = 2, col = "gray", lwd = segmentlinewidth)


#Annotate point
points(1.025, 0.025, pch = 16, col = "black", cex = 1.5)


dev.off()
