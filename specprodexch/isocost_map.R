require(shape)
pdf(file = "specprodexch/isocost_map.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 6, 2, 2))

isocost <- function(l, c = 10, w = 1, r = 1) {
  c - (w/r)*l
}


isoquant <- function(l, alpha = 0.5, x = 5) {
  (x / l^alpha)^(1/(1 - alpha))
}


xlims <- c(0, 13)
ylims <- c(0, 13)

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
     xaxs="i", 
     yaxs="i")

ticksy <- c(0, 6, 9, 12, ylims[2])
ylabels <- c(NA, expression(paste(k[1]==frac(c[1], r))), expression(paste(k[2]==frac(c[2], r))), expression(paste(k[3]==frac(c[3], r))), NA)
ticksx <- c(0, 6, 9, 12, xlims[2])
xlabels <- c(NA, expression(paste(l[1]==c[1]/w)), expression(paste(l[2])==c[2]/w), expression(paste(l[3]==c[3]/w)), NA)




axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)


lines(xx1, isocost(xx1, c = 6, w = 1, r = 1), col = COLB[3], lwd = graphlinewidth)
lines(xx1, isocost(xx1, c = 9, w = 1, r = 1), col = COLB[3], lwd = graphlinewidth)
lines(xx1, isocost(xx1, c = 12, w = 1, r = 1), col = COLB[3], lwd = graphlinewidth)


#Label the axes
mtext(expression(paste("Hours of labor, ", l)), side=1, line = 2.5, cex = axislabelsize)
text(-1.8, 0.5*ylims[2], expression(paste("Amount of capital, ", k)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the price lines
text(5.4, 1, expression(paste(c[1])))
text(8.4, 1, expression(paste(c[2])))
text(11.4, 1, expression(paste(c[3])))


#Add mrs = mrt at i
text(7, 10.25, expression(paste("Marginal rate of transformation:")), cex = labelsize)
text(7, 9.25, expression(paste(mrt(l,k)== frac(w,r))), cex = labelsize)
Arrows(7, 8.5, 7, 5.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

dev.off()