require(shape)
pdf(file = "firmmarketsupply/tech_change_isoquant.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 6, 4, 4))

isocost <- function(l, c = 10, w = 1, r = 1) {
  c - (w/r)*l
}


isoquant <- function(l, alpha = 0.5, x = 5, A = 1) {
  (x / (A*l^alpha))^(1/(1 - alpha))
}


xlims <- c(0, 12)
ylims <- c(0, 12)

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

# ticksy <- seq(from = ylims[1], to = ylims[2], by = 1)
# ylabels <- seq(from = ylims[1], to = ylims[2], by = 1)
# ticksx <- seq(from = xlims[1], to = xlims[2], by = 1)
# xlabels <- seq(from = xlims[1], to = xlims[2], by = 1)
ticksy <- c(0, 6 , ylims[2])
ylabels <- c(NA, expression(paste(k[i])==k[t]), NA)
ticksx <- c(0, 3, 6, xlims[2])
xlabels <- c(NA, expression(paste(l[t])), expression(paste(l[i])), NA)




axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#lines(xx1, bcA(xx1, w = 10, p = 1.5), col = COLB[3], lwd = graphlinewidth)
#lines(xx1, isocost(xx1, c = 10, w = 1, r = 1), col = COLB[3], lwd = graphlinewidth)
lines(xx1, isocost(xx1, c = 9, w = 1, r = 1), col = COLB[3], lwd = graphlinewidth)
lines(xx1, isocost(xx1, c = 12, w = 1, r = 1), col = COLB[3], lwd = graphlinewidth)
lines(xx1, isoquant(xx1, x = 5, alpha = 0.5, A = 0.83), col = COLA[3], lwd = graphlinewidth)
lines(xx1, isoquant(xx1, x = 5, alpha = 0.33, A = 1.05), col = COLA[4], lwd = graphlinewidth)

#Label the axes
mtext(expression(paste("Hours of labor, ", l)), side=1, line = 2.5, cex = axislabelsize)
text(-1.6, 0.5*ylims[2], expression(paste("Amount of capital, ", k)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the isoquant curve
text(11.25, 4.5, expression("initial"), cex = labelsize)
text(11.25, 4, expression("isoquant"), cex = labelsize)
text(11.25, 3.6, expression(paste(x[i] == bar(x))), cex = labelsize)


text(11.25, 2.8, expression("innovation"), cex = labelsize)
text(11.25, 2.3, expression("isoquant"), cex = labelsize)
text(11.25, 1.9, expression(paste(x[t] == bar(x))), cex = labelsize)

#Label the price lines
text(7.75, 1, expression(paste(c[1])))
text(10.75, 1, expression(paste(c[2])))

#Label a
segments(0, 6, 6, 6, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(6, 0, 6, 6, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(6, 6, pch = 16, col = "black", cex = 1.5)
text(6.25, 6.25, expression(paste(a)), cex = labelsize)

#Label b
segments(0, isoquant(l = 3, alpha = 0.33, A = 1.05), 3, isoquant(l = 3, alpha = 0.33, A = 1.05), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(3, 0, 3, isoquant(l = 3, alpha = 0.33, A = 1.05), lty = 2, col = "gray" , lwd = segmentlinewidth)
points(3, isoquant(l = 3, alpha = 0.33, A = 1.05), pch = 16, col = "black", cex = 1.5)
text(2.75, isoquant(l = 3, alpha = 0.33, A = 1.05) - 0.25, expression(paste(b)), cex = labelsize)
Arrows(5.8, 5.8, 3.5, isoquant(l = 3, alpha = 0.33, A = 1.05) - .2, col = "black", lty = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)




#Add mrs = mrt at i
# text(5, 10.25, expression(paste(trs(l,k) == mrt(l,k))), cex = labelsize)
# text(5, 9.25, expression(paste(frac(mp[l], mp[k]) == frac(w,r))), cex = labelsize)
# Arrows(5, 8.5, 5, 5.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#New tech cost min
text(9, 2, expression("innovation"), cex = labelsize)
text(9, 1.5, expression("decreases"), cex = labelsize)
text(9, 1, expression("costs"), cex = labelsize)
Arrows(9.5, 2.25, 7, 2.25, col = "black", lty = 2, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


text(10, 10, expression("Innovation expands"), cex = labelsize)
text(10, 9.5, expression("the production set"), cex = labelsize)
text(10, 9, expression("creating a new"), cex = labelsize)
text(10, 8.5, expression("production isoquant"), cex = labelsize)
text(10, 8, expression(paste("for the same ", x == bar(x))), cex = labelsize)


text(1.5, 5.25, expression("At the new"), cex = labelsize)
text(1.5, 4.75, expression("isoquant, the"), cex = labelsize)
text(1.5, 4.25, expression("firm reaches"), cex = labelsize)
text(1.5, 3.75, expression(paste("lower isocost ", c[1])), cex = labelsize)
text(1.5, 3.25, expression("and it employs "), cex = labelsize)
text(1.5, 2.75, expression(paste("the same capital ")), cex = labelsize)
text(1.5, 2.25, expression(paste("and less labor at")), cex = labelsize)
text(1.5, 1.75, expression(paste("identical relative ")), cex = labelsize)
text(1.5, 1, expression(paste("prices, ", frac(w,r))), cex = labelsize)


#text(9, 5.9, expression(paste(b)), cex = labelsize)
#text(10.5, 7.775, expression(paste(c)), cex = labelsize)

dev.off()
