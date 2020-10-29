require(shape)
pdf(file = "specprodexch/cd_isoquant_comparison.pdf", width = 7, height = 7)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 5, 1, 1))

isocost <- function(l, c = 10, w = 1, r = 1) {
  c - (w/r)*l
}


isoquant <- function(l, alpha = 0.5, x = 5) {
  (x / l^alpha)^(1/(1 - alpha))
}


xlims <- c(0, 10)
ylims <- c(0, 10)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(13, 16.125, 19.25)

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
# ticksy <- c(0, isoquant(l = 2.68), isoquant(l = 5), isoquant(l = 9.35) , ylims[2])
# ylabels <- c(NA, expression(paste(k[a])), expression(paste(k,"*")), expression(paste(k[b])), NA)
# ticksx <- c(0, 2.68, 5, 9.35, xlims[2])
# xlabels <- c(NA, expression(paste(l[a])), expression(paste(l,"*")), expression(paste(l[b])), NA)

ticksy <- c(0, NA, ylims[2])
ylabels <- c(NA, NA, NA)
ticksx <- c(0, NA, xlims[2])
xlabels <- c(NA, NA, NA)



axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#lines(xx1, bcA(xx1, w = 10, p = 1.5), col = COLB[3], lwd = graphlinewidth)
# lines(xx1, isocost(xx1, c = 10, w = 1, r = 1), col = COLB[3], lwd = graphlinewidth)
# lines(xx1, isocost(xx1, c = 8, w = 1, r = 1), col = COLB[3], lwd = graphlinewidth)
# lines(xx1, isocost(xx1, c = 12, w = 1, r = 1), col = COLB[3], lwd = graphlinewidth)
lines(xx1, isoquant(xx1, x = 5, alpha = 0.5), col = COLA[3], lwd = graphlinewidth)
lines(xx1, isoquant(xx1, x = 3, alpha = 0.5), col = COLA[3], lwd = graphlinewidth)
lines(xx1, isoquant(xx1, x = 7, alpha = 0.5), col = COLA[3], lwd = graphlinewidth)

#Label the axes
mtext(expression(paste("Hours of labor, ", l)), side=1, line = 2.5, cex = axislabelsize)
text(-1, 0.5*ylims[2], expression(paste("Quantity of capital goods, ", k)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the isoquant curve
#text(11.25, 3.15, expression("isoquant"), cex = labelsize)

text(11.25, 1.15, expression(paste(underline(x)[1])), cex = labelsize)
text(11.25, 2.65, expression(paste(underline(x)[2])), cex = labelsize)
text(11.25, 4.75, expression(paste(underline(x)[3])), cex = labelsize)


#Label the price lines
#text(6.75, 1, expression(paste(c[1])))
#text(8, 1.3, expression(paste(p[x] == 1)))
#text(8.75, 1, expression(paste(c[2])))
#text(11.3, 3.7, expression(paste(p[x] == 0.5)))
#text(10.75, 1, expression(paste(c[3])))
#text(11.3, 6.5, expression(paste(p[x] == 0.25)))

#Add the contour plot for the indifference curves
# contour(x, y, 
#         outer(x, y, uA),
#         drawlabels = FALSE,
#         col = COLA[3],
#         lwd = graphlinewidth,
#         levels = a, 
#         xaxs="i", 
#         yaxs="i", 
#         add = TRUE)

#Add the offer curve (superimposed on the indifference curves tangent to the price lines)
#xx2 <- seq(2, xlims[2], length.out = npts)
#lines(xx2, offerCurve(xx2, w = 10, rmax = 2, xmax = 12), col = COL[3], lwd = graphlinewidth)

#Annotate points on offer curve mrs = p for each of p = 1, 0.5, 0.25
#Where mrs = 2 - (1/6)*x

#Label a
# segments(2.68, 0, 2.68, isoquant(l = 2.68), lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(0, isoquant(l = 2.68) , 2.68, isoquant(l = 2.68), lty = 2, col = "gray" , lwd = segmentlinewidth)
# points(2.68, isoquant(l = 2.68), pch = 16, col = "black", cex = 1.5)
# text(2.68 + 0.25, isoquant(l = 2.68) + 0.25, expression(paste(a)), cex = labelsize)

#Label b
# segments(9.35, 0, 9.35, isoquant(9.35), lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(0, isoquant(l = 9.35) , 9.35, isoquant(l = 9.35), lty = 2, col = "gray" , lwd = segmentlinewidth)
# points(9.35, isoquant(l = 9.35), pch = 16, col = "black", cex = 1.5)
# text(9.35 + 0.25, isoquant(l = 9.35) + 0.25, expression(paste(b)), cex = labelsize)


#Label i
# segments(0, 5, 5, 5, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(5, 0, 5, 5, lty = 2, col = "gray" , lwd = segmentlinewidth)
# points(5, 5, pch = 16, col = "black", cex = 1.5)
# text(5.25, 5.25, expression(paste(i)), cex = labelsize)

#Add mrs = mrt at i
# text(5, 10.25, expression(paste(mrts(l,k) == mrt(l,k))), cex = labelsize)
# text(5, 9.25, expression(paste(frac(mp[l], mp[k]) == frac(w,p[k]))), cex = labelsize)
# Arrows(5, 8.5, 5, 5.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#text(9, 5.9, expression(paste(b)), cex = labelsize)
#text(10.5, 7.775, expression(paste(c)), cex = labelsize)

dev.off()
