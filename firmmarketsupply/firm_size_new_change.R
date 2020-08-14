#Graph Designer: Sai Mumunuru, Simon Halliday + Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(pBrackets)
require(shape)
#library(matlib)
pdf(file = "firmmarketsupply/firm_size_new_change.pdf", width = 9, height = 7)

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
Grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00")

par(mar =  c(4, 7, 2, 2))

firmDeath <- function(x, s2 = 2.6, pmin = -10) {
  (pmin + s2*x)
}

firmGrowth <- function(x, s1 = 1.2) {
  (s1*x)
}  

#Find the x value where the two lines intersect
sol <- uniroot(function(x)  firmDeath(x) - firmGrowth(x),
               c(.01,10), tol=1e-8)  
#isolate x from the values in uniroot
xsol <- sol$root 

#Find the x value where the two lines intersect
sol2 <- uniroot(function(x)  firmDeath(x) - firmGrowth(x, s1 = 1.5),
               c(.01,10), tol=1e-8)  
#isolate x from the values in uniroot
xsol2 <- sol2$root 

xlims <- c(0, 16)
ylims <- c(-10, 20)

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

ticksy <- c(ylims[1], firmDeath(xsol),  firmDeath(xsol2), ylims[2])
ylabels <- c(expression(-f*underline(s)), expression(paste(g[b] == d[b])), expression(paste(g[e]==d[e])),  NA)
ticksx <- c(xlims[1], xsol, xsol2, xlims[2])
xlabels <- c(NA, expression(paste(bar(s)[b])), expression(paste(bar(s)[e])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)


npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, firmDeath(xx1), col = CBCols[2], lwd = graphlinewidth)
lines(xx1, firmGrowth(xx1), col = CBCols[1], lwd = graphlinewidth, lty = 2)
lines(xx1, firmGrowth(xx1, s1 = 1.5), col = CBCols[1], lwd = graphlinewidth, lty = 1)

mtext(expression(paste("Average firm size, ", bar(s))), side=1, line = 2.5, cex = axislabelsize)
text(-2.4, 0.5*(ylims[1] + ylims[2]), expression(paste("Increase or decrease in average firm size")), xpd = TRUE, cex = axislabelsize, srt = 90)
#mtext(expression(paste("Increase or decrease in average firm size")), side=2, line = 1, cex = axislabelsize)

#Segments and point at the solution
segments(0, firmGrowth(xsol), xsol, lty = 2, col = Grays[20] , lwd = segmentlinewidth)
segments(xsol, 0, xsol, firmGrowth(xsol), lty = 2, col = Grays[20] , lwd = segmentlinewidth)
points(xsol, firmGrowth(xsol), pch = 16, col = "black", cex = 1.5)
text(xsol + 0.3, firmGrowth(xsol) - 0.5, expression(paste(b)), xpd = TRUE, cex = labelsize)

#Segments and point at the solution
segments(0, firmGrowth(xsol2, s1 = 1.5), xsol2, lty = 2, col = Grays[20] , lwd = segmentlinewidth)
segments(xsol2, 0, xsol2, firmGrowth(xsol2, s1 = 1.5), lty = 2, col = Grays[20] , lwd = segmentlinewidth)
points(xsol2, firmGrowth(xsol2, s1 = 1.5), pch = 16, col = "black", cex = 1.5)
text(xsol2 + 0.3, firmGrowth(xsol2, s1 = 1.5) - 0.5, expression(paste(e)), xpd = TRUE, cex = labelsize)


# segments(0, 15, 10, lty = 2, col = Grays[20] , lwd = segmentlinewidth)
# segments(10, 0, 10, 15, lty = 2, col = Grays[20] , lwd = segmentlinewidth)
#points(10, 15, pch = 16, col = "black", cex = 1.5)

#text(-0.65, -8, expression(-f*underline(s)), cex = labelsize, xpd = TRUE )
#Label the figures
#text(14.4, firmGrowth(x=12, s1 = 1.5) + 2.5, expression(paste(bar(s)(1 - f)*g["+"]^s)),cex = labelsize, xpd = TRUE)

text(9.5, firmDeath(x = 9.5) + 6, expression(paste("Firm death effect")), cex = labelsize, xpd = TRUE)
text(9.5, firmDeath(x = 9.5) + 4.5, expression(f(bar(s) - underline(s))), cex = labelsize, xpd = TRUE)

text(14.3, firmGrowth(x=14.3) - 3.4, expression(paste("Firm growth effect")), cex = labelsize, xpd = TRUE)
text(14.3, firmGrowth(x=14.3) - 5, expression(bar(s)(1 - f)*g[1]^s),cex = labelsize)

text(14.3, firmGrowth(x=14.3, s1 = 1.5) - 0.6, expression(bar(s)(1 - f)*g[2]^s),cex = labelsize, xpd = TRUE)

Arrows(12, firmGrowth(12) + 0.2, 12, firmGrowth(12, s1 = 1.5) - 0.6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5, xpd = TRUE)
# brackets(x1 = 4.2 - 0.2, y1 = firmDeath(4.2) + 0.1,
#          x2 = 4.2 - 0.2, y2 = firmGrowth(x = 4.2) - 0.5,
#          ticks = 0.35, curvature = 0.5, type = 1, h = 0.4,
#          col = "black", lwd = 2, lty = 1, xpd = TRUE)

# text(1.65, 4, expression(paste(g[a] > d[a])),cex = labelsize)
# text(1.65, 2.5, expression(paste("average firm")),cex = labelsize)
# text(1.65, 1, expression(paste("size increases")),cex = labelsize)
# text(2.8, 2, expression(paste(g[a] > d[a*minute])),cex = labelsize)
# 
# brackets(x1 = 10 + 0.2, y1 = firmDeath(10) + 0.1,
#          x2 = 10 + 0.2, y2 = firmGrowth(x = 10) + 0.5,
#          ticks = 0.25, curvature = 0.5, type = 1, h = 0.4, 
#          col = "black", lwd = 2, lty = 1, xpd = TRUE)
# 
# text(11.5, 15.4, expression(paste(d[c*minute] > g[c])),cex = labelsize)

# segments(0, firmDeath(10), 10, firmDeath(10), lty = 2, col = Grays[20] , lwd = segmentlinewidth)
# segments(0, firmGrowth(10), 10, firmGrowth(10), lty = 2, col = Grays[20] , lwd = segmentlinewidth)
# segments(10, 0, 10, firmDeath(10), lty = 2, col = Grays[20] , lwd = segmentlinewidth)
# points(10, firmGrowth(10), pch = 16, col = "black", cex = 1.5)
# text(10 - 0.3, firmDeath(10) + 0.6, expression(paste(c*minute)),cex = labelsize)
# text(10 + 0.3, firmGrowth(10) - 0.6, expression(paste(c)),cex = labelsize)
# 
# points(10, firmDeath(10), pch = 16, col = "black", cex = 1.5)
# 
# segments(0, firmGrowth(4.2), 4.2, firmGrowth(4.2), lty = 2, col = Grays[20] , lwd = segmentlinewidth)
# segments(0, firmDeath(4.2), 4.2, firmDeath(4.2), lty = 2, col = Grays[20] , lwd = segmentlinewidth)
# segments(4.2, 0, 4.2, firmGrowth(4.2), lty = 2, col = Grays[20] , lwd = segmentlinewidth)
# points(4.2, firmGrowth(4.2), pch = 16, col = "black", cex = 1.5)
# 
# points(4.2, firmDeath(4.2), pch = 16, col = "black", cex = 1.5)
# text(4.2 + 0.3, firmDeath(4.2) - 0.3, expression(paste(a*minute)),cex = labelsize)
# text(4.2 - 0.3, firmGrowth(4.2) + 0.6, expression(paste(a)),cex = labelsize)


dev.off()
