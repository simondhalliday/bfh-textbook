require(shape)
pdf(file = "specprodexch/production_technique.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 5, 4, 4))

mrsA <- function(x, rmax = 10, xmax = 20) {
  rmax - (rmax/xmax)*x
}

uA <- function(x, y, rmax, rmax = 10, xmas = 20) {
  y + rmax*x - (1/2)(rmax/xmax)*x^2
}

xlims <- c(0, 12)
ylims <- c(0, 12)

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
ticksy <- c(0, 6, 8, ylims[2])
ylabels <- c(NA, expression(paste(k[1])), expression(paste(k[2])), NA)
ticksx <- c(0, 4, 6, xlims[2])
xlabels <- c(NA, expression(paste(l[1])), expression(paste(l[2])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Infeasible area
xpoly <- c(0, 4, 4, xlims[2], xlims[2], 0, 0)
ypoly <- c(ylims[2], ylims[2], 6, 6, 0, 0, ylims[2])
polygon(x = xpoly, y = ypoly, col = COLB[1], density=NULL, border = NA)

#Feasible area
xpoly1 <- c(xlims[2], 4, 4,  xlims[2], xlims[2])
ypoly1 <- c(ylims[2], ylims[2], 6, 6, ylims[2])
polygon(x = xpoly1, y = ypoly1, col = COLA[1], density=NULL, border = NA)

#lines(xx1, mrsA(xx1, rmax = 20, xmax = 10), col = COLA[4], lwd = graphlinewidth)
#lines(xx1, brfB(xx1, alpha = 16, beta = 1/24), col = COLB[4], lwd = graphlinewidth)

#Production set
segments(4, 6, xlims[2], 6, lty = 1, col = COLA[3], lwd = graphlinewidth)
segments(4, 6, 4, ylims[2], lty = 1, col = COLA[3],  lwd = graphlinewidth)

#Segments to i
segments(0, 6, 4, 6, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(4, 0, 4, 6, lty = 2, col = "gray", lwd = segmentlinewidth)

#For k2
segments(0, 8, 4, 8, lty = 2, col = "gray", lwd = segmentlinewidth)

#For l2
segments(6, 0, 6, 6, lty = 2, col = "gray", lwd = segmentlinewidth)



mtext(expression(paste("Hours of labor, ", l)), side=1, line = 2.5, cex = axislabelsize)
text(-1, 0.5*ylims[2], expression(paste("Amount of Capital, ", k)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#For good production technique i
points(4, 6, pch = 16, col = "black", cex = 1.5)
text(4.25, 6.25, expression(i))

#For production technique a
points(4, 8, pch = 16, col = "black", cex = 1.5)
text(4.25, 8.25, expression(a))

#For production technique b
points(6, 6, pch = 16, col = "black", cex = 1.5)
text(6.25, 6.25, expression(b))


text(10, 11, expression(paste("Feasible")), cex = labelsize)
text(10, 10.5, expression(paste("production")), cex = labelsize)
text(10, 10, expression(paste("techniques")), cex = labelsize)
text(2, 4, expression(paste("Infeasible")), cex = labelsize)
text(2, 3.5, expression(paste("production")), cex = labelsize)
text(2, 3, expression(paste("techniques")), cex = labelsize)

#text(7.3, 2.5, expression("Curve"))
#Arrows(7.3, 3.5, 7.3, 6.1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the iso-welfare functions for the HG, Aisha
# text(3.8, 1.5, expression(u[1]^A))
# text(4.6, 1.5, expression(u[2]^A))
# text(5.5, 1.5, expression(u[3]^A))
#text(6.6, 8.3, expression(u[4]^A))


#Label the indifference curves for the HG, Betty
# text(7.6, 17, expression(u[1]^B))
# text(6.75, 17, expression(u[2]^B))
# text(6, 17, expression(u[3]^B))
#text(3.4, 6.9, expression(v[4]^B))

#Label mrs function
text(18, 2.5, expression(paste(mrs(x,y) == r[max] - bgroup("(",frac(r[max], x[max]),")")*x)))
#Arrows(10, 7.5, 10, 5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label satiation
#text(20, 3.5, expression(paste(x[max] == "Point")))
#text(20, 3, expression(paste("of Satiation")))
#Arrows(20, 2.5, 20, 0.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label highest willingness to pay
#text(5, 10, expression("Consumer Surplus"))
#text(5, 9, expression(paste(CS==frac(1, 2)*bgroup("(",r[max] - p,")")*x)))
#Arrows(5, 8.5, 5, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

dev.off()
