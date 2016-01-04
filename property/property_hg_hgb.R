#based on 
#http://r.789695.n4.nabble.com/Indifference-curve-td4634746.html
#Graphics 
require(shape)
pdf(file = "bfh-textbook/property/property_hg_hgb.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 3

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 4, 4, 4))

uA <- function(x, y) {
  0.5*log(x) + 0.5*log(y) + 0.15*log(10 - x) + 0.15*log(15 - y)
}

uB <- function(x, y) {
  0.15*log(x) + 0.15*log(y) + 0.5*log(10 - x) + 0.5*log(15 - y) 
}

xlims <- c(0, 10)
ylims <- c(0, 15)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(2.1, 2.357, 2.505, 2.54)
b <- c(2.1, 2.357, 2.505, 2.54)

#B's value when at A's bliss point
#0.35*log(5.88) + 0.35*log(8.88) + 0.5*log(10 - 5.88) + 0.5*log(15 - 8.88) 

#A's bliss point x = 7.69231, y = 11.5385
#A's value when at A's bliss point = 2.55465
#0.5*log(x) + 0.5*log(y) + 0.15*log(10 - x) + 0.15*log(15 - y)

#B's bliss point x = 2.30769, y = 3.46154
#B's value when at A's bliss point = 2.55465
#0.15*log(x) + 0.15*log(y) + 0.5*log(10 - x) + 0.5*log(15 - y)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

ticksy <- seq(from = 0, to = 15, by = 1)
ylabels <- seq(from = 0, to = 15, by = 1)
ticksx <- seq(from = 0, to = 10, by = 1)
xlabels <- seq(from = 0, to = 10, by = 1)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0)


#persp(x, y, outer(x, y, u), ticktype="detailed") 
contour(x, y, 
        outer(x, y, uA),
        #labels = c("v1", "v2", "v3"),
        drawlabels = FALSE,
        col = COLA[3],
        #xlab = expression(paste("A's Apples, ", x)),
        #ylab = expression(paste("A's Oranges, ", y)),
        #cex.lab = axislabelsize,
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE) 

mtext("A's Apples, x", side=1, line = 2.5, cex = axislabelsize)
text(-0.8, 7, expression(paste("A's Oranges, y")), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Add arrows:
arrows(-0.8, 10, -0.8, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(6.2, -1.7, 9, -1.7, xpd = TRUE, length=0.1,angle=40,lwd=3)


contour(x, y, 
        outer(x, y, uB),
        drawlabels = FALSE,
        col = COLB[2],
        lwd = graphlinewidth,
        levels = b, 
        add = TRUE
) 

segments(2.30769, 3.46154, 7.69231, 11.5385, lty = 1, col = COL[2] , lwd = graphlinewidth)
text(4.9, 2.4, expression("Pareto Efficient"))
text(4.9, 1.9, expression("Curve"))
Arrows(4.9, 2.7, 4.9, 6.9, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the iso-welfare functions for the HG, Aisha
text(9.35, 3.5, expression(v[1]^A))
text(9, 6, expression(v[2]^A))
text(8.5, 8.7, expression(v[3]^A))
text(8.2, 10, expression(v[4]^A))

#Label the indifference curves for the HG, Betty
text(0.55, 11.5, expression(v[1]^B))
text(0.95, 9, expression(v[2]^B))
text(1.5, 6.3, expression(v[3]^B))
text(1.7, 5, expression(v[4]^B))

#Label point i. 
points(5, 7.5, pch = 16, col = "black", cex = 1.5)
text(5, 7.9, expression(paste("i")))

#A's bliss point - 3.1073 is the value of u
points(x = 7.69231, y = 11.5385, pch = 16, col = "black", cex = 1.5)
text(7.69231, 14, expression(paste("A's highest v, ", v[max]^A)))
Arrows(7.69231, 13.7, 7.69231, 12.1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#B's bliss point: 
points(x = 2.30769, y = 3.461547, pch = 16, col = "black", cex = 1.5)
text(x = 2.30769, y = 0.9, expression(paste("B's highest v, ", v[max]^B)))
Arrows(2.30769, 1.2, 2.30769, 2.9, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)



par(new = TRUE)

#Use the same x and ylims as previously, but with locations switched
xlims2 <- c(10, 0)
ylims2 <- c(15, 0)

#Leave the ylab and xlab blank to ensure no axes titles
plot(0, 0, xlim = xlims2, ylim = ylims2, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")),
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = 1.3, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

#Set up axes at sides 3 and 4 (top and right)
axis(side = 3, at = ticksx, pos = 0, labels = xlabels)
axis(side = 4, at = ticksy, pos = 0, labels = ylabels, las = 0)
mtext("B's Apples, x", side=3, line = 2.5, cex = axislabelsize)
text(-0.8, 7, expression(paste("B's Oranges, y")), xpd = TRUE, cex = axislabelsize, srt = 270) 

#Add arrows:
arrows(-0.8, 10, -0.8, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(6.2, -1.8, 9, -1.8, xpd = TRUE, length=0.1,angle=40,lwd=3)

dev.off()
