require(shape)
pdf(file = "bfh-textbook/property/property_hg_he.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 3

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 4, 4, 4))

uA <- function(x, y) {
  0.5*log(x) + 0.5*log(y) + 0.35*log(10 - x) + 0.35*log(15 - y)
}

uB <- function(x, y) {
  0.5*log(10 - x) + 0.5*log(15 - y)
}

xlims <- c(0, 10)
ylims <- c(0, 15)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(2.2, 2.526628, 2.9, 3.1)
b <- c(1.2, 1.618586, 2.08, 2.275)

#For B we need a utility where xA = 5.88, ya = 8.82 => corresponds to 1.618 above

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

contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLA[3],
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

segments(0, 0, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
text(5.5, 6.4, expression("Pareto Efficient Curve"))
Arrows(5.3, 6.7, 5.3, 7.6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the iso-welfare functions for the HG, Aisha
text(9.8, 2.9, expression(v[1]^A))
text(9.35, 3.5, expression(v[2]^A))
text(8.3, 5.2, expression(v[3]^A))
text(6.6, 8.3, expression(v[4]^A))

#Label the indifference curves for the HE, Betty
text(0.3, 13.3, expression(u[1]^B))
text(0.3, 11.8, expression(u[2]^B))
text(0.3, 7.8, expression(u[3]^B))
text(0.3, 4.6, expression(u[4]^B))

#A's bliss point - 3.1073 is the value of u
points(5.88, 8.82, pch = 16, col = "black", cex = 1.5)
text(5.88, 11, expression(paste("A's highest v, ", v[max]^A)))
Arrows(5.88, 10.6, 5.88, 9.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label point i. 
points(5, 7.5, pch = 16, col = "black", cex = 1.5)
text(5, 7.9, expression(paste("i")))

#Label two points for comparison
points(2.05, 3.075, pch = 16, col = "black", cex = 1.5)
text(2.05, 3.55, expression(paste("j")))
points(9, 13.5, pch = 16, col = "black", cex = 1.5)
text(9, 13, expression(paste("k")))

#Label two points for comparison
points(8, 2, pch = 16, col = "black", cex = 1.5)
text(8, 1.5, expression(paste("e")))



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
