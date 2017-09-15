require(shape)
library(extrafont)
library(pBrackets)
pdf(file = "what_can_markets_do/edgeworthbox_simulation_path2.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 4, 4, 4))

uA <- function(x, y, rmax = 100, xmax = 10) {
  y + rmax*x - (1/2)*(rmax/xmax)*x^2
}

indiffA <- function(x, utility = uA(x = 1, y = 400), rmax = 100, xmax = 10) {
  utility - rmax*x + (1/2)*(rmax/xmax)*x^2
}

mrsA <- function(x, rmax = 100, xmax = 10) {
  rmax - (rmax/xmax)*x
}

uB <- function(x, y, rmax = 100, xmax = 10) {
  (400 - y) + rmax*(10 - x) - (1/2)*(rmax/xmax)*(10 - x)^2
}

indiffB <- function(x, utility = uB(x = 9, y = 0), rmax = 100, xmax = 10) {
  utility + rmax*(x) - (1/2)*(rmax/xmax)*(x)^2
}

WalrasP <- function(x, intercept = 450, pw = mrsA(x = 5)) {
  intercept - pw*x
}

xlims <- c(0, 10)
ylims <- c(0, 400)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(uA(x = 9, y = 0), uA(x = 5, y = 365))
b <- c(uB(x = 1, y = 400), uB(x = 5, y = 200), 655)

#B's value when at A's bliss point
#0.35*log(5.88) + 0.35*log(8.88) + 0.5*log(10 - 5.88) + 0.5*log(15 - 8.88) 

#B's bliss point x = 4.11765; y = 6.17647
#A's value when at A's bliss point
#0.5*log(4.11765) + 0.5*log(6.17647) + 0.35*log(10 - 4.11765) + 0.35*log(15 - 6.17647) 

#Use the same x and ylims as previously, but with locations switched
xlims2 <- c(10, 0)
ylims2 <- c(400, 0)

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
#Pareto-improving lens
# xpoly1 <- seq(from = 1, to = 9, length.out = 500)
# ypoly1 <- indiffA(xpoly1)
# ypoly2 <- WalrasP(xpoly1)
# polygon(x = c(xpoly1, rev(xpoly1)), y = c(ypoly1, rev(ypoly2)), col = COL[4], density = NULL, border = NA)

par(new = TRUE)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

#Pareto-improving lens
# xpoly1 <- seq(from = 1, to = 9, length.out = 500)
# ypoly1 <- indiffA(xpoly1)
# ypoly2 <- WalrasP(xpoly1)
# polygon(x = c(xpoly1, rev(xpoly1)), y = c(ypoly1, rev(ypoly2)), col = COL[4], density = NULL, border = NA)

#Pareto-improving lens
# xpoly1 <- seq(from = 1, to = 9, length.out = 500)
# ypoly1 <- indiffA(xpoly1)
# ypoly2 <- WalrasP(xpoly1)
# polygon(x = c(xpoly1, rev(xpoly1)), y = c(ypoly1, rev(ypoly2)), col = COL[4], density = NULL, border = NA)


ticksy <- seq(from = 0, to = 400, by = 40)
ylabels <- seq(from = 0, to = 400, by = 40)
ticksx <- seq(from = 0, to = 10, by = 1)
xlabels <- seq(from = 0, to = 10, by = 1)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0)

a2 <- c(uA(8.1, 43), uA(6.55, 99), uA(5.8, 170), uA(5.34, 255)) 

contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLA[3],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE) 

contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLA[3],
        lwd = segmentlinewidth,
        levels = a2, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE,
        lty = 2) 

mtext("A's Good, x", side=1, line = 2.5, cex = axislabelsize)
text(-0.8, 0.5*ylims[2], expression(paste("A's Money, y")), xpd = TRUE, cex = axislabelsize, srt = 90) 

xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Add arrows:
arrows(-0.8, 260, -0.8, 360, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)
arrows(6.2, -45, 9, -45, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)

xx2 <- seq(1, 10, length.out = npts)
#lines(xx2, WalrasP(xx2), col = "gray", lwd = segmentlinewidth)
#lines(xx2, WalrasP(xx2, intercept = 9.4), col = "purple", lwd = segmentlinewidth, lty = 1)

# contour(x, y, 
#         outer(x, y, uB),
#         drawlabels = FALSE,
#         col = COLB[2],
#         lwd = graphlinewidth,
#         levels = b, 
#         add = TRUE
# ) 

# segments(5, 3.95, 5, 6.05, lty = 1, col = COL[2] , lwd = graphlinewidth)
# segments(5, 0, 5, 3.95, col = COL[2] , lwd = segmentlinewidth, lty = 2)
# segments(5, 6.05, 5, 10, col = COL[2] , lwd = segmentlinewidth, lty = 2)
# 
segments(5, 0, 5, 120, col = "purple" , lwd = segmentlinewidth, lty = 2)
segments(5, 120, 5, 280, col = "purple" , lwd = segmentlinewidth, lty = 1)
segments(5, 280, 5, ylims[2], col = "purple", lwd = segmentlinewidth, lty = 2)
# 
# 
# #Label the PEC
text(2, 44, expression("Pareto Efficient Curve"))
Arrows(3.1, 44, 4.8, 44, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#A path
Arrows(9, 5, 8.25, 38, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.3, arr.length = 0.3)
Arrows(8, 45, 6.72, 93, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.3, arr.length = 0.3)
Arrows(6.48, 105, 5.89, 160, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.3, arr.length = 0.3)
Arrows(5.75, 175, 5.4, 245, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.3, arr.length = 0.3)
Arrows(5.33, 261, 5.1, 350, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Trades
points(8.1, 43, pch = 1, col = "black", cex = 1)
points(6.55, 99, pch = 1, col = "black", cex = 1)
points(5.8, 170, pch = 1, col = "black", cex = 1)
points(5.34, 255, pch = 1, col = "black", cex = 1)
points(5, 365, pch = 16, col = "black", cex = 1.5)
text(5.2, 370, expression(paste(H)))

# #Label the walrasian P
# 
# Arrows(2.25, 5.2, 2.25, 6.85, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# text(2.25, 5, expression(slope == -p[n] ))
# 
# #Label the iso-welfare functions for the HG, Aisha
#text(8.2, 25, expression(u[1]^A))
text(7.5, 11, expression(u[e]^A))
text(7.5, 255, expression(u[H]^A))

# 
# #Label the indifference curves for the HG, Betty
# text(1.8, 375, expression(u[1]^B))
# text(1.8, 295, expression(u[2]^B))
# text(1.8, 215, expression(u[3]^B))

# 
#Label point f.
# points(5, 120, pch = 16, col = "black", cex = 1.5)
# text(4.8, 110, expression(paste(f)))

#Label point g.
#points(5, 158, pch = 16, col = "black", cex = 1.5)
# text(5.2, 290, expression(paste(g)))
# 
# #Initial Allocations
points(x = 9, y = 0, pch = 16, col = "black", cex = 1.5)
text(9, 12, expression(paste(e)))

# points(x = 5, y = 200, pch = 16, col = "black", cex = 1.5)
# text(4.9, 190, expression(paste(n)))



#Braces for labels
# brackets(x1 = 8.9, y1 = -0.3, x2 = 5, y2 = -0.3,  
#          ticks = 0.5, curvature = 0.5, type = 1, 
#          col = "black", lwd = 2, lty = 1, xpd = TRUE)
# text(7, -1, expression(paste("Quantity of the good, x")), xpd = TRUE)
# text(7, -1.4, expression(paste("A sells to B")), xpd = TRUE)

# brackets(x1 = 10.2, y1 = 5, x2 = 10.2, y2 = 1.1,  
#          ticks = 0.5, curvature = 0.5, type = 1, 
#          col = "black", lwd = 2, lty = 1, xpd = TRUE)
# text(11.2, 3, expression(paste("Quantity of money, y")), xpd = TRUE, srt = 270)
# text(10.9, 3, expression(paste("B pays A")), xpd = TRUE, srt = 270)

par(new = TRUE)

#Use the same x and ylims as previously, but with locations switched
xlims2 <- c(10, 0)
ylims2 <- c(400, 0)

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
mtext("B's Good, x", side = 3, line = 2.5, cex = axislabelsize)
text(-0.8, 0.5*ylims[2], expression(paste("B's Money, y")), xpd = TRUE, cex = axislabelsize, srt = 270) 

#Add arrows:
arrows(-0.8, 260, -0.8, 360, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)
arrows(6.2, -50, 9, -50, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)


points(x = 5, y = 200, pch = 16, col = "black", cex = 1.5)
text(4.9, 190, expression(paste(n)))

dev.off()

