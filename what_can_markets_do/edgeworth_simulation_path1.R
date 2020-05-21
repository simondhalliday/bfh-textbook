require(shape)
library(extrafont)
library(pBrackets)
pdf(file = "what_can_markets_do/edgeworthbox_simulation_path1.pdf", width = 9, height = 7)

#Set parameters for graphics
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
grays <- gray.colors(25, start = 1, end = 0)

par(mar =  c(6, 5, 5, 5))

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
a <- c(uA(x = 9, y = 0), uA(x = 5, y = 158))
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


ticksy <- seq(from = 0, to = 400, by = 50)
ylabels <- seq(from = 0, to = 400, by = 50)
ticksx <- seq(from = 0, to = 10, by = 1)
xlabels <- seq(from = 0, to = 10, by = 1)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = axislabelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0, cex.axis = axislabelsize)

contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLA[3],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE) 

mtext("A's Good, x", side=1, line = 3.3, cex = axislabelsize)
text(-1.1, 0.5*ylims[2], expression(paste("A's Money, y")), xpd = TRUE, cex = axislabelsize, srt = 90) 

xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Add arrows:
arrows(-1.1, 272, -1.1, 380, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)
arrows(6.5, -60, 9, -60, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)

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
segments(5, 0, 5, 120, col = COL[2], lwd = segmentlinewidth, lty = 2)
segments(5, 120, 5, 280, col = COL[2] , lwd = segmentlinewidth, lty = 1)
segments(5, 280, 5, ylims[2], col = COL[2], lwd = segmentlinewidth, lty = 2)
# 
# 
# #Label the PEC
text(2, 44, expression("Pareto-efficient"), cex = annotatesize)
text(2, 29, expression("curve"), cex = annotatesize)
Arrows(3.3, 44, 4.8, 44, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#A path
Arrows(9, 5, 8.25, 15, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.3, arr.length = 0.3)
Arrows(8.02, 18, 6.7, 57, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.3, arr.length = 0.3)
Arrows(6.46, 64, 5.95, 86, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.3, arr.length = 0.3)
Arrows(5.75, 95, 5.45, 112, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.3, arr.length = 0.3)
Arrows(5.28, 122, 5.1, 145, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.3, arr.length = 0.3)

#Trades
points(8.1, 17, pch = 1, col = "black", cex = 1.5)
points(6.55, 62, pch = 1, col = "black", cex = 1.5)
points(5.82, 93, pch = 1, col = "black", cex = 1.5)
points(5.34, 119, pch = 1, col = "black", cex = 1.5)


# #Label the walrasian P
# 
# Arrows(2.25, 5.2, 2.25, 6.85, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# text(2.25, 5, expression(slope == -p[n] ))
# 
# #Label the iso-welfare functions for the HG, Aisha
text(6, 50, expression(u[e]^A), cex = labelsize)
text(6, 130, expression(u[L]^A), cex = labelsize)
#text(8.2, 185, expression(u[3]^A))
# 
# #Label the indifference curves for the HG, Betty
# text(1.8, 375, expression(u[1]^B))
# text(1.8, 295, expression(u[2]^B))
# text(1.8, 215, expression(u[3]^B))

# 
#Label point f.
# points(5, 120, pch = 16, col = "black", cex = 1.5)
# text(4.8, 110, expression(paste(f)))

#Label point L.
points(5, 158, pch = 16, col = "black", cex = 1.5)
text(5.2, 168, expression(paste(L)), cex = labelsize)
# 
# #Initial Allocations
points(x = 9, y = 0, pch = 16, col = "black", cex = 1.5)
text(9, 11, expression(paste(z)), cex = labelsize)

points(x = 5, y = 200, pch = 16, col = "black", cex = 1.5)
text(5.2, 210, expression(paste(n)), cex = labelsize)



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
axis(side = 3, at = ticksx, pos = 0, labels = xlabels, cex.axis = axislabelsize)
axis(side = 4, at = ticksy, pos = 0, labels = ylabels, las = 0, cex.axis = axislabelsize)
mtext("B's Good, x", side = 3, line = 3, cex = axislabelsize)
text(-0.9, 0.5*ylims[2], expression(paste("B's Money, y")), xpd = TRUE, cex = axislabelsize, srt = 270) 

#Add arrows:
arrows(-0.9, 275, -0.9, 380, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)
arrows(6.5, -60, 9, -60, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)


#Customize ticks and labels for the plot
#ticksy <- seq(from = 0, to = 15, by = 1)
#ylabels <- seq(from = 0, to = 15, by = 1)
#ticksx <- seq(from = 0, to = 10, by = 1)
#xlabels <- seq(from = 0, to = 10, by = 1)
#axis(1, at = ticksx, pos = 0, labels = xlabels)
#axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0)

dev.off()

