#Graph Designer: Simon Halliday & Madeleine Wettach '20
#Authors: Bowles, Foley and Halliday
#Title: Microeconomics: Competition, Conflict and Coordination

require(shape)
pdf(file = "competitionmarkets/cournot_brfs_social.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(5, 5, 4, 4))

socialW <- function(xa, s = 0.5, pmax = 20, c1 = 2, slope = 1) {
  (pmax - c1)/s - slope*xa
}

piA <- function(xa, xb, s = 0.5, pmax = 20, c1 = 2) {
  (pmax - s*xb)*xa - s*(xa)^2 - c1*xa
}

piB <- function(xa, xb, s = 0.5, pmax = 20, c1 = 2) {
  (pmax - s*xa)*xb - s*(xb)^2 - c1*xb
}

brfB <- function(xa, s = 0.5, pmax = 20, c1 = 2) {
  (pmax - c1)/(2*s) - (1/2)*xa
}

brfA <- function(xa, s = 0.5, pmax = 20, c1 = 2) {
  (pmax - c1)/s - 2*xa
}

xANE <- function(pbar, beta = 2, c1 = 1){
  (pbar - c1)/(3*beta)
}

isoA <- function(xa, s = 0.5, pmax = 20, c1 = 2, piA = 72){
  -(c1*xa - pmax*xa + s*xa^2 + piA)/(s*xa)
}

isoB1 <- function(xa, s = 0.5, pmax = 20, c1 = 2, piB = 72){
  (-sqrt((c1 - pmax + s*xa)^2 - 4*piB*s) - c1 + pmax -s*xa)/(2*s)
}

isoB2 <- function(xa, s = 0.5, pmax = 20, c1 = 2, piB = 72){
  (sqrt((c1 - pmax + s*xa)^2 - 4*piB*s) - c1 + pmax -s*xa)/(2*s)
}


#Input into Wolfram Alpha: solve for x y = 1/b - (0.5*x)/(a*b) - u/(a*b*x)

#solve for y 72 = (p - s*y)*x - s*(x)^2 - c1*x 
#solve for y 72 = (p - s*x)*y - s*(y)^2 - c1*y

xlims <- c(0, 36)
ylims <- c(0, 36)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(72, 81)
b <- c(72, 81)

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
ticksy <- c(0, 9, 12, 18, 36, ylims[2])
ylabels <- c(NA, expression(paste(x^{B},"*")), expression(paste(x^{BN})), expression(paste( frac(bar(p) - c[1],2*beta) )), expression(paste( frac(bar(p) - c[1],beta) )), NA)
ticksx <- c(0, 9, 12, 18, 36, xlims[2])
xlabels <- c(NA, expression(paste(x^{A},"*")), expression(paste(x^{AN})), expression(paste(frac(bar(p) - c[1],2*beta))), expression(paste( frac(bar(p) - c[1],beta) )), NA)


axis(1, at = ticksx, pos = 0, labels = FALSE)
text(x = c(0, 9, 12, 18, 36, xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Pareto-improving lens
# xpoly1 <- seq(from = 5.95, to = 12, length.out = 500)
# ypoly1 <- isoA(xpoly1)
# ypoly2 <- isoB1(xpoly1)
# polygon(x = c(xpoly1, rev(xpoly1)), y = c(ypoly1, rev(ypoly2)), col=COL[4], density=NULL, border = NA)


lines(xx1, brfA(xx1, s = 0.5, pmax = 20, c1 = 2), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfB(xx1, s = 0.5, pmax = 20, c1 = 2), col = COLB[4], lwd = graphlinewidth)
lines(xx1, socialW(xx1, s = 0.5, pmax = 20, c1 = 2), col = COL[2], lwd = graphlinewidth)


# contour(y, x, 
#         outer(x, y, piA),
#         #labels = c("v1", "v2", "v3"),
#         drawlabels = FALSE,
#         col = COLA[3],
#         #xlab = expression(paste("A's Apples, ", x)),
#         #ylab = expression(paste("A's Oranges, ", y)),
#         #cex.lab = axislabelsize,
#         lwd = segmentlinewidth,
#         levels = a, 
#         xaxs="i", 
#         yaxs="i", 
#         add = TRUE) 

text(0.5*(xlims[2]), -5, expression(paste("A's output, ", x^A)), xpd = TRUE, cex = axislabelsize) 
text(-4, 0.5*ylims[2], expression(paste("B's output, ", x^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Add arrows:
#arrows(-0.8, 10, -0.8, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
#arrows(6.2, -1.7, 9, -1.7, xpd = TRUE, length=0.1,angle=40,lwd=3)


# contour(x, y, 
#         outer(x, y, piB),
#         #labels = c("v1", "v2", "v3"),
#         drawlabels = FALSE,
#         col = COLB[2],
#         #xlab = expression(paste("")),
#         #ylab = expression(paste("")),
#         lwd = segmentlinewidth,
#         levels = b, 
#         add = TRUE
#         #xaxs="i", 
#         #yaxs="i"
# ) 


#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
# text(7.3, 3, expression("Pareto Efficient"))
# text(7.3, 2.4, expression("Curve"))
# Arrows(7.3, 3.5, 7.3, 6.1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the iso-welfare functions for the HG, Aisha
# text(4.1, 1.5, expression(pi[1]^A))
# text(6.2, 1.5, expression(pi[2]^A))
#text(6, 1.5, expression(pi[3]^A))
#text(7, 1.5, expression(pi[Opp]^A))
#text(6.6, 8.3, expression(u[4]^A))

#Label the indifference curves for the HG, Betty
#text(11.8, 18, expression(pi[Vic]^B))
# text(8.5, 22, expression(pi[1]^B))
# text(5.6, 22, expression(pi[2]^B))
#text(8.4, 18, expression(pi[3]^B))
#text(3.4, 6.9, expression(v[4]^B))

#Label Nash Equilibrium 
segments(0, 12, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(12, 0, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(12, 12, pch = 16, col = "black", cex = 1.5)
#text(14, 12.4, expression(paste("Nash Equilibrium")))
text(12.5, 12.5, expression(paste(n)))

#Checking points
#points(5.95, 5.95, pch = 16, col = "black", cex = 1.5)
#segments(8, 10, 10, 8, lty = 1, col = COL[2] , lwd = graphlinewidth)

# 
# text(9, 9.9, expression(paste(i)))
# segments(0, 9, 9, 9, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(9, 0, 9, 9, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(13.5, 0, 13.5, 9, lty = 2, col = "gray" , lwd = segmentlinewidth)
# points(9, 9, pch = 16, col = "black", cex = 1.5)
# points(13.5, 9, pch = 16, col = "black", cex = 1.5)
# text(13.45, 9.6, expression(paste("d")))


# points(10, 8, pch = 16, col = "black", cex = 1.5)
# text(10, 8.6, expression(paste("f")))
# points(8, 10, pch = 16, col = "black", cex = 1.5)
# text(8, 10.6, expression(paste("g")))


# #Annotate Pareto Efficient Curve and relevant points

# points(6, 8, pch = 16, col = "black", cex = 1.5)
# text(6, 8.5, expression(paste("g")))
# 
# points(8, 6, pch = 16, col = "black", cex = 1.5)
# text(7, 7.5, expression(paste("i")))
# 
# points(7, 7, pch = 16, col = "black", cex = 1.5)
# text(8, 6.5, expression(paste("f")))

segments(0, 18, 18, 18, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(18, 0, 18, 18, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(18, 18, pch = 16, col = "black", cex = 1.5)
text(18.5, 18.5, expression(paste(j)))

#B's brf
text(32, 12, expression(paste("B's best response")))
text(32, 11, expression(paste("function")))
Arrows(32, 10, 32, 2.9, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#A's brf
text(6, 35, expression(paste("A's best response")))
text(6, 34, expression(paste("function")))
Arrows(6, 33, 6, 25, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Arrow for owners and consumers
Arrows(23, 21, 23, 14, lty = 1, lwd = 2, arr.type = "triangle",  col = "black")
text(23, 23, expression(paste("Efficient output")), cex = labelsize)
text(23, 22, expression(paste("for consumers")), cex = labelsize)
# 
# Arrows(12, 8, 9.5, 5.5, lty = 1, lwd = 2, arr.type = "triangle",  col = "#f0027f")
# text(10.5, 5, expression(paste("Better")), cex = labelsize)
# text(10.5, 4, expression(paste("for")), cex = labelsize)
# text(10.5, 3, expression(paste("owners")), cex = labelsize)

#f0027f
#e41a1c
dev.off()
