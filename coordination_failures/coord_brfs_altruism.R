require(shape)
pdf(file = "coordination_failures/coord_brfs_altruism.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(5, 5, 4, 4))

uA <- function(ea, eb, alpha = 16, beta = 1/24) {
  alpha*(1 - beta*eb)*ea - 0.5*(ea)^2
}

uB <- function(ea, eb, alpha = 16, beta = 1/24) {
  alpha*(1 - beta*ea)*eb - 0.5*(eb)^2
}

brfB <- function(ea, alpha = 16, beta = 1/24) {
  alpha*(1 - beta*ea)
}

brfA <- function(ea, alpha = 16, beta = 1/24) {
  (alpha - ea)/(alpha * beta)
}

brfPEA <- function(ea, alpha = 16, beta = 1/12) {
  (alpha - ea)/(alpha * 2*beta)
}

brfPEB <- function(eb, alpha = 16, beta = 1/12) {
  alpha*(1 - beta*eb*2)
}

indiffeA <- function(ea, alpha = 16, beta = 1/24, uA = 46.08) {
  1/beta - (0.5*ea)/(alpha*beta) - uA/(alpha*beta*ea)
}



#Input into Wolfram Alpha: solve for x y = 1/b - (0.5*x)/(a*b) - u/(a*b*x)

# indiffAlowB <- function(eb, alpha = 16, beta = 1/24, uA = 46.08) {
#   0.5*(-sqrt((2*alpha*beta*eb - 2*alpha)^2 - 8*uA)) - 2*alpha*beta*eb + 2*alpha
# }
# 
# indiffAlow <- function(ea, alpha = 16, beta = 1/24, uA = 46.08) {
#   0.5*(-sqrt((2*alpha*beta*ea - 2*alpha)^2 - 8*uA)) - 2*alpha*beta*ea + 2*alpha
# }
# 
# indiffAgain <- function(ea, alpha = 16, beta = 1/24, uA = 46.08) {
#   (-2*alpha*ea +2*uA + ea^2 )/ ( 2 * alpha * beta * ea)
# }

xlims <- c(0, 26)
ylims <- c(0, 26)

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

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
# ticksx <- seq(from = 0, to = xlims[2], by = 2)
# xlabels <- seq(from = 0, to = xlims[2], by = 2)
# ticksy <- c(0, 6.9, 12, 9.6, 16, 24, 26)
# ylabels <- c(NA, expression(paste(e^B,"*")), expression(paste(1/(1 + 2*beta) )), expression(paste(e^{BN})), expression(paste(bar(y) )), expression(paste(1/(1 + 3*beta) )), NA)
# ticksx <- c(0, 6.9, 12, 9.6, 16, 24, 26)
# xlabels <- c(NA, expression(paste(e^A,"*")), expression(paste(1/(1 + 2*beta) )), expression(paste(e^{AN})), expression(paste(bar(y) )), expression(paste(1/(1 + 3*beta) )), NA)
ticksy <- c(0, 6.9, 12, 9.6, 16, 24, 26)
ylabels <- c(NA, expression(paste(e^B,"*")), NA, expression(paste(e^{BN})), NA, NA, NA)
ticksx <- c(0, 6.9, 12, 9.6, 16, 24, 26)
xlabels <- c(NA, expression(paste(e^A,"*")), NA, expression(paste(e^{AN})), NA, NA, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, brfA(xx1, alpha = 16, beta = 1/24), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfB(xx1, alpha = 16, beta = 1/24), col = COLB[4], lwd = graphlinewidth)
lines(xx1, brfPEA(xx1, alpha = 16, beta = 1/24), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfPEB(xx1, alpha = 16, beta = 1/24), col = COLB[4], lwd = graphlinewidth)

segments(0, 9.6, 9.6, 9.6, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(9.6, 0, 9.6, 9.6, lty = 2, col = "gray" , lwd = segmentlinewidth)

segments(0, 6.9, 6.9, 6.9, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(6.9, 0, 6.9, 6.9, lty = 2, col = "gray" , lwd = segmentlinewidth)

mtext(expression(paste("A's hours of work,", h^A)), side=1, line = 2.5, cex = axislabelsize)
text(-2.8, 0.5*ylims[2], expression(paste("B's hours of work,", h^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
#text(7.3, 3, expression("Pareto Efficient"))
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

#Label point i. 
points(9.6, 9.6, pch = 16, col = "black", cex = 1.5)
text(12, 10.1, expression(paste("Nash Equilibrium")))
text(9.2, 9.2, expression(paste("n")))

#Annotate Pareto Efficient Curve and relevant points
# segments(8, 6, 6, 8, lty = 1, col = COL[2] , lwd = graphlinewidth)
# points(6, 8, pch = 16, col = "black", cex = 1.5)
# text(6, 8.5, expression(paste("g")))
# 
points(6.9, 6.9, pch = 16, col = "black", cex = 1.5)
text(7.1, 7.5, expression(paste("i")))
# 
# points(7, 7, pch = 16, col = "black", cex = 1.5)
# text(8, 6.5, expression(paste("f")))

#points(5.84, 8.77, pch = 16, col = "black", cex = 1.5)

#B's brf
text(22, 7.8, expression(paste("B's original")))
text(22, 7, expression(paste("best-response")))
text(22, 6.25, expression(paste("function")))
Arrows(22, 5.75, 22, 2.25, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#A's brf
text(4, 24.8, expression(paste("A's original")))
text(4, 24, expression(paste("best-response")))
text(4, 23.25, expression(paste("function")))
Arrows(4, 22.5, 4, 19, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#A's PE Response
text(2.5, 5, expression(paste("A's Altruistic")))
text(2.5, 4.2, expression(paste("response")))
Arrows(7, 1.5, 10.25, 1.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#B's PE Response
text(4.5, 1.5, expression(paste("B's Altruistic")))
text(4.5, 0.7, expression(paste("response")))
Arrows(3, 5.5, 3, 8.75, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5) 
# #Use the same x and ylims as previously, but with locations switched
# xlims2 <- c(18, 0)
# ylims2 <- c(18, 0)

#Leave the ylab and xlab blank to ensure no axes titles
# plot(0, 0, xlim = xlims2, ylim = ylims2, type = "n",
#      xlab = expression(paste("")),
#      ylab = expression(paste("")),
#      xaxt = "n", 
#      yaxt = "n", 
#      cex.lab = 1.3, 
#      bty = "n",
#      xaxs="i", 
#      yaxs="i")
# 
# 
# lines(xx1, indiffAlow(xx1, uA = 46.08, alpha = 16, beta = 1/24), col = COL[2], lwd = graphlinewidth)


#Set up axes at sides 3 and 4 (top and right)
# axis(side = 3, at = ticksx, pos = 0, labels = xlabels)
# axis(side = 4, at = ticksy, pos = 0, labels = ylabels, las = 0)
# mtext("B's Apples, x", side=3, line = 2.5, cex = axislabelsize)
# text(-0.8, 7, expression(paste("B's Oranges, y")), xpd = TRUE, cex = axislabelsize, srt = 270) 
# 
# #Add arrows:
# arrows(-0.8, 10, -0.8, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
# arrows(6.2, -1.8, 9, -1.8, xpd = TRUE, length=0.1,angle=40,lwd=3)
# 

#Customize ticks and labels for the plot
#ticksy <- seq(from = 0, to = 15, by = 1)
#ylabels <- seq(from = 0, to = 15, by = 1)
#ticksx <- seq(from = 0, to = 10, by = 1)
#xlabels <- seq(from = 0, to = 10, by = 1)
#axis(1, at = ticksx, pos = 0, labels = xlabels)
#axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0)

dev.off()
