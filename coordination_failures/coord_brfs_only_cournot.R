require(shape)
pdf(file = "coordination_failures/coord_brfs_only_cournot.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(4, 6, 1, 1))

uA <- function(ea, eb, alpha = 30, beta = 1/2) {
  (alpha - beta*(ea+eb))*ea - 0.5*(ea)^2
}

uB <- function(ea, eb, alpha = 30, beta = 1/2) {
  (alpha - beta*(ea+eb))*eb - 0.5*(eb)^2
}

brfB <- function(ea, alpha = 30, beta = 1/2) {
  (alpha - beta*ea)/(1 + 2*beta)
}

brfA <- function(ea, alpha = 30, beta = 1/2) {
  (alpha - ea*(1 + 2*beta))/(beta)
}

brfPE <- function(ea, alpha = 30, beta = 1/8) {
  alpha*(1 - beta*ea)
}

hANE <- function(alpha, beta = 1/2){
  alpha/(1 + 3*beta)
}

intercept1 <- function(alpha, beta = 1/2){
  alpha/(1 + 2*beta)
}

xlims <- c(0, 22)
ylims <- c(0, 22)

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
ticksy <- c(0, hANE(alpha = 30), intercept1(alpha = 30), ylims[2])
ylabels <- c(NA,  expression(paste(h^{BN})), expression(paste(frac(alpha, 1 + 2*beta))), NA)
ticksx <- c(0, hANE(alpha = 30), intercept1(alpha = 30), xlims[2])
xlabels <- c(NA, expression(paste(h^{AN})), expression(paste(frac(alpha, 1 + 2*beta))), NA)

axis(1, at = ticksx,  pos = 0, labels = FALSE)
text(x = c(0, hANE(alpha = 30), intercept1(alpha = 30), xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE)

axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(9.4, 10.55, length.out = npts)

#I need something like xx1 with npts for 
# xpoly1 <- seq(from = 8, to = 12, length.out = 500)
# ypoly1 <- indiffA(xpoly1, uA = 144, alpha = 30, beta = 1/2)
# ypoly2 <- indiffBroot1(xpoly1, uB = 144, alpha = 30, beta = 1/2)
# polygon(x = c(xpoly1, rev(xpoly1)), y = c(ypoly1, rev(ypoly2)), col=COL[4], density=NULL, border = NA)
# 


lines(xx1, brfA(xx1, alpha = 30, beta = 1/2), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfB(xx1, alpha = 30, beta = 1/2), col = COLB[4], lwd = graphlinewidth)
#lines(xx2, PEC(xx2, alpha = 30, beta = 1/2), col = COL[2], lwd = graphlinewidth)
#lines(xx1, PECroot2(xx1, alpha = 30, beta = 1/2), col = COLB[2], lwd = graphlinewidth)
#lines(xx1, indiffB(xx1, alpha = 30, beta = 1/2, uB = 42), col = COL[2], lwd = graphlinewidth)
#lines(xx1, indiffB2(xx1, alpha = 30, beta = 1/2, uB = 46.08), col = COL[2], lwd = graphlinewidth)
#lines(xx1, indiffB3(xx1, alpha = 30, beta = 1/2, uB = 46.08), col = COL[2], lwd = graphlinewidth)
#lines(xx1, indiffAlow(xx1, uA = 42, alpha = 30, beta = 1/2), col = COL[2], lwd = graphlinewidth)
#lines(xx1, indiffAgain(xx1, uA = 42, alpha = 30, beta = 1/2), col = COL[2], lwd = graphlinewidth)


#persp(x, y, outer(x, y, u), ticktype="detailed") 
# contour(y, x, 
#         outer(x, y, uA),
#         drawlabels = FALSE,
#         col = COLA[3],
#         lwd = graphlinewidth,
#         levels = a, 
#         xaxs="i", 
#         yaxs="i", 
#         add = TRUE) 

text(0.5*xlims[2], -1.9, expression(paste("A's hours, ", h^A)), xpd = TRUE, cex = axislabelsize)
text(-1.7, 9, expression(paste("B's hours, ", h^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Add arrows:
#arrows(-0.8, 10, -0.8, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
#arrows(6.2, -1.7, 9, -1.7, xpd = TRUE, length=0.1,angle=40,lwd=3)


# contour(x, y, 
#         outer(x, y, uB),
#         #labels = c("v1", "v2", "v3"),
#         drawlabels = FALSE,
#         col = COLB[2],
#         #xlab = expression(paste("")),
#         #ylab = expression(paste("")),
#         lwd = graphlinewidth,
#         levels = b, 
#         add = TRUE
#         #xaxs="i", 
#         #yaxs="i"
# ) 

segments(0, 12, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(12, 0, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)

# text(4, 10.2, expression("Pareto-efficient"))
# text(4, 9.6, expression("Curve"))
# Arrows(5.7, 10.1, 9.3, 10.1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the iso-welfare functions for the HG, Aisha
# text(5.3, 0.8, expression(u[1]^A == u[n]^A))
# text(7.2, 0.8, expression(u[2]^A))
# text(4.4, 0.8, expression(u[3]^A))
# text(5.2, 0.8, expression(u[4]^A))
# text(9.2, 0.8, expression(u[5]^A))

#Label the indifference curves for the HG, Betty
# text(7.8, 19, expression(u[1]^B == u[n]^B))
# text(4.9, 19, expression(u[2]^B))
# text(6.75, 17, expression(u[3]^B))
# text(6, 17, expression(u[4]^B))
# text(3.2, 17, expression(u[5]^B))

#Label Nash Equilibrium 
#segments(0, 9.6, 9.6, 9.6, lty = 2, col = "gray" , lwd = segmentlinewidth)
#segments(9.6, 0, 9.6, 9.6, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(hANE(alpha = 30), hANE(alpha = 30), pch = 16, col = "black", cex = 1.5)
text(hANE(alpha = 30) + 2.2, hANE(alpha = 30) + 0.4, expression(paste("Nash Equilibrium")))
text(hANE(alpha = 30) - 0.4, hANE(alpha = 30) - 0.4, expression(paste(n)))


#Annotate Pareto Efficient Curve and relevant points
# points(10.55, PEC(10.55), pch = 16, col = "black", cex = 1.5)
# text(10.55, PEC(10.55) - 0.5, expression(paste("g")))
# 
# 
# points(9.4, PEC(9.4), pch = 16, col = "black", cex = 1.5)
# text(9.4 - 0.5, PEC(9.4), expression(paste(f)))
# 
# points(hApEff2(alpha = 30), hApEff2(alpha = 30), pch = 16, col = "black", cex = 1.5)
# text(hApEff2(alpha = 30)- 0.3, hApEff2(alpha = 30) - 0.3, expression(paste(i)))

#points(5.84, 8.77, pch = 16, col = "black", cex = 1.5)

#B's brf
text(3, 17, expression(paste("B's best response")))
text(3, 16.3, expression(paste("function")))
text(3, 15.2, expression(paste(h^B*(h^A) == frac(alpha - beta*h^A, 1 + 2*beta) )))
#Arrows(2, 16.5, 2, 15.2, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#A's brf
text(16, 5, expression(paste("A's best response")))
text(16, 4.3, expression(paste("function")))
text(16, 3.2, expression(paste(h^A*(h^B) == frac(alpha - beta*h^B, 1 + 2*beta) )))
#Arrows(15.5, 3, 15.5, 1.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


# points(2.8, 11.2, pch = 16, col = "black", cex = 1.5)
# text(15.5, 4, expression(paste("A's best response")))
# 
# points(11.2, 2.8, pch = 16, col = "black", cex = 1.5)
# text(15.5, 4, expression(paste("A's best response")))

dev.off()
