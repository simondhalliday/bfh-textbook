require(shape)
pdf(file = "competitionmarkets/cournot_brf_isoA.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 6, 4, 4))

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




xlims <- c(0, 36)
ylims <- c(0, 36)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(42, 72, 102)
b <- c(72, 81, 90)


#B's value when at A's bliss point
#0.35*log(5.88) + 0.35*log(8.88) + 0.5*log(10 - 5.88) + 0.5*log(15 - 8.88) 

#B's bliss point x = 4.11765; y = 6.17647
#A's value when at A's bliss point
#0.5*log(4.11765) + 0.5*log(6.17647) + 0.35*log(10 - 4.11765) + 0.35*log(15 - 6.17647) 


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
ticksy <- c(0, 7.5, 12, 17.5, ylims[2])
ylabels <- c(NA, expression(paste(x[3]^B)), expression(paste(x[2]^B)), expression(paste(x[1]^B)), expression(paste((p[max] - c[1])/s)))
ticksx <- c(0, 9.2, 12, 14.25, 18, xlims[2])
xlabels <- c(NA, expression(paste(x[1]^A)), expression(paste(x[2]^A)), expression(paste(x[3]^A)), expression(paste((p[max] - c[1])/2*s)), NA)


axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, brfA(xx1, s = 0.5, pmax = 20, c1 = 2), col = COLA[4], lwd = graphlinewidth)


#persp(x, y, outer(x, y, u), ticktype="detailed") 
contour(x, y, 
        outer(x, y, piA),
        #labels = c("v1", "v2", "v3"),
        drawlabels = FALSE,
        col = COLA[2],
        #xlab = expression(paste("A's Apples, ", x)),
        #ylab = expression(paste("A's Oranges, ", y)),
        #cex.lab = axislabelsize,
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE) 

mtext(expression(paste("A's output, ", x^A)), side=1, line = 2.5, cex = axislabelsize)
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
#         lwd = graphlinewidth,
#         levels = b,
#         add = TRUE
#         #xaxs="i",
#         #yaxs="i"
# )


#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
# text(7.3, 3, expression("Pareto Efficient"))
# text(7.3, 2.4, expression("Curve"))
# Arrows(7.3, 3.5, 7.3, 6.1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


segments(0, 12, 14, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(12, 0, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(12, 12, pch = 16, col = "black", cex = 1.5)
segments(0, 7.5, 16.25, 7.5, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(14.25, 0, 14.25, 7.5, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(14.25, 7.5, pch = 16, col = "black", cex = 1.5)
segments(0, 17.7, 11.2, 17.7, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(9.2, 0, 9.2, 17.7, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(9.2, 17.7, pch = 16, col = "black", cex = 1.5)

text(14.25, 22, expression(paste("Iso-profit horizontal")))
text(14.25, 20.8, expression(paste("at intersection with")))
text(14.25, 19.6, expression(paste("best-response function")))
Arrows(14.25, 19, 14.25, 9, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)



#Label the iso-welfare functions for the HG, Aisha
text(3.2, 1.5, expression(pi[1]^A))
text(5.5, 1.5, expression(pi[2]^A))
text(8.2, 1.5, expression(pi[3]^A))
# text(31, 1.5, expression(pi[1]^A))
# text(28, 1.5, expression(pi[2]^A))
# text(25, 1.5, expression(pi[3]^A))
text(30, 8, expression("A's iso-profit curves"))
text(30, 6.7, expression(paste(pi[3]^A > pi[2]^A, phantom() > pi[1]^A)))
#text(6.6, 8.3, expression(u[4]^A))

#Label the indifference curves for the HG, Betty
# text(8.9, 19, expression(pi[1]^B))
# text(7.9, 19, expression(pi[2]^B))
# text(7, 19, expression(pi[3]^B))
#text(3.4, 6.9, expression(v[4]^B))

# #Label Nash Equilibrium 
# segments(0, 12, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(12, 0, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)
# points(12, 12, pch = 16, col = "black", cex = 1.5)
# text(14, 12.4, expression(paste("Nash Equilibrium")))
# text(11.5, 11.5, expression(paste("n")))


# #Annotate Pareto Efficient Curve and relevant points
# segments(8, 6, 6, 8, lty = 1, col = COL[2] , lwd = graphlinewidth)
# points(6, 8, pch = 16, col = "black", cex = 1.5)
# text(6, 8.5, expression(paste("g")))
# 
# points(8, 6, pch = 16, col = "black", cex = 1.5)
# text(7, 7.5, expression(paste("i")))
# 
# points(7, 7, pch = 16, col = "black", cex = 1.5)
# text(8, 6.5, expression(paste("f")))

#points(5.84, 8.77, pch = 16, col = "black", cex = 1.5)

#B's brf
text(7, 30, expression(paste("A's best-response")))
text(7, 29, expression(paste("function")))
Arrows(7, 28.2, 7, 23.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


dev.off()
