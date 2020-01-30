#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
#pdf(file = "indiff_shaded_newSTEP1.pdf", width = 8, height = 6)
#pdf(file = "indiff_shaded_newSTEP2.pdf", width = 8, height = 6)
pdf(file = "constrained_optimization/living_mufn.pdf", width = 8, height = 6)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 1, 1))

uFn <- function(x, y = 3, alpha = 0.4){
  (x^alpha)*(y^(1 - alpha))
}

muFn <- function(x, y = 3, alpha = 0.4){
  alpha*(x^(alpha - 1))*(y^(1 - alpha))
}


indiffA <- function(x, alpha = 0.5, uA = 5) {
  (uA / x^alpha)^(1/(1 - alpha))
}

xlims <- c(0, 16)
ylims <- c(0, 1)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(2, 4, 6)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i"
)


ticksy <- seq(from = 0, to = ylims[2], by = 1)
ylabels <- seq(from = 0, to = ylims[2], by = 1)
ticksx <- seq(from = 0, to = xlims[2], by = 1)
xlabels <- seq(from = 0, to = xlims[2], by = 1)
# ticksy <- c(ylims[1], 5.25, 8, ylims[2])
# ylabels <- c(NA, expression(paste(y,"*")), expression(paste(bar(y))), NA)
# ticksx <- c(xlims[1], 5.25, 8.944272, xlims[2])
# xlabels <- c(NA, expression(paste(x,"*")), expression(paste(bar(x))), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)

#Draw the polygon for shading the utility areas
# xpoly1 <- seq(from = xlims[1], to = xlims[2], length.out = 500)
# ypoly1 <- indiffA(xpoly1, alpha = 0.5, uA = 4)
# polygon(x = c(xpoly1, rev(xpoly1[1])), y = c(ypoly1, rev(ypoly1)[1]), col=COLA[1], density=NULL, border = NA)
# ypoly1 <- indiffA(xpoly1, alpha = 0.5, uA = 4)
# polygon(x = c(0, 0, xlims[2], xlims[2], 0), y = c(0, indiffA(xlims[2]), indiffA(xlims[2]), 0, 0), col=COLA[1], density=NULL, border = NA)
# polygon(x = c(xlims[2], rev(xpoly1), xpoly1), y = c(ylims[2], rev(ypoly1), ypoly1), col=COLB[1], density=NULL, border = NA)


#Draw the graphs
lines(xx1, muFn(xx1), col = COLA[5], lwd = graphlinewidth)

#Label the feasible frontier
# text(3.2, 1, expression("Feasible Frontier"), cex = labelsize)
# text(3.2, 0.5, expression("(production possibilities frontier)"), cex = labelsize)
# Arrows(4.35, 0.95, 8.1, 0.95, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Axis labels
#mtext(expression(paste("Living, ", x)), side = 1, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], -0.12, expression(paste("Living, ", x)), xpd = TRUE, cex = axislabelsize)
text(-1.1, 0.5*ylims[2], expression(paste("Marginal utility of Living, ", u[x])), xpd = TRUE, cex = axislabelsize, srt = 90)
                                    
                                    


# contour(x, y, 
#         outer(x, y, uFn),
#         drawlabels = FALSE,
#         col = COLB[3],
#         lwd = graphlinewidth,
#         levels = a, 
#         xaxs="i", 
#         yaxs="i", 
#         add = TRUE)


#Label the indifference curves
# text(9.5, 1.3, expression(u[4]^A == 4), cex = labelsize)
# text(3, 3, expression(u^A < 4), cex = labelsize)
# text(3, 2.5, expression(paste("Worse than ", u[4]^A)), cex = labelsize)
# text(7, 7, expression(u^A > 4), cex = labelsize)
# text(7, 6.5, expression(paste("Better than ", u[4]^A)), cex = labelsize)
#text(9.5, 3.15, expression(u[2]^A), cex = labelsize)
#text(9.5, 5.85, expression(u[3]^A), cex = labelsize)

text(2 + 0.1, muFn(2) + 0.05, expression(paste(f)), cex = labelsize)
points(2, muFn(2), pch = 16, col = "black", cex = 1.5)

text(8+ 0.1, muFn(8) + 0.05, expression(paste(i)), cex = labelsize)
points(8, muFn(8), pch = 16, col = "black", cex = 1.5)

text(14+ 0.1, muFn(14) + 0.05, expression(paste(g)), cex = labelsize)
points(14, muFn(14), pch = 16, col = "black", cex = 1.5)

dev.off()
