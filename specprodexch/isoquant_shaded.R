#Graph Designer: Simon Halliday + Scott Cohn
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "specprodexch/isoquant_shaded.pdf", width = 8, height = 6)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(6, 6, 4, 4))

uFn <- function(x, y, alpha = 0.5){
  (x^alpha)*(y^(1 - alpha))
}

indiffA <- function(x, alpha = 0.5, uA = 5) {
  (uA / x^alpha)^(1/(1 - alpha))
}

xlims <- c(0, 10)
ylims <- c(0, 10)

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

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)

#Draw the polygon for shading the utility areas
xpoly1 <- seq(from = xlims[1], to = xlims[2], length.out = 500)
ypoly1 <- indiffA(xpoly1, alpha = 0.5, uA = 4)
polygon(x = c(xpoly1, rev(xpoly1[1])), y = c(ypoly1, rev(ypoly1)[1]), col=COLB[1], density=NULL, border = NA)
ypoly1 <- indiffA(xpoly1, alpha = 0.5, uA = 4)
polygon(x = c(0, 0, xlims[2], xlims[2], 0), y = c(0, indiffA(xlims[2]), indiffA(xlims[2]), 0, 0), col=COLB[1], density=NULL, border = NA)


polygon(x = c(xlims[2], rev(xpoly1), xpoly1), y = c(ylims[2], rev(ypoly1), ypoly1), col=COLA[1], density=NULL, border = NA)


#Draw the graphs
lines(xx1, indiffA(xx1, alpha = 0.5, uA = 4), col = COLA[5], lwd = graphlinewidth)

#Label the feasible frontier
# text(3.2, 1, expression("Feasible Frontier"), cex = labelsize)
# text(3.2, 0.5, expression("(production possibilities frontier)"), cex = labelsize)
# Arrows(4.35, 0.95, 8.1, 0.95, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Axis labels
mtext(expression(paste("Hours of labor, ", l)), side = 1, line = 3.5, cex = axislabelsize)
text(-1.3, 5, expression(paste("Amount of capital, ", k)), xpd = TRUE, cex = axislabelsize, srt = 90) 


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
text(8.5, 5, expression("Cobb-Douglas"), cex = labelsize)
text(8.5, 4.4, expression("isoquant"), cex = labelsize)
text(8.5, 3.7, expression(underline(x) ==f(l,k)), cex = labelsize)
Arrows(9, 3.25, 9, 2.1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label feasible and infeasible
text(3, 3, expression(paste(x = underline(x), " infeasible")), cex = labelsize)
text(3, 2.3, expression(paste("with plan ", (list(x, l, k)) )), cex = labelsize)

text(8, 8.5, expression(paste("Production set:")), cex = labelsize)
text(8, 7.9, expression(paste(x = underline(x), " feasible")), cex = labelsize)
text(8, 7.3, expression(paste("with plan ", (list(x, l, k)) )), cex = labelsize)

#text(9.5, 3.15, expression(u[2]^A), cex = labelsize)
#text(9.5, 5.85, expression(u[3]^A), cex = labelsize)

dev.off()