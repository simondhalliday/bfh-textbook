#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "employment/employment_rents.pdf", width = 8, height = 6)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 5, 4, 4))

PCFn <- function(delta, mu = 0.5) {
  delta/mu
}

isoreturnFn <- function(delta, pi=0.125) {
  1 - (pi)/delta
}

xlims <- c(0, 0.6)
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


# ticksy <- seq(from = 0, to = ylims[2], by = 1)
# ylabels <- seq(from = 0, to = ylims[2], by = 1)
# ticksx <- seq(from = 0, to = xlims[2], by = 1)
# xlabels <- seq(from = 0, to = xlims[2], by = 1)
ticksy <- c(ylims[1], 0.2, 0.4, 0.8, ylims[2])
ylabels <- c(NA, expression(paste(B)), expression(paste(B + mu)), expression(paste(w)), NA)
ticksx <- c(xlims[1], 0.25, xlims[2])
xlabels <- c(NA, expression(paste(p^{NC})), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(1.5, 2.5, length.out = npts)
xx4 <- seq(3, 5, length.out = npts)
xx5 <- seq(7, 9, length.out = npts)



#Draw the polygon for feasibility
xpoly1 <- c(0, 0.5, 0.5, 0, 0)
ypoly1 <- c(0, 0, 0.2, 0.2, 0)
polygon(x = xpoly1, y = ypoly1, col=COLA[1], density=NULL, border = NA)

xpoly2 <- c(0, 0.5, 0.5, 0, 0)
ypoly2 <- c(0.2, 0.2, 0.4, 0.4, 0.2)
polygon(x = xpoly2, y = ypoly2, col="lightgrey", density=NULL, border = NA)

xpoly3 <- c(0, 0.5, 0.5, 0, 0)
ypoly3 <- c(0.4, 0.4, 0.8, 0.8, 0.4)
polygon(x = xpoly3, y = ypoly3, col=COLB[1], density=NULL, border = NA)

#Label the feasible frontier
text(0.25, 0.3, expression("Disutility of effort when employed"), cex = labelsize)
text(0.25, 0.1, expression("Unemployment Benefit"), cex = labelsize)
text(0.25, 0.6, expression("Rent when employed"), cex = labelsize)

# text(0.1, 0.7, expression("employer"), cex = labelsize)
# text(3.2, 0.5, expression("(production possibilities frontier)"), cex = labelsize)
# Arrows(4.35, 0.95, 8.1, 0.95, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

# text(0.5, 0.25, expression("Better for"), cex = labelsize)
# text(0.5, 0.2, expression("employee"), cex = labelsize)


#Axis labels
mtext(expression(paste("Expected spell (duration) of employment, ", s)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.07, 0.5*(ylims[2]), expression(paste("Wage, ", w)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#segments(0, isoreturnFn(0.25), 0.25, isoreturnFn(0.25), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 0.8, 0.5, 0.8, lty = 1, col = COLB[4] , lwd = segmentlinewidth)

segments(0, 0.2, 0.5, 0.2, lty = 1, col = COLA[4] , lwd = segmentlinewidth)
segments(0.5, 0.2, 0.5, 0.8, lty = 1, col = COLA[4] , lwd = segmentlinewidth)
#text(0.25, isoreturnFn(0.25) + 0.05, expression(paste(n)), cex = labelsize)
#points(0.25, isoreturnFn(0.25), pch = 16, col = "black", cex = 1.5)



dev.off()
