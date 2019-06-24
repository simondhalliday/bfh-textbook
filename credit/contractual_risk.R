#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "credit/contractual_risk.pdf", width = 8, height = 6)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 5, 4, 4))

PCFn <- function(delta, q = 0.5) {
  delta/q
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
ticksy <- c(ylims[1], isoreturnFn(delta= 0.25), ylims[2])
ylabels <- c(NA, expression(paste(f,"*", phantom()==frac(1,2))), NA)
ticksx <- c(xlims[1], 0.25, xlims[2])
xlabels <- c(NA, expression(paste(delta,"*", phantom()==frac(q, 2))), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(0.01, xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(1.5, 2.5, length.out = npts)
xx4 <- seq(3, 5, length.out = npts)
xx5 <- seq(7, 9, length.out = npts)



#Draw the polygon for feasibility
xpoly1 <- c(0.01, xx1, 0.01, 0.01)
ypoly1 <- c(0.01, PCFn(xx1), ylims[2], 0.01)
polygon(x = xpoly1, y = ypoly1, col=COLA[1], density=NULL, border = NA)


#Draw the graphs
lines(xx1, PCFn(xx1), col = COLA[4], lwd = graphlinewidth)
lines(xx1, isoreturnFn(xx1), col = COLB[4], lwd = graphlinewidth)
lines(xx1, isoreturnFn(xx1, pi = 0.075), col = COLB[4], lwd = graphlinewidth)
lines(xx1, isoreturnFn(xx1, pi = 0.175), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, indiffA(xx1, alpha = 0.5, uA = 4), col = COLB[5], lwd = graphlinewidth)
#lines(xx1, ff(xx1, c = 9, s = 1.34), col = COL[3], lwd = graphlinewidth)
# lines(xx3, trsline(xx3, constant = 16), col = "gray", lty = 2, lwd = graphlinewidth)
# lines(xx4, trsline(xx4, constant = 8, slope = 1), col = "gray", lty = 2, lwd = graphlinewidth)
# lines(xx5, trsline(xx5, constant = 4, slope = 0.25), col = "gray", lty = 2, lwd = graphlinewidth)
#lines(xx4, mcline(xx4, constant = totalcost(x = 3, c0 = 2, c1 = 0.05, c2 = 0.05) - 3*marginalcost(x = 3, c1 = 0.05, c2 = 0.05), slope = marginalcost(x = 3, c1 = 0.05, c2 = 0.05)), col = "gray", lty = 2, lwd = graphlinewidth)



#Label the feasible frontier
text(0.1, 0.75, expression("Feasible combinations"), cex = labelsize)
text(0.1, 0.7, expression("of risk and interest"), cex = labelsize)
# text(3.2, 0.5, expression("(production possibilities frontier)"), cex = labelsize)
# Arrows(4.35, 0.95, 8.1, 0.95, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Axis labels
mtext(expression(paste("Interest factor, ", delta)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.08, 0.5*(ylims[2]), expression(paste("Risk, ", f)), xpd = TRUE, cex = axislabelsize, srt = 90) 


# contour(x, y, 
#         outer(x, y, uFn),
#         drawlabels = FALSE,
#         col = COLB[3],
#         lwd = graphlinewidth,
#         levels = a, 
#         xaxs="i", 
#         yaxs="i", 
#         add = TRUE)


#Label participation constraint
text(0.34, 0.97, expression(paste("Borrower's Participation Constraint")), cex = labelsize)
text(0.34, 0.89, expression(paste(f == frac(delta, q))), cex = labelsize)

#Label Iso-profit
text(0.53, 0.9, expression(paste("Iso-profit curves")), cex = labelsize)
text(0.53, 0.83, expression(paste(pi[1] == pi^L)), cex = labelsize)
text(0.53, 0.73, expression(paste(pi[2] == pi,"*")), cex = labelsize)
text(0.53, 0.63, expression(paste(pi[3] == pi^H)), cex = labelsize)
#text(9, 2.3, expression(paste("Peyton's, ", u[1]^P)), cex = labelsize)

# text(8, 4, expression(paste(trs(l,k) == frac(2,8), phantom() == 0.25)), cex = labelsize)
# Arrows(8, 3.6, 8, 2.4, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# 
# text(4, 6, expression(paste(trs(l,k) == frac(4,4), phantom()==1)), cex = labelsize)
# Arrows(4, 5.6, 4, 4.4, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# 
# text(4, 8, expression(paste(trs(l,k) == frac(8,2), phantom() == 4)), cex = labelsize)
# Arrows(3.05, 8, 2.3, 8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#Label feasible and infeasible
#text(8, 9, expression(paste(trs(l,k) == frac(x[l], x[k]), phantom() == frac(mp[l], mp[k]), phantom() == -frac(dk,dl))), cex = labelsize)
#text(3, 2.5, expression(paste("with plan ", (list(x, l, k)) )), cex = labelsize)

# text(8, 8.5, expression(paste("Production set:")), cex = labelsize)
# text(8, 8, expression(paste(x = bar(x), " feasible")), cex = labelsize)
# text(8, 7.5, expression(paste("with plan ", (list(x, l, k)) )), cex = labelsize)

#text(9.5, 3.15, expression(u[2]^A), cex = labelsize)
#text(9.5, 5.85, expression(u[3]^A), cex = labelsize)


# segments(0, 8, 2.9705, 8, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(2.9705, 0, 2.9705, 8, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(2, 0, 2, 8, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(4, 0, 4, 4, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(0, 4, 4, 4, lty = 2, col = "gray" , lwd = segmentlinewidth)
# 
# 

# 
# segments(8, 0, 8,indiffA(x = 8)  , lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(0, indiffA(x = 8) , 8,indiffA(x = 8)  , lty = 2, col = "gray" , lwd = segmentlinewidth)

#Annotate points (4,4),(2,8),(8,2) on feasibility frontier

segments(0, isoreturnFn(0.25), 0.25, isoreturnFn(0.25), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0.25, 0, 0.25, isoreturnFn(0.25), lty = 2, col = "gray" , lwd = segmentlinewidth)
text(0.25, isoreturnFn(0.25) + 0.05, expression(paste(a)), cex = labelsize)
points(0.25, isoreturnFn(0.25), pch = 16, col = "black", cex = 1.5)



dev.off()
