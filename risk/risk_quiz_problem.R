#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Microeconomics: Competition, Conflict and Coordination

library(shape)
#pdf(file = "feasible_frontier_indifference_badsSTEP1.pdf", width = 8, height = 8)
#pdf(file = "feasible_frontier_indifference_badsSTEP2.pdf", width = 8, height = 8)
#pdf(file = "feasible_frontier_indifference_badsSTEP3.pdf", width = 8, height = 8)
#pdf(file = "feasible_frontier_indifference_badsSTEP4.pdf", width = 8, height = 8)
pdf(file = "risk/risk_quiz_problem.pdf", width = 8, height = 8)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 2
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")
grays <- gray.colors(25, start = 1, end = 0)


#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 1, 1))

ppf <- function(x, slope = 1/100, bary = 4) {
  bary - slope *(25-x)^2
}

uFn <- function(x, y, alpha = 0.4){
  ((25-x)^alpha)*(y^(1-alpha))
}

indiffA <- function(x, ubar =2, alpha = 0.4) {
  (ubar/ ((25-x)^alpha))^(1/ (1 - alpha))
}

tangentLine <- function(x, slope, intercept){
  intercept + slope*x
}

ylims <- c(0, 6)
xlims <- c(0, 26)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
#step by step graphs:
#step 2:
#a <- uFn(8, 3) - 1
#step 3:
#a <- c(uFn(8, 3) - 1, uFn(8, 3) + 1.4) 
#final:
a <- c(uFn(15, 3) - 2, uFn(15, 3), uFn(15, 3) + 2, 50) #alpha = 0.4

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
ticksx <- seq(from = 0, to = xlims[2], by = 5)
xlabels <- seq(from = 0, to = xlims[2], by = 5)
ticksy <- c(ylims[1], 1, 2, 3, 4, ylims[2])
ylabels <- c(NA, 1, 2, 3, 4, NA)
# ticksx <- c(xlims[1], 5.25, 8.944272, xlims[2])
# xlabels <- c(NA, expression(paste(x,"*")), expression(paste(bar(x))), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

xx3 <- seq(4.5, 11.5, length.out = npts)

#Draw the polygon for shading the feasible set
xpoly1 <- seq(from = xlims[1], to = xlims[2], length.out = 500)
ypoly1 <- ppf(xpoly1)
polygon(x = c(xlims[2], xlims[2], xpoly1, 0),
        y = c(ppf(xlims[2]), 0, ypoly1, ylims[2]),
        col=COLA[1], density=NULL, border = NA)

#Draw the graphs
lines(xx1, ppf(xx1), col = COLA[5], lwd = graphlinewidth)

#lines(xx3, tangentLine(xx3, slope = 1/4, intercept = 1), col = grays[22], lwd = segmentlinewidth, lty = 2)

#Label the feasible frontier
text(20, 0.8, expression("Risk-return"), cex = labelsize)
text(20, 0.6, expression("schedule"), cex = labelsize)
Arrows(17, 0.7, 7.5, 0.7, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

contour(x, y, 
        outer(x, y, uFn),
        drawlabels = FALSE,
        col = COLB[3],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE)

#Axis labels
text(0.5*xlims[2], -0.45,  expression(paste("Risk, ", Delta)), xpd = TRUE, cex = axislabelsize) 
text(-2, 0.5*ylims[2], expression(paste("Expected income, ", hat(y))), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Add mrs = mrt at b
#text(7.3, 3.6, expression(paste(mrs(x,y) == mrt(x,y))), cex = labelsize)
#Arrows(8, 3.5, 8, 3.15, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(12.8, 3.8, expression(paste(mrs(Delta,hat(y)) == mrt(Delta,hat(y)))), cex = labelsize)
Arrows(12.8, 3.7, 14.8, 3.15, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#Label the indifference curves
text(23.3, 5.7, expression(u[1]^A), cex = labelsize)
text(20.5, 5.7, expression(u[2]^A), cex = labelsize)
text(15, 5.7, expression(u[3]^A), cex = labelsize)

#Annotate max u point on feasible frontier
# text(8.4, ppf(8) - 0.1, expression(paste(b)), cex = labelsize)
# segments(8, 0, 8, ppf(x = 8), lty = 2, col = grays[20], lwd = segmentlinewidth)
# #segments(0, ppf(x = 8), 8, ppf(x = 8), lty = 2, col = grays[20], lwd = segmentlinewidth)
# points(8, ppf(x = 8), pch = 16, col = "black", cex = 1.5)

# text(3.2-0.2, ppf(3.2) + 0.1, expression(paste(a)), cex = labelsize)
# segments(3.2, 0, 3.2, ppf(x = 3.2), lty = 2, col = grays[20], lwd = segmentlinewidth)
# segments(0, ppf(x = 3.2), 3.2, ppf(x = 3.2), lty = 2, col = grays[20], lwd = segmentlinewidth)
# points(3.2, ppf(x = 3.2), pch = 16, col = "black", cex = 1.5)


text(15 + 0.5, ppf(15) - 0.1, expression(paste(b)), cex = labelsize)
segments(15, 0, 15, ppf(x = 15), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, ppf(x = 15), 15, ppf(x = 15), lty = 2, col = grays[20], lwd = segmentlinewidth)
points(15, ppf(x = 15), pch = 16, col = "black", cex = 1.5)



dev.off()
