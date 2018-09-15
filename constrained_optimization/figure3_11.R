#Graph Designer: Simon Halliday & Riley Boeth '17
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "constrained_optimization/indiff_feasible_environ_initial.pdf", width = 8, height = 8)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 4, 4))

#Change this to make it log of l 

# ppf <- function(l, k = 0.5) {
#   k * (1/l)
# }

#Change to feasibility frontier equation

ppf <- function(x, y) {
  100-((x^2)/2)
}

#change alpha to .4

uFn <- function(x, y, alpha = 0.4){
  (x^alpha)*(y^(1-alpha))
}

#Add limits on axes and levels of utility for each indifference curve

ylims <- c(0, 130)
xlims <- c(0, 20)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

a <- c(29.16-4, 29.16, 29.16+4) #alpha = 0.4

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

#changed x and y limits and ticks 
 
ticksx <- c(xlims[1], 7.07, xlims[2])
xlabels <- c(NA, expression(paste(x,"*")), NA)
ticksy <- c(ylims[1], ppf(x = 7.07), 100, ylims[2])
ylabels <- c(NA, expression(paste(y,"*")), expression(paste(bar(y))), NA)


axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)


#Draw the graphs
lines(xx1, ppf(xx1, y), col = COLA[5], lwd = graphlinewidth)

#Label the feasible frontier
text(15, 15, expression("Initial"), cex = labelsize)
text(15, 11, expression("Feasible"), cex = labelsize)
text(15, 7.5, expression("Frontier"), cex = labelsize)
Arrows(0.5, 78, 0.5, 97, col = "black", lty = 1, lwd = 2, code = 3, arr.type = "triangle", arr.lwd = 0.5)
text(2, 88, expression("Abatement"), cex = labelsize)
text(2, 84, expression("Cost"), cex = labelsize)

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
mtext(expression(paste("Environmental quality, ", x)), side = 1, line = 2.5, cex = axislabelsize)
text(-1.8, 0.5*ylims[2], expression(paste("Goods in millions, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the indifference curves generally
text(8.2, 120, expression("Policy-Maker's"), cex = labelsize)
text(8.2, 116, expression("Indifference Curves"), cex = labelsize)

#Label the indifference curves and place labels properly
text(19, 33, expression(u[1]^A), cex = labelsize)
text(19, 42, expression(u[2]^A), cex = labelsize)
text(19, 51, expression(u[3]^A), cex = labelsize)

#Annotate max u point on feasibility frontier
text(7.3, ppf(7.07) + 3, expression(paste(f)), cex = labelsize)
segments(7.07, 0, 7.07, ppf(x = 7.07), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, ppf(x = 7.07), 7, ppf(x = 7), lty = 2, col = "gray", lwd = segmentlinewidth)
points(7.07, ppf(x = 7.07), pch = 16, col = "black", cex = 1.5)




dev.off()