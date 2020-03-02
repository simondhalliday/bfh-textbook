#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "specprodexch/ppf_eos.pdf", width = 9, height = 9)

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
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(1, 1, 1, 1))

#Change this to make it log of l 

# ppf <- function(l, k = 0.5) {
#   k * (1/l)
# }

ppf <- function(fish, k = 0.1, alpha = 2, maxfish = 5) {
  k * (fish - maxfish)^alpha 
}

fishProd <- function(l, k = 0.5){
  (-k)*l
}

feasibleLabor <- function(l, time = 10){
  - time - l
}

manufactureProd <- function(l, k = 0.1, alpha = 2){
  k * (-l)^alpha
}

xlims <- c(-10, 10)
ylims <- c(-11, 11)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(13, 15, 17)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n"
)

ticksy <- c(ylims[1], -5, 0, 2.5, ylims[2])
ylabels <- c(NA, NA, NA, NA, NA)
ticksx <- c(xlims[1], -5, 0, xlims[2])
xlabels <- c(NA, NA, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(0, 5, length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Draw the polygon for shading the feasible set
xpoly1 <- seq(from = 0, to = 5, length.out = 500)
ypoly1 <- ppf(xpoly1, k = 10/25, alpha = 2, maxfish = 5)
polygon(x = c(xpoly1, rev(xpoly1[1])), y = c(ypoly1, rev(ypoly1)[1]), col=COLA[1], density=NULL, border = NA)

# shade 
polygon(x = c(-10, 0, 0), 
        y = c(0, 0, -10),
        border = FALSE, col = adjustcolor(COL[3], alpha.f = 0.5))


#Draw the graphs
lines(xx1, ppf(xx1, k = 10/25, alpha = 2, maxfish = 5), col = COLA[5], lwd = graphlinewidth)
lines(xx2, fishProd(xx2, k = 2), col = COLB[3], lwd = graphlinewidth)
lines(xx3, feasibleLabor(xx3, time = 10), col = COL[3], lwd = graphlinewidth)
lines(xx4, manufactureProd(xx4, k = 0.1, alpha = 2), col = COLB[4], lwd = graphlinewidth)

#mtext(expression(paste("Quantity of fish, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-0.5, 8, expression(paste("Shirts, ", y)), xpd = TRUE, cex = annotatesize, srt = 90) 
text(8, -0.5, expression(paste("Fish, ", x)), xpd = TRUE, cex = annotatesize)
text(0.5, -7.5, expression(paste("Labor for Fish, ", l^f)), xpd = TRUE, cex = annotatesize, srt= 90) 
text(-7.5, 0.5, expression(paste("Labor for Shirts, ", l^s)), xpd = TRUE, cex = annotatesize)

#Label the points on the axes we want
text(-0.6, 2.8, expression(paste(12.5)), xpd = TRUE, cex = annotatesize)
text(-0.5, 9.7, expression(paste(50)), xpd = TRUE, cex = annotatesize)
text(2.9, -0.3, expression(paste(2.5)), xpd = TRUE, cex = annotatesize)
text(5.2, -0.3, expression(paste(5)), xpd = TRUE, cex = annotatesize)
text(-5.3, -0.3, expression(paste(5)), xpd = TRUE, cex = annotatesize)
text(-0.3, -5.3, expression(paste(5)), xpd = TRUE, cex = annotatesize)
text(-10.3, -0.3, expression(paste(10)), xpd = TRUE, cex = annotatesize)
text(-0.5, -10.3, expression(paste(10)), xpd = TRUE, cex = annotatesize)

#Label the two production functions
#Clothing
text(-3.25, 6, expression(paste("Shirt Production")), xpd = TRUE, cex = annotatesize)
text(-3.15, 4.8, expression(paste(y == frac(1,2)(l^s)^2)), xpd = TRUE, cex = annotatesize)
Arrows(-4.5, 5, -6.5, 5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Fishing
text(6.8, -4.8, expression(paste("Fish Production")), xpd = TRUE, cex = annotatesize)
text(7, -6, expression(paste(x == frac(1,2)(l^f))), xpd = TRUE, cex = annotatesize)
Arrows(5.8, -5.8, 3.5, -5.8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Draw segments for the 50/50 split of time
segments(-5, -5, -5, 2.5, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(-5, -5, 2.5, -5, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(-5, 2.5, 2.5, 2.5, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(2.5, -5, 2.5, 2.5, lty = 2, col = "gray", lwd = segmentlinewidth)

#Annotate Max time on clothes
segments(-10, 0, -10, 10, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(-10, 10, 0, 10, lty = 2, col = "gray", lwd = segmentlinewidth)
text(-6.5, 9.5, expression(paste("10 hrs of labor")), cex = annotatesize)
text(-6.5, 8.7, expression(paste("for shirts produces")),  cex = annotatesize)
text(-6.5, 8.1, expression(paste("50 shirts")),  cex = annotatesize)
points(-10, 10, pch = 16, col = "black", cex = 1.5)
text(-9.7, 10.5, expression(paste("a")),  cex = annotatesize)

points(-5, 2.5, pch = 16, col = "black", cex = 1.5)
text(-5, 3, expression(paste("e")), cex = annotatesize)

#Annotate Max time on fishing
segments(0, -10, 5, -10, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(5, -10, 5, 0, lty = 2, col = "gray", lwd = segmentlinewidth)
text(7.8, -8.4, expression(paste("10 hrs of labor")), cex = annotatesize)
text(7.8, -9.2, expression(paste("for fishing produces")),cex = annotatesize)
text(7.8, -9.9, expression(paste("5 kgs of fish")), cex = annotatesize)
points(5, -10, pch = 16, col = "black", cex = 1.5)
text(4.6, -10.5, expression(paste("c")), cex = annotatesize)

points(2.5, -5, pch = 16, col = "black", cex = 1.5)
text(3, -5, expression(paste("f")), cex = annotatesize)

#Annotate point on ppf 
points(2.5, 2.5, pch = 16, col = "black", cex = 1.5)
#text(5.6, 3.35, expression(paste("12.5 Shirts and")),cex = annotatesize)
#text(5.6, 2.75, expression(paste("2.5 Kilograms of Fish")), cex = annotatesize)
text(2.9, 2.6, expression(paste("b")), cex = annotatesize)

#Annotate point on labor feasibility frontier
points(-5, -5, pch = 16, col = "black", cex = 1.5)
text(-6.75, -5, expression(paste(list(l^S ==5, l^F == 5) )),cex = annotatesize)
text(-4.5, -4.5, expression(paste("d")),cex = annotatesize)


text(-8, -6.5, expression("Constraint"),cex = labelsize)
text(-8, -7.1, expression("on totals"),cex = labelsize)
text(-8, -7.7, expression("labor hours"),cex = labelsize)
text(-8, -8.5, expression(paste(l^S + l^F <= 10)),cex = annotatesize)
Arrows(-6, -7, -3.5, -7, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the feasible frontier
text(1.4, 2, expression("Feasible"), cex = annotatesize)
text(1.4, 1.4, expression("set of"), cex = annotatesize)
text(1.4, 0.7, expression("outputs"), cex = annotatesize)

# Label constaint 
#Label the feasible frontier
text(-2.65, -2.2, expression("Feasible"),cex = labelsize)
text(-2.65, -2.8, expression("set of"),cex = labelsize)
text(-2.65, -3.4, expression("labor hours"),cex = labelsize)



dev.off()
