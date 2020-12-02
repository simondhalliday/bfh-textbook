#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "specprodexch/ppf_feasible_hrs.pdf", width = 9, height = 9)

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
grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00")


#Edited the margins to cater for the larger LHS labels
par(mar =  c(1.5, 1.5, 0.5, 1.5))

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
 time - l
}

manufactureProd <- function(l, k = 0.1, alpha = 2){
  k * (-l)^alpha
}

xlims <- c(0, 10)
ylims <- c(0, 11)

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

ticksy <- c(0, 5, 10, ylims[2])
ylabels <- c(NA, 5, 10, NA)
ticksx <- c(0, 5, 10, xlims[2])
xlabels <- c(NA, 5, 10, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(0, 5, length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(0, xlims[2],length.out = npts)
xx4 <- seq(11, 0, length.out = npts)

#Draw the polygon for shading the feasible set
xpoly1 <- seq(from = 0, to = 5, length.out = 500)
ypoly1 <- ppf(xpoly1, k = 10/25, alpha = 2, maxfish = 5)
#polygon(x = c(xpoly1, rev(xpoly1[1])), y = c(ypoly1, rev(ypoly1)[1]), col=COLA[1], density=NULL, border = NA)

# shade 
polygon(x = c(10, 0, 0), 
        y = c(0, 0, 10),
        border = FALSE, col = adjustcolor(COL[3], alpha.f = 0.5))


#Draw the graphs
lines(xx3, feasibleLabor(xx3, time = 10), col = CBCols[3], lwd = graphlinewidth)

#mtext(expression(paste("Quantity of fish, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-0.5, 7.5, expression(paste("Labor for fish, ", l^f)), xpd = TRUE, cex = axislabelsize, srt= 90) 
text(7.5, -0.5, expression(paste("Labor for shirts, ", l^s)), xpd = TRUE, cex = axislabelsize)

#Draw segments for the 50/50 split of time
segments(0, 5, 5, 5, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(5, 0, 5, 5, lty = 2, col = grays[20], lwd = segmentlinewidth)

#Annotate point on labor feasibility frontier
points(5, 5, pch = 16, col = "black", cex = 1.5)
text(6.1, 5.1, expression(paste(list(l^S ==5, l^F == 5) )),cex = annotatesize)
text(4.7, 4.7, expression(paste("d")),cex = annotatesize)

Arrows(3, 9.3, 3, 7.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(3, 9.6, expression("Constraint"),cex = labelsize)
text(3, 10, expression("on total"),cex = labelsize, xpd = TRUE)
text(3, 10.4, expression("labor hours"),cex = labelsize, xpd = TRUE)
text(3, 10.8, expression(paste(l^S + l^F <= 10)),cex = annotatesize)
#Arrows(-6, -7, -3.5, -7, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

# Label constaint 
#Label the feasible frontier
text(2.65, 2.8, expression("Feasible"),cex = labelsize)
text(2.65, 2.4, expression("set of"),cex = labelsize)
text(2.65, 2, expression("labor hours"),cex = labelsize)



dev.off()
