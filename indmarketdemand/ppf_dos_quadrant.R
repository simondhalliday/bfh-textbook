#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "indmarketdemand/ppf_dos_quadrant.pdf", width = 9, height = 9)

#Set parameters for graphics
axislabelsize <- 1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 4, 4))

#Change this to make it log of l 

# ppf <- function(l, k = 0.5) {
#   k * (1/l)
# }


ppf <- function(x, yint = 64){
  sqrt(yint - (x)^2)
}

#actualfunction
#foodProd <- function(l, k = -2.529822, alpha = 0.5){
#  (-k)*l^alpha
#}

foodProd <- function(l, k = (6.328125)^-1, alpha = 2){
  (-k)*(l)^alpha
}

feasibleLabor <- function(l, time = 10){
  - time - l
}

manufactureProd <- function(l, k =  2.529822, alpha = 0.5){
  k * (-l)^alpha
}

xlims <- c(-10, 10)
ylims <- c(-10, 10)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n"
)

ticksy <- c(ylims[1], -5, 0, manufactureProd(-5), ylims[2])
ylabels <- c(NA, NA, NA, NA, NA)
ticksx <- c(xlims[1], -5, 0, manufactureProd(-5), xlims[2])
xlabels <- c(NA, NA, NA, NA, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 501 
xx1 <- seq(0, 20, length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Draw the polygon for shading the feasible set
xpoly1 <- seq(from = 0, to = 8, length.out = 501)
ypoly1 <- ppf(xpoly1)
polygon(x = c(xpoly1, rev(xpoly1[1])), y = c(ypoly1, rev(ypoly1)[1]), col=COLA[1], density=NULL, border = NA)

#Draw the graphs
lines(xx1, ppf(xx1), col = COLA[5], lwd = graphlinewidth)
lines(xx2, foodProd(xx2), col = COLB[3], lwd = graphlinewidth)
lines(xx3, feasibleLabor(xx3, time = 10), col = COL[3], lwd = graphlinewidth)
lines(xx4, manufactureProd(xx4, k =  2.529822, alpha = 0.5), col = COLB[4], lwd = graphlinewidth)

#mtext(expression(paste("Quantity of food, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-0.5, 8, expression(paste("Meters of Cloth, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(8, -0.5, expression(paste("Kilograms of Food, ", x)), xpd = TRUE, cex = axislabelsize)
text(0.5, -7.5, expression(paste("Labor for Food, ", L^f)), xpd = TRUE, cex = axislabelsize, srt= 90) 
text(-7.5, 0.5, expression(paste("Labor for Cloth, ", L^c)), xpd = TRUE, cex = axislabelsize)

#Label the points on the axes we want
#text(-0.6, 2.8, expression(paste(12.5)), xpd = TRUE, cex = axislabelsize)
text(-0.5, 5.4, expression(paste(5.66)), xpd = TRUE, cex = axislabelsize)
#text(2.9, -0.3, expression(paste(2.5)), xpd = TRUE, cex = axislabelsize)
text(5.1, -0.3, expression(paste(5.66)), xpd = TRUE, cex = axislabelsize)
text(-5.3, -0.3, expression(paste(5)), xpd = TRUE, cex = axislabelsize)
text(-0.3, -5.3, expression(paste(5)), xpd = TRUE, cex = axislabelsize)
text(-10.3, -0.3, expression(paste(10)), xpd = TRUE, cex = axislabelsize)
text(-0.5, -10.3, expression(paste(10)), xpd = TRUE, cex = axislabelsize)

#Label the two production functions
#Clothing
text(-8, 5.8, expression(paste("Cloth Production")), xpd = TRUE, cex = axislabelsize)
text(-8.2, 5, expression(paste(y == 2.53*(L^s)^frac(1,2))), xpd = TRUE, cex = axislabelsize)
Arrows(-6.7, 5, -4.5, 5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#food
text(2.2, -3, expression(paste("Food Production")), xpd = TRUE, cex = axislabelsize)
text(2, -3.8, expression(paste(x == 2.53*(L^f)^frac(1,2))), xpd = TRUE, cex = axislabelsize)
Arrows(3.4, -3.8, 4.5, -3.8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Draw segments for the 50/50 split of time
segments(-5, -5, -5, manufactureProd(-5), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(-5, -5, manufactureProd(-5), -5, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(-5, manufactureProd(-5), manufactureProd(-5), manufactureProd(-5), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(manufactureProd(-5), -5, manufactureProd(-5), manufactureProd(-5), lty = 2, col = "gray", lwd = segmentlinewidth)

#Annotate Max time on clothes
segments(-10, 0, -10, 8, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(-10, 8, 0, 8, lty = 2, col = "gray", lwd = segmentlinewidth)
text(-7.3, 9.5, expression(paste("10 hrs of labor")))
text(-7.3, 8.9, expression(paste("for cloth produces")))
text(-7.3, 8.3, expression(paste("8 meters of cloth")))
points(-10, 8, pch = 16, col = "black", cex = 1.5)

#Annotate Max time on farming
segments(0, -10, 8, -10, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(8, -10, 8, 0, lty = 2, col = "gray", lwd = segmentlinewidth)
text(5.2, -8.4, expression(paste("10 hrs of labor")))
text(5.2, -9, expression(paste("for farming produces")))
text(5.2, -9.6, expression(paste("8 kgs of food")))
points(8, -10, pch = 16, col = "black", cex = 1.5)

#Annotate point on ppf 
points(manufactureProd(-5), manufactureProd(-5), pch = 16, col = "black", cex = 1.5)
text(manufactureProd(-5) + 2.75, manufactureProd(-5) + 0.5, expression(paste("5.66 meters of cloth &")))
text(manufactureProd(-5) + 2.75, manufactureProd(-5), expression(paste("5.66 Kilograms of food")))

#Annotate point on labor feasibility frontier
points(-5, -5, pch = 16, col = "black", cex = 1.5)
text(-6.75, -5, expression(paste(list(L^c ==5, L^f == 5) )))

text(-3, -9, expression(paste(L^c + L^f <= 10)))
Arrows(-3, -8.5, -3, -7.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the feasible frontier
text(4.5, 9, expression("Feasible Frontier"))
text(4.5, 8.5, expression("(production possibilities frontier)"))


dev.off()
