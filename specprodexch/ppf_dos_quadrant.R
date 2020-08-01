#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "specprodexch/ppf_dos_quadrant.pdf", width = 9, height = 9)

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
par(mar =  c(3, 3, 1, 3))

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

# shade 
polygon(x = c(-10, 0, 0), 
        y = c(0, 0, -10),
        border = FALSE, col = adjustcolor(COL[3], alpha.f = 0.5))

#Draw the graphs
lines(xx1, ppf(xx1), col = CBCols[1], lwd = graphlinewidth)
lines(xx2, foodProd(xx2), col = CBCols[2], lwd = graphlinewidth)
lines(xx3, feasibleLabor(xx3, time = 10), col = CBCols[3], lwd = graphlinewidth)
lines(xx4, manufactureProd(xx4, k =  2.529822, alpha = 0.5), col = COLB[4], lwd = graphlinewidth)

#mtext(expression(paste("Quantity of food, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-0.75, 9.5, expression(paste("Shirts,", y)), xpd = TRUE, srt = 90, cex = labelsize) 
text(10.1, -0.75, expression(paste("Kgs of fish, ", x)), xpd = TRUE, cex = labelsize)
text(0.5, -9.5, expression(paste("Labor for fish, ", l^f)), xpd = TRUE, cex = labelsize, srt= 90) 
text(-9.5, 0.5, expression(paste("Labor for shirts, ", l^s)), xpd = TRUE, cex = labelsize)

#Label the points on the axes we want
#text(-0.6, 2.8, expression(paste(12.5)), xpd = TRUE, cex = axislabelsize)
text(-0.8, 5.4, expression(paste(5.66)), xpd = TRUE, cex = labelsize)
#text(2.9, -0.3, expression(paste(2.5)), xpd = TRUE, cex = axislabelsize)
text(6, -0.35, expression(paste(5.66)), xpd = TRUE, cex =labelsize)
text(-5, -0.3, expression(paste(5)), xpd = TRUE, cex = labelsize)
text(-0.3, -5.3, expression(paste(5)), xpd = TRUE, cex = labelsize)
text(-10.3, -0.3, expression(paste(10)), xpd = TRUE, cex = labelsize)
text(-0.5, -10.3, expression(paste(10)), xpd = TRUE, cex = labelsize)

#Feasble output
text(2.65, 2.8, expression("Feasible"),cex = labelsize)
text(2.65, 2.2, expression("output"),cex = labelsize)

#Label the two production functions
#Clothing
text(-8.5, 5.8, expression(paste("Shirt production")), xpd = TRUE, cex = labelsize)
text(-8.7, 4.7, expression(paste(y == 2.53*(l^s)^frac(1,2))), xpd = TRUE, cex = labelsize)
Arrows(-6.5, 4.5, -4, 4.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#food
text(10, -3, expression(paste("Fish production")), xpd = TRUE, cex = labelsize)
text(10, -3.8, expression(paste(x == 2.53*(l^f)^frac(1,2))), xpd = TRUE, cex = labelsize)
Arrows(8, -3.8, 5.5, -3.8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Draw segments for the 50/50 split of time
segments(-5, -5, -5, manufactureProd(-5), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(-5, -5, manufactureProd(-5), -5, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(-5, manufactureProd(-5), manufactureProd(-5), manufactureProd(-5), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(manufactureProd(-5), -5, manufactureProd(-5), manufactureProd(-5), lty = 2, col = grays[20], lwd = segmentlinewidth)

#Annotate Max time on clothes
segments(-10, 0, -10, 8, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(-10, 8, 0, 8, lty = 2, col = grays[20], lwd = segmentlinewidth)
text(-7.3, 9.5, expression(paste("10 hrs of labor")), cex = labelsize)
text(-7.3, 8.9, expression(paste("for shirts produces")),cex = labelsize)
text(-7.3, 8.3, expression(paste("8 shirts")), cex = labelsize)
points(-10, 8, pch = 16, col = "black", cex = 1.5)

#Annotate Max time on farming
segments(0, -10, 8, -10, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(8, -10, 8, 0, lty = 2, col = grays[20], lwd = segmentlinewidth)
text(5.2, -10.4, expression(paste("10 hrs of labor")), xpd= TRUE, cex = labelsize)
text(5, -11.1, expression(paste("for fishing produces")), xpd= TRUE, cex = labelsize)
text(5.2, -11.9, expression(paste("8 kgs of fish")),xpd= TRUE, cex = labelsize)
points(8, -10, pch = 16, col = "black", cex = 1.5)

#Annotate point on ppf 
points(manufactureProd(-5), manufactureProd(-5), pch = 16, col = "black", cex = 1.5)
# text(manufactureProd(-5) + 3.5, manufactureProd(-5) + 0.5, expression(paste("5.66 shirts &")),xpd= TRUE, cex = labelsize)
# text(manufactureProd(-5) + 4, manufactureProd(-4.5), expression(paste("5.66 kilograms of fish")),xpd= TRUE, cex = labelsize)

#Annotate point on labor feasibility frontier
points(-5, -5, pch = 16, col = "black", cex = 1.5)
text(-7, -5, expression(paste(list(l^s ==5, l^f == 5))), cex = labelsize)

text(-3, -9, expression(paste(l^s + l^f <= 10)), cex = labelsize)
Arrows(-3, -8.5, -3, -7.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
text(-3, -9.8, expression("Constraint"),cex = labelsize)
text(-3, -10.5, expression("on total"),cex = labelsize, xpd = TRUE)
text(-3, -11.2, expression("labor hours"),cex = labelsize, xpd = TRUE)



#Label the feasible frontier
text(5.5, 8, expression("Feasible frontier"),cex = labelsize)
#text(5.5, 8.4, expression("(production possibilities frontier)"),cex = labelsize)

# Label constraint 
#Label the feasible frontier
text(-2.65, -2.2, expression("Feasible"),cex = labelsize)
text(-2.65, -2.8, expression("labor hours"),cex = labelsize)
#text(-2.65, -3.4, expression("labor hours"),cex = labelsize)

dev.off()
