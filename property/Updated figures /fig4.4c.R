
require(shape)
#pdf(file = "fig4.4cSTEP1.pdf", width = 9, height = 7)
#pdf(file = "fig4.4cSTEP2.pdf", width = 9, height = 7)
#pdf(file = "fig4.4cSTEP3.pdf", width = 9, height = 7)
pdf(file = "fig4.4c.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1
graphlinewidth <- 3

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

indiffcurveA1 <- function(x, U = 3, A = 1, a = 1/2) {
  ((((U-2)/A)*(1/x)^a)^(1/(1-a)))
}

indiffcurveA2 <- function(x, U = 3, A = 1, a = 1/2) {
  (((U/A)*(1/x)^a)^(1/(1-a)))
}

indiffcurveA3 <- function(x, U = 3, A = 1, a = 1/2) {
  ((((U+2)/A)*(1/x)^a)^(1/(1-a)))
}

#Ayanda happens to have found 8 apples and 2 oranges, 
#and Bongani happens to have found 2 apples and 13 oranges. 
#Ayanda's utility (8^0.5)*(2^0.5) = 4
#Bongani's utility (2^0.5)*(13^0.5) = 5.09

par(mar =  c(6, 4, 4, 4))
xlims <- c(0, 10)
ylims <- c(0, 15)
xlims2 <- c(10, 0)
ylims2 <- c(15, 0)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("A's coffee (kilograms), ", x^A)),
     ylab = expression(paste("A's data (gigabytes), ", y^A)), line = 2.5,
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")


npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
#xx2 <- seq(xlims[1], xlims[2], length.out = npts)
#xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
#xx4 <- seq(xlims[1], 10, length.out = npts2)

#Draw the lines for the graphs
lines(xx1, indiffcurveA1(xx1), col = COLA[3], lwd = graphlinewidth)
lines(xx1, indiffcurveA2(xx1), col = COLA[3], lwd = graphlinewidth)
lines(xx1, indiffcurveA3(xx1), col = COLA[3], lwd = graphlinewidth)
#lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 4)
#lines(xx2, solowInfeas(xx2, delta = 5), col = COL[1], lwd = 4, lty = 2)

#Customize ticks and labels for the plot
ticksy <- seq(from = 0, to = 15, by = 1)
ylabels <- seq(from = 0, to = 15, by = 1)
ticksx <- seq(from = 0, to = 10, by = 1)
xlabels <- seq(from = 0, to = 10, by = 1)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0)

#Add arrows and names:
arrows(-.8, 10, -.8, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(6.3, -1.6, 9, -1.6, xpd = TRUE, length=0.1,angle=40,lwd=3)
mtext("Ayanda", side=1, line=3, at=0, col=COLA[4])

#Annotation of the three graphs and the NE
text(.3, 10, expression(u[1]^A))
text(1.1, 10, expression(u[2]^A))
text(2.75, 10, expression(u[3]^A))
#text(16, 0.52, expression(paste("Nash Equilibrium")))

#Line to label B's endowment
#segments(0, 13, 2, 13, lty = 2, col = "darkgray", lwd = 2)
#segments(2, 0, 2, 13, lty = 2, col = "darkgray", lwd = 2)


#Set up second axes and labels

par(new = TRUE)
#par(mar =  c(6, 4, 4, 4))

xlims2 <- c(10, 0)
ylims2 <- c(15, 0)

plot(0, 0, xlim = xlims2, ylim = ylims2, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")),
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = 1.3, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

axis(side = 3, at = ticksx, pos = 0, labels = xlabels)
axis(side = 4, at = ticksy, pos = 0, labels = ylabels, las = 0)
#axis(side=3, at = xlims2, pos = 0))
mtext(expression(paste("B's coffee (kilograms) ,",x^B )), side=3, line = 2.5, cex = axislabelsize)
#axis(side = 4, at = ylims2, pos = 0)
text(-.8, 7, expression(paste("B's data (gigabytes) ,",y^B )), xpd = TRUE, cex = axislabelsize, srt = 270) 

#Add arrows and names:
arrows(-.8, 10, -.8, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(6.3, -1.8, 9, -1.8, xpd = TRUE, length=0.1,angle=40,lwd=3)
mtext("Bongani", side=3, line=3, at=0, col= COLB[2])

indiffcurveB1 <- function(x, U = sqrt(14), A = 1, a = 0.5) {
  ((((U-2)/A)*(1/x)^a)^(1/(1-a)))
}

indiffcurveB2 <- function(x, U = sqrt(14), A = 1, a = 0.5) {
  (((U/A)*(1/x)^a)^(1/(1-a)))
}

indiffcurveB3 <- function(x, U = sqrt(14), A = 1, a = 0.5) {
  ((((U+2)/A)*(1/x)^a)^(1/(1-a)))
}

lines(xx1, indiffcurveB1(xx1), col = COLB[2], lwd = graphlinewidth)
lines(xx1, indiffcurveB2(xx1), col = COLB[2], lwd = graphlinewidth)
lines(xx1, indiffcurveB3(xx1), col = COLB[2], lwd = graphlinewidth)

#Label B's indifference curves
text(3.6, 10, expression(u[1]^B))
text(1.65, 10, expression(u[2]^B))
text(.55, 10, expression(u[3]^B))

#Add a point for a feasible allocation
points(1, 14, pch = 16, col = "black", cex = 1.5)

#Annotating the feasible allocation
text(.8, 13.5, expression(z))

#Arrow to Slope of BRF
#Arrows(14.2, 0.12, 12.2, 0.12, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#text(16.8, 0.12, expression(paste("Slope = ", q[p])))

#Arrow to Slope of isoprofit
#Arrows(13, 0.80, 15, 0.80, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#text(10.2, 0.80, expression(paste("Slope = ", frac(q, p))))

dev.off()

