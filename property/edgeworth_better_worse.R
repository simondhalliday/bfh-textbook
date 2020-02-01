require(shape)
pdf(file = "property/edgeworth_better_worse.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

indiffcurveA1 <- function(x, U = 4, A = 1, a = 0.5) {
  ((((U-2)/A)*(1/x)^a)^(1/(1-a)))
}

indiffcurveA2 <- function(x, U = 4, A = 1, a = 0.5) {
  (((U/A)*(1/x)^a)^(1/(1-a)))
}

indiffcurveA3 <- function(x, U = 4, A = 1, a = 0.5) {
  ((((U+2)/A)*(1/x)^a)^(1/(1-a)))
}

#Aisha happens to have found 8 coffee and 2 data, 
#and Betty happens to have found 2 coffee and 13 data. 
#Aisha's utility (8^0.5)*(2^0.5) = 4
#Betty's utility (2^0.5)*(13^0.5) = 5.09

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 4, 4, 4))
xlims <- c(0, 10)
ylims <- c(0, 15)
xlims2 <- c(10, 0)
ylims2 <- c(15, 0)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = "",
     ylab = "", 
     line = 2.5,
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

mtext(expression(paste("A's coffee (kilograms), ", x^A)), side=1, line = 2.9, cex = axislabelsize)
text(-0.8, 6, expression(paste("A's data (gigabytes), ", y^A)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Add arrows:
arrows(-0.75, 11, -0.75, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(7, -1.8, 9, -1.8, xpd = TRUE, length=0.1,angle=40,lwd=3)


npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
#xx2 <- seq(xlims[1], xlims[2], length.out = npts)
#xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
#xx4 <- seq(xlims[1], 10, length.out = npts2)

#Draw the lines for the graphs
#lines(xx1, indiffcurveA1(xx1), col = COLA[3], lwd = 4)
#lines(xx1, indiffcurveA2(xx1), col = COLA[3], lwd = 4)
#lines(xx1, indiffcurveA3(xx1), col = COLA[3], lwd = 4)
#lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 4)
#lines(xx2, solowInfeas(xx2, delta = 5), col = COL[1], lwd = 4, lty = 2)

#Customize ticks and labels for the plot
ticksy <- seq(from = 0, to = 15, by = 1)
ylabels <- seq(from = 0, to = 15, by = 1)
ticksx <- seq(from = 0, to = 10, by = 1)
xlabels <- seq(from = 0, to = 10, by = 1)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0)


text(-0.5, -1.4, expression("Ayanda"), xpd = TRUE, cex = namesize, col = COLA[4])
text(10.4, 16.4, expression("Biko"), xpd = TRUE, cex = namesize, col = COLB[4])


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
#axis(side=3, at = xlims2, pos = 0)

text(4.5, -1.6, expression(paste("B's coffee (kilograms), ", x^B)), xpd = TRUE, cex = axislabelsize)
text(-0.8, 7, expression(paste("B's data (gigabytes), ", y^B)), xpd = TRUE, cex = axislabelsize, srt = 270) 

#Add arrows:
arrows(-0.7, 11, -0.7, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(6.9, -1.5, 9, -1.5, xpd = TRUE, length=0.1,angle=40,lwd=3)


indiffcurveB1 <- function(x, U = 5.09, A = 1, a = 0.5) {
  ((((U-2)/A)*(1/x)^a)^(1/(1-a)))
}

indiffcurveB2 <- function(x, U = 5.09, A = 1, a = 0.5) {
  (((U/A)*(1/x)^a)^(1/(1-a)))
}

indiffcurveB3 <- function(x, U = 5.09, A = 1, a = 0.5) {
  ((((U+2)/A)*(1/x)^a)^(1/(1-a)))
}

#lines(xx1, indiffcurveB1(xx1), col = COLB[3], lwd = 4)
#lines(xx1, indiffcurveB2(xx1), col = COLB[3], lwd = 4)
#lines(xx1, indiffcurveB3(xx1), col = COLB[3], lwd = 4)

#Label B's indifference curves
#text(9.1, 1.6, expression(u[1]^B))
#text(9.1, 3.4, expression(u[2]^B))
#text(9.1, 6.2, expression(u[3]^B))

#Add a point for the initial endowment
#points(2, 13, pch = 16, col = "black", cex = 1.5)


#Arrow to Slope of BRF
Arrows(5.25, 7.875, 9, 13.5, lty = 1, lwd = 2, arr.type = "triangle",  col = COLB[3])
Arrows(4.75, 7.125, 1, 1.5, lty = 1, lwd = 2, arr.type = "triangle",  col = COLA[3])

text(7.7, 9, expression(paste("Better")), cex = labelsize)
text(7.7, 10, expression(paste("for Biko")), cex = labelsize)
text(2.5, 5, expression(paste("Better")), cex = labelsize)
text(2.5, 6, expression(paste("for Ayanda")), cex = labelsize)

#Arrow to Slope of isoprofit
#Arrows(13, 0.80, 15, 0.80, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#text(10.2, 0.80, expression(paste("Slope = ", frac(q, p))))

dev.off()

