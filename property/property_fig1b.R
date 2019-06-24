require(ggplot2)
require(shape)
pdf(file = "property/property_fig1b.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 3
namesize <- 1.3

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

uA <- function(x, y, alpha = 0.5, lambda = 0.4) {
  (x^(alpha)*y^(1 - alpha))^(1-lambda)*((10 - x)^(alpha)*(15 - y)^(1 - alpha))^lambda
}

indiffcurveB1 <- function(x, U = 3.741657, A = 1, a = 0.5) {
  ((((U-2)/A)*(1/x)^a)^(1/(1-a)))
}

indiffcurveB2 <- function(x, U = 3.741657, A = 1, a = 0.5) {
  (((U/A)*(1/x)^a)^(1/(1-a)))
}

indiffcurveB3 <- function(x, U = 3.741657, A = 1, a = 0.5) {
  ((((U+2)/A)*(1/x)^a)^(1/(1-a)))
}

#Aisha happens to have found 8 coffee and 2 data, 
#and Betty happens to have found 2 coffee and 13 data. 
#Aisha's utility (8^0.5)*(2^0.5) = 4
#Betty's utility (2^0.5)*(13^0.5) = 5.09

#COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
#COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")
COL <- c("#fc9272", "#fb6a4a", "#ef3b2c","#cb181d", "#99000d")
par(mar =  c(4, 4, 2, 2))
xlims <- c(0, 10)
ylims <- c(0, 15)



plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = "",
     ylab = "",
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n")

mtext(expression(paste("B's coffee (kilograms), ", x^B)), side=1, line = 2.5, cex = axislabelsize)
text(-1, 7, expression(paste("B's data (gigabytes), ", y^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 


npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
#xx2 <- seq(xlims[1], xlims[2], length.out = npts)
#xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
#xx4 <- seq(xlims[1], 10, length.out = npts2)

#Draw the lines for the graphs
lines(xx1, indiffcurveB1(xx1), col = COLB[2], lwd = graphlinewidth)
lines(xx1, indiffcurveB2(xx1), col = COLB[2], lwd = graphlinewidth)
lines(xx1, indiffcurveB3(xx1), col = COLB[2], lwd = graphlinewidth)
#lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 4)
#lines(xx2, solowInfeas(xx2, delta = 5), col = COL[1], lwd = 4, lty = 2)

#Customize ticks and labels for the plot
ticksy <- seq(from = 0, to = 15, by = 1)
ylabels <- seq(from = 0, to = 15, by = 1)
ticksx <- seq(from = 0, to = 10, by = 1)
xlabels <- seq(from = 0, to = 10, by = 1)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Annotation of the three graphs and the NE
text(9, 0.7, expression(u[1]^B))
text(9, 2, expression(u[2]^B))
text(9, 4.1, expression(u[3]^B))
#text(16, 0.52, expression(paste("Nash Equilibrium")))

#Line to label B's endowment
segments(0, 14, 1, 14, lty = 2, col = "darkgray", lwd = 2)
segments(1, 0, 1, 14, lty = 2, col = "darkgray", lwd = 2)

#Add a point for Aisha's endowment
points(1, 14, pch = 16, col = "black", cex = 1.5)

#Annotating B's endowment
text(1.7, 14, expression(z == (list(x[z]^B, y[z]^B))))


text(-0.5, -1.4, expression("Bongani"), xpd = TRUE, cex = namesize, col = COLB[4])
#text(10.4, 16.4, expression("Bongani"), xpd = TRUE, cex = namesize, col = COLB[4])

#Arrow to Slope of BRF
#Arrows(14.2, 0.12, 12.2, 0.12, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#text(16.8, 0.12, expression(paste("Slope = ", q[p])))

#Arrow to Slope of isoprofit
#Arrows(13, 0.80, 15, 0.80, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#text(10.2, 0.80, expression(paste("Slope = ", frac(q, p))))

dev.off()

