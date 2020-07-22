require(shape)
pdf(file = "property/qql_y_indiff.pdf", width = 9, height = 7)

#Set parameters for graphics
namesize <- 1.3
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5


COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")
Grays <- gray.colors(25, start = 1, end = 0)

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 7.5, 1, 1))

mrsA <- function(x, rmax = 2, xmax = 12) {
  rmax - (rmax/xmax)*x
}


uA <- function(x, y, rmax = 2, xmax = 12) {
  y + rmax*x - (1/2)*(rmax/xmax)*x^2
}

indiffA <- function(x, uA = 10, rmax = 2, xmax = 12) {
  uA - rmax*x + (1/2)*(rmax/xmax)*(x^2)
}

bcA <- function(x, w = 20, p = 2) {
  w - p*x
}


xlims <- c(0, 12)
ylims <- c(0, 18)

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
     bty = "n",
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
# ticksx <- seq(from = 0, to = xlims[2], by = 2)
# xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(0, 13, 15, 17, ylims[2])
ylabels <- c(0, expression(paste(y[1] == u[1]^B)), expression(paste(y[2] == u[2]^B)), expression(paste(y[3] == u[3]^B)), NA)
ticksx <- c(0,  xlims[2])
xlabels <- c(0, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = axislabelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = axislabelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#lines(xx1, bcA(xx1, w = 12, p = 1), col = COLB[3], lwd = graphlinewidth)
#lines(xx1, indiffA1(xx1, uA = 20, rmax = 2.5, xmax = 10), col = COLB[4], lwd = graphlinewidth)

mtext(expression(paste("Hours of Living, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-2.2, 10, expression(paste("Quantity of money, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
#text(7.3, 3, expression("Pareto Efficient"))
#text(7.3, 2.5, expression("Curve"))
#Arrows(7.3, 3.5, 7.3, 6.1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the iso-welfare functions for the HG, Aisha
text(11.8, 1.7, expression(u[1]^B), cex = annotatesize)
text(11.8, 3.7, expression(u[2]^B), cex = annotatesize) 
text(11.8, 5.7, expression(u[3]^B), cex = annotatesize)
#text(6.6, 8.3, expression(u[4]^A))

#Label the indifference curves for the HG, Betty
# text(7.6, 17, expression(u[1]^B))
# text(6.75, 17, expression(u[2]^B))
# text(6, 17, expression(u[3]^B))
#text(3.4, 6.9, expression(v[4]^B))

#Label mrs function
#text(10, 8, expression(paste(mrs(x,y) == r[max] - frac(r[max], x[max])*x)))
#Arrows(10, 7.5, 10, 5.6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label satiation
#text(20, 4, expression(paste(x[max] == "Point")))
#text(20, 3, expression(paste("of Satiation")))
#Arrows(20, 2.5, 20, 0.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label highest willingness to pay
# text(5, 10.5, expression(paste(r[max] == "Maximum")))
# text(5, 9.5, expression(paste("Willingness to Pay")))
# Arrows(3, 10, 0.5, 10, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLA[3],
        #cex.lab = axislabelsize,
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE)

#segments(0, 6, 6, 6, lty = 2, col = "gray" , lwd = segmentlinewidth)
#segments(6, 0, 6, 6, lty = 2, col = "gray" , lwd = segmentlinewidth)


slopeline <- function(x, yint, slope = 1.5){
  yint - slope*x
}

slopex1 <- seq(1.5,4.5,length.out = 200)
lines(slopex1, slopeline(slopex1, yint = 12.2, slope = mrsA(x = 3)), col = Grays[21], lty = 2, lwd = graphlinewidth)
lines(slopex1, slopeline(slopex1, yint = 14.2, slope = mrsA(x = 3)), col = Grays[21], lty = 2, lwd = graphlinewidth)
lines(slopex1, slopeline(slopex1, yint = 16.2, slope = mrsA(x = 3)), col = Grays[21], lty = 2, lwd = graphlinewidth)

slopex2 <- seq(7.6,10.5,length.out = 200)
lines(slopex2, slopeline(slopex2, yint = 6.2, slope = mrsA(x = 9)), col = Grays[21], lty = 2, lwd = graphlinewidth)
lines(slopex2, slopeline(slopex2, yint = 8.2, slope = mrsA(x = 9)), col = Grays[21], lty = 2, lwd = graphlinewidth)
lines(slopex2, slopeline(slopex2, yint = 10.2, slope = mrsA(x = 9)), col = Grays[21], lty = 2, lwd = graphlinewidth)


firstpointsx <- c(3, 3, 3)
firstpointsy <- c(indiffA(x = firstpointsx[1], uA = 13), indiffA(x = firstpointsx[2], uA = 15), indiffA(x = firstpointsx[3], uA = 17))
points(firstpointsx, firstpointsy, pch = 16, col = "black", cex = 1.5)

secondpointsx <- c(9, 9, 9)
secondpointsy <- c(indiffA(x = 9, uA = 13), indiffA(x = 9, uA = 15), indiffA(x = 9, uA = 17))
points(secondpointsx, secondpointsy, pch = 16, col = "black", cex = 1.5)


# par(new = TRUE)
# 
# #Use the same x and ylims as previously, but with locations switched
# xlims2 <- c(18, 0)
# ylims2 <- c(18, 0)

#Leave the ylab and xlab blank to ensure no axes titles
# plot(0, 0, xlim = xlims2, ylim = ylims2, type = "n",
#      xlab = expression(paste("")),
#      ylab = expression(paste("")),
#      xaxt = "n", 
#      yaxt = "n", 
#      cex.lab = 1.3, 
#      bty = "n",
#      xaxs="i", 
#      yaxs="i")
# 
# 
# lines(xx1, indiffAlow(xx1, uA = 46.08, alpha = 16, beta = 1/24), col = COL[2], lwd = graphlinewidth)


#Set up axes at sides 3 and 4 (top and right)
# axis(side = 3, at = ticksx, pos = 0, labels = xlabels)
# axis(side = 4, at = ticksy, pos = 0, labels = ylabels, las = 0)
# mtext("B's Apples, x", side=3, line = 2.5, cex = axislabelsize)
# text(-0.8, 7, expression(paste("B's Oranges, y")), xpd = TRUE, cex = axislabelsize, srt = 270) 
# 
# #Add arrows:
# arrows(-0.8, 10, -0.8, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
# arrows(6.2, -1.8, 9, -1.8, xpd = TRUE, length=0.1,angle=40,lwd=3)
# 

#Customize ticks and labels for the plot
#ticksy <- seq(from = 0, to = 15, by = 1)
#ylabels <- seq(from = 0, to = 15, by = 1)
#ticksx <- seq(from = 0, to = 10, by = 1)
#xlabels <- seq(from = 0, to = 10, by = 1)
#axis(1, at = ticksx, pos = 0, labels = xlabels)
#axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0)

dev.off()
