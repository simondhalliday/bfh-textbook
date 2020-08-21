require(shape)
pdf(file = "property/qql_y_indiff_new.pdf", width = 9, height = 7)

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
grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 7.5, 1, 1))

mrsA <- function(x, rmax = 32, xmax = 16) {
  rmax - (rmax/xmax)*x
}

uA <- function(x, y, rmax = 32, xmax = 16) {
  (y) + rmax*(x) - (1/2)*(rmax/xmax)*(x)^2
}

indiffA <- function(x, uA = 256, rmax = 32, xmax = 16) {
  uA - rmax*(x) + (1/2)*(rmax/xmax)*(x)^2
}

bcA <- function(x, w = 20, p = 2) {
  w - p*x
}


xlims <- c(0, 16)
ylims <- c(0, 400)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(260, 340, 400)

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
ticksy <- c(0, a[1], a[2], a[3], ylims[2])
ylabels <- c(0, expression(paste(y[1] == 260)), expression(paste(y[2] == 340)), expression(paste(y[3] == 400)), NA)
ticksx <- seq(0, xlims[2], 2)
xlabels <- seq(0, xlims[2], 2)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#lines(xx1, bcA(xx1, w = 12, p = 1), col = COLB[3], lwd = graphlinewidth)
#lines(xx1, indiffA1(xx1, uA = 20, rmax = 2.5, xmax = 10), col = COLB[4], lwd = graphlinewidth)

#mtext(expression(paste("Hours of Living, ", x)), side=1, line = 2.5, cex = axislabelsize)
text( 0.5*xlims[2],-38, expression(paste("Hours of Living, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-2.9, 0.5*ylims[2], expression(paste("Quantity of money, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
#text(7.3, 3, expression("Pareto Efficient"))
#text(7.3, 2.5, expression("Curve"))
#Arrows(7.3, 3.5, 7.3, 6.1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the iso-welfare functions for the HG, Aisha
text(15.5, indiffA(x = 15.5, uA = a[1]) + 15, expression(u[1]^B), cex = annotatesize)
text(15.5, indiffA(x = 15.5, uA = a[2]) + 15, expression(u[2]^B), cex = annotatesize) 
text(15.5, indiffA(x = 15.5, uA = a[3]) + 15, expression(u[3]^B), cex = annotatesize)
#text(6.6, 8.3, expression(u[4]^A))

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
        col = CBCols[1],
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

# #segment at 2 hours of work
# segments(2, 0, 2, ylims[2], lty = 2, col = grays[20] , lwd = segmentlinewidth)
# #segment at 10 hours of work


# segments(8, 0, 8, indiffA(x = secondpointsx[2], uA = a[2]), lty = 2, col = grays[20] , lwd = segmentlinewidth)
# segments(0, indiffA(x = secondpointsx[2], uA = a[2]), 8, indiffA(x = secondpointsx[2], uA = a[2]), lty = 2, col = grays[20] , lwd = segmentlinewidth)


# slopex1 <- seq(0.5,3.5,length.out = 200)
# lines(slopex1, slopeline(slopex1, yint = a[1] - 5, slope = mrsA(x = 2)), col = Grays[21], lty = 2, lwd = graphlinewidth)
# lines(slopex1, slopeline(slopex1, yint = a[2] - 5, slope = mrsA(x = 2)), col = Grays[21], lty = 2, lwd = graphlinewidth)
# lines(slopex1, slopeline(slopex1, yint = a[3] - 5, slope = mrsA(x = 2)), col = Grays[21], lty = 2, lwd = graphlinewidth)

slopex1 <- seq(6.5,10.5,length.out = 200)
lines(slopex1, slopeline(slopex1, yint = a[1] - 65, slope = mrsA(x = 8)), col = Grays[21], lty = 2, lwd = graphlinewidth)

slopex2 <- seq(6,10,length.out = 200)
lines(slopex2, slopeline(slopex2, yint = a[2] - 65, slope = mrsA(x = 8)), col = Grays[21], lty = 2, lwd = graphlinewidth)
lines(slopex2, slopeline(slopex2, yint = a[3] - 65, slope = mrsA(x = 8)), col = Grays[21], lty = 2, lwd = graphlinewidth)

slopex3 <- seq(1.5,5.5,length.out = 200)
lines(slopex3, slopeline(slopex3, yint = a[1] - 16, slope = mrsA(x = 4)), col = Grays[21], lty = 2, lwd = graphlinewidth)

# firstpointsx <- c(2, 2, 2)
# firstpointsy <- c(indiffA(x = firstpointsx[1], uA = a[1]), indiffA(x = firstpointsx[2], uA = a[2]), indiffA(x = firstpointsx[3], uA = a[3]))
# points(firstpointsx, firstpointsy, pch = 16, col = "black", cex = 1.5)

secondpointsx <- c(8, 8, 8)
secondpointsy <- c(indiffA(x = secondpointsx[1], uA = a[1]), indiffA(x = secondpointsx[2], uA = a[2]), indiffA(x = secondpointsx[3], uA = a[3]))
points(secondpointsx, secondpointsy, pch = 16, col = "black", cex = 1.5)
text(secondpointsx, secondpointsy + 15, c("f", "g", "h"), xpd = TRUE, cex = annotatesize) 


#Point at same height, but different x as 8
points(4, indiffA(x = secondpointsx[2], uA = a[2]), pch = 16, col = "black", cex = 1.5)
text(4, indiffA(x = secondpointsx[2], uA = a[2]) + 15, expression(e), xpd = TRUE, cex = annotatesize) 


dev.off()
