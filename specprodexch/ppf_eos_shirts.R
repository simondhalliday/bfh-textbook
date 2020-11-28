#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "specprodexch/ppf_eos_shirts.pdf", width = 9, height = 9)

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
par(mar =  c(1.2, 1.2, 0.5, 0.5))

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
  k * (l)^alpha
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

ticksy <- c( 0, 2.5, 10, ylims[2])
ylabels <- c(NA,2.5, 10, NA)
ticksx <- c(0, 5, 10, xlims[2])
xlabels <- c(NA,5, 10, NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = annotatesize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1,cex.axis = annotatesize)

npts <- 500 
xx1 <- seq(0, 5, length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(0, 11, length.out = npts)


#Draw the graphs
lines(xx4, manufactureProd(xx4, k = 0.1, alpha = 2), col = COLB[4], lwd = graphlinewidth)

#mtext(expression(paste("Quantity of fish, ", x)), side=1, line = 2.5, cex = axislabelsize)
text(-0.5, 8, expression(paste("Shirts, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(8, -0.5, expression(paste("Labor for shirts, ", l^s)), xpd = TRUE, cex =axislabelsize)


#Label the two production functions
#Clothing
text(3.5, 5.5, expression(paste("Shirt production")), xpd = TRUE, cex = annotatesize)
text(3.5, 4.8, expression(paste(y == frac(1,10)(l^s)^2)), xpd = TRUE, cex = annotatesize)
Arrows(4.6, 4.8, 6.5, 4.8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Draw segments for the 50/50 split of time
segments(5, 0, 5, 2.5, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, 2.5, 5, 2.5, lty = 2, col = grays[20], lwd = segmentlinewidth)

#Annotate Max time on clothes
segments(10, 0, 10, 10, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(10, 10, 0, 10, lty = 2, col = grays[20], lwd = segmentlinewidth)
text(6.5, 9.5, expression(paste("10 hrs of labor")), cex = annotatesize)
text(6.5, 9.1, expression(paste("for shirts produces")),  cex = annotatesize)
text(6.5, 8.7, expression(paste("10 shirts")),  cex = annotatesize)
points(10, 10, pch = 16, col = "black", cex = 1.5)
text(9.75, 10.2, expression(paste("a")),  cex = annotatesize)

points(5, 2.5, pch = 16, col = "black", cex = 1.5)
text(4.9, 2.8, expression(paste("e")), cex = annotatesize)




dev.off()
