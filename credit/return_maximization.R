#Graph Designer: Simon Halliday, Scott Cohn
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "credit/return_maximization.pdf", width = 8, height = 6)

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
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0)

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 1, 1))

return <- function(f, mu = 5){
  mu*f*(1 - f)
}

indiffA <- function(x, alpha = 0.7, uA = 4) {
  (uA / x^alpha)^(1/(1 - alpha))
}

trsline <- function(x, constant = 10, slope = 4){
  constant - (slope)*x
}

ff <- function(x, c = 12, s = 1/3){
  c - s*x
}

xlims <- c(0, 1)
ylims <- c(0, 1.5)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(2, 4, 6)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs = "i", 
     yaxs = "i"
)



ticksy <- c(ylims[1], return(f = 0.5), ylims[2])
ylabels <- c(NA, expression(paste(y,"*")), NA)
ticksx <- c(xlims[1], 0.5, xlims[2])
xlabels <- c(NA, expression(paste(f,"*")), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(1.5, 2.5, length.out = npts)
xx4 <- seq(3, 5, length.out = npts)
xx5 <- seq(7, 9, length.out = npts)

#Draw the graphs
lines(xx1, return(xx1), col = COLA[5], lwd = graphlinewidth)

#Axis labels
mtext(expression(paste("Risk, ", f)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.08, 0.5*(ylims[2]), expression(paste("Expected income, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

segments(0.5, 0, 0.5, return(0.5), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, return(0.5), 0.5, return(0.5), lty = 2, col = grays[20] , lwd = segmentlinewidth)

#Annotate points (4,4),(2,8),(8,2) on feasibility frontier
text(0.5, return(0.5) + 0.075, expression(paste(a)), cex = labelsize)
points(0.5, return(0.5), pch = 16, col = "black", cex = 1.5)

dev.off()
