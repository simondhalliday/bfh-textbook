#Graph Designer: Simon Halliday, Scott Cohn
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "risk/zhao_rice_problem.pdf", width = 8, height = 6)


#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.75
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 8, 1, 1))

return <- function(d){
  400 + 128*d - d^2
}

uA <- function(d, h){
  h - 9*d - 7*d^2
}

uA2 <- function(d, h){
  h - 9*d - 0.75*d^2
}

indiffA <- function(d, int = 3596) {
  int + 9*d + 0.75*d^2
}

insurance <- function(d, intercept = 3130, slope = 23.5){
  intercept  + slope*d
}

ff <- function(x, c = 12, s = 1/3){
  c - s*x
}

xlims <- c(0, 128)
ylims <- c(0, 5000)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(uA2(d = 34, h = return(d=34)) - 800, uA2(d = 34, h = return(d=34)), uA2(d = 34, h = return(d=34)) + 800)

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



ticksy <- c(ylims[1], uA2(d = 34, h = 3596), return(d = 34), return(d = 64), ylims[2])
#ylabels <- c(NA, expression(paste(hat(y)[a] == 1.25)), NA)
ylabels <- c(NA, expression(paste(h[c]^Z)== u[2]), expression(paste(h[b]^{Z})), expression(paste(h[n])), NA)
#ylabels <- c(NA, expression(paste(-(1 + rho))), expression(paste(y[a] == frac(q,4))), NA)
ticksx <- c(xlims[1], 34, 64, xlims[2])
xlabels <- c(NA, expression(paste(Delta[b] == 34)), expression(paste(Delta[n] == 64)), NA)

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
lines(xx1, insurance(xx1), col = CBCols[3], lwd = graphlinewidth)
# lines(xx1, indiffA(xx1, int = a[1]), col = CBCols[2], lwd = graphlinewidth)
# lines(xx1, indiffA(xx1, int = a[3]), col = CBCols[2], lwd = graphlinewidth)

contour(x, y,
        outer(x, y, uA2),
        drawlabels = FALSE,
        col = COLB[4],
        lwd = graphlinewidth,
        levels = a,
        xaxs="i",
        yaxs="i",
        add = TRUE)


#Axis labels
#mtext(expression(paste("Risk, ", f)), side = 1, line = 2.5, cex = axislabelsize)
text(0.5*(xlims[2]), -700, expression(paste("Risk, ", Delta)), xpd = TRUE, cex = axislabelsize) 
#text(0.5*(xlims[2]), -0.1, expression(paste(f[a] == frac(1, 2))), xpd = TRUE, cex = labelsize) 

text(-25, 0.45*(ylims[2]), expression(paste("Expected harvest, ", h )), xpd = TRUE, cex = axislabelsize, srt = 90) 
#text(-0.02, -0.03, expression(paste(0)), xpd = TRUE, cex = labelsize) 

# segments(0.5, 0, 0.5, return(0.5), lty = 2, col = grays[20] , lwd = segmentlinewidth)
# segments(0, return(0.5), 0.5, return(0.5), lty = 2, col = grays[20] , lwd = segmentlinewidth)

segments(0, return(64), 64, return(64), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(64, 0, 64, return(64), lty = 2, col = "gray" , lwd = segmentlinewidth)

segments(0, return(34), 34, return(34), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(34, 0, 34, return(34), lty = 2, col = "gray" , lwd = segmentlinewidth)


#Annotate points 
text(64, return(64) + 200, expression(paste(n)), cex = labelsize)
points(64, return(64), pch = 16, col = "black", cex = 1.5)

text(34, return(34) + 200, expression(paste(b)), cex = labelsize)
points(34, return(34), pch = 16, col = "black", cex = 1.5)

#point for insurance
text(52, return(52) + 200, expression(paste(i[1])), cex = labelsize)
points(52, return(52), pch = 16, col = "black", cex = 1.5)

#point for insurance
text(9.5, 3350 - 200, expression(paste(i[2])), cex = labelsize)
points(9.5, 3350, pch = 16, col = "black", cex = 1.5)


text(90, 4800, expression(paste("insurance")), cex = labelsize)
text(90, 4500, expression(paste("line")), cex = labelsize)

text(56, 4800, expression(paste(u[1])), cex = labelsize)
text(46, 4800, expression(paste(u[2])), cex = labelsize)
text(36, 4800, expression(paste(u[3])), cex = labelsize)


dev.off()