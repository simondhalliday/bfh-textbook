require(ggplot2)
require(shape)
pdf(file = "employment/employment_good_bad.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 3

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Parameters for figures
u1 <- 4
u2 <- 10
u3 <- 16
a1 <- 2
a2 <- 2
a3 <- 2

indiffFn1 <- function(e, u1 = 4, a1 = 2) {
  u1 + (a1) / (1 - e)
}

indiffFn2 <- function(e, u2 = 10.5, a2 = 2) {
  u2 + (a2) / (1 - e)
}

indiffFn3 <- function(e, u3 = 16.5, a3 = 2) {
  u3 + (a3) / (1 - e)
}

#The derivative for the function q = 1 - delta/(p - u) = delta/p - u)^2;
#From that we get the derivative and substitute in the relevant values
#for delta, u and p; that is we get a slope of 1/32
#But we also need the intercept; 0.375 above, or q/2 as it stood
tangentLine <- function(w){
  0.375 + (1/32)*w
}

COL <- c("#bae4b3", "#74c476", "#238b45")
par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 1.1)
ylims <- c(0, 40)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("Effort, ", e)),
     ylab = expression(paste("Income, ", y)),
     xaxt = "n", 
     yaxt = "n",
     cex.lab = axislabelsize, 
     #line = 2.5,
     bty = "n", 
     xaxs="i", 
     yaxs="i")

npts <- 501 
xx1 <- seq(xlims[1], xlims[2]-0.15, length.out = npts)
xx2 <- seq(xlims[1], xlims[2]-0.15, length.out = npts)
xx3 <- seq(xlims[1], xlims[2]-0.15, length.out = npts)
xx4 <- seq(7, 17, 0.01)

#Draw the lines for the graphs
lines(xx1, indiffFn1(xx1), col = COLB[3], lwd = graphlinewidth)
lines(xx2, indiffFn2(xx2), col = COLB[3], lwd = graphlinewidth)
lines(xx3, indiffFn3(xx3), col = COLB[3], lwd = graphlinewidth)
#lines(xx4, tangentLine(xx4), col = "darkgrey", lty = 2, lwd = 4)

#Customize ticks and labels for the plot
ticksx <- c(0, 1, 1.1)
xlabels <- c(0, 1, NA)
ticksy <- c(0, 40)
ylabels <- c(0, NA)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Annotation of the three indifference curves
text(6.8, 0.05, expression(paste(u[1])))
text(13.2, 0.05, expression(paste(u[2])))
text(19.2, 0.05, expression(paste(u[3])))

#Line for the max quality, q = 1 
segments(1, 0, 1, 40, lty = 2, col = "darkgray", lwd = 2)
#Annotating line of 100% quality
text(20, 1.025, expression(paste("Maximum level of effort, ", bar(e))))

#Add a point for the tangency
points(12, 0.75, pch = 16, col = "black", cex = 1.5)

#Arrow to Slope of BRF
Arrows(10, 0.8, 12, 0.8, col = "black", lty = 1, lwd = 1.5, arr.type = "triangle")
text(5, 0.8, expression(paste("Slope = ", -mrs(y, e) == -frac(1, u[e]))))
text(5.4, 0.73, expression(paste(" = ", frac((1-e)^2, a))))
dev.off()

