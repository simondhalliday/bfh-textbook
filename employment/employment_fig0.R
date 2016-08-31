require(shape)
pdf(file = "employment/employment_fig0.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 3

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Parameters for figures
u1 <- 4
u2 <- 10
u3 <- 16
delta1 <- 2
delta2 <- 2
delta3 <- 2

indiffFn1 <- function(w, u1 = 4, delta1 = 2) {
  1 - (delta1) / (w - u1)
}

indiffFn2 <- function(w, u2 = 10, delta2 = 2) {
  1 - (delta2) / (w - u2)
}

indiffFn3 <- function(w, u3 = 16, delta3 = 2) {
  1 - (delta3) / (w - u3)
}

#The derivative for the function q = 1 - delta/(p - u) = delta/p - u)^2;
#From that we get the derivative and substitute in the relevant values
#for delta, u and p; that is we get a slope of 1/32
#But we also need the intercept; 0.375 above, or q/2 as it stood
tangentLine <- function(w){
  0.375 + (1/32)*w
}

par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 40)
ylims <- c(0, 1.1)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("Income, ", y)),
     ylab = expression(paste("Effort, ", e)),
     xaxt = "n", 
     yaxt = "n",
     cex.lab = axislabelsize, 
     line = 2.5,
     bty = "n", 
     xaxs="i", 
     yaxs="i")

npts <- 500 
xx1 <- seq(u1 + delta1, xlims[2], length.out = npts)
xx2 <- seq(u2 + delta2, xlims[2], length.out = npts)
xx3 <- seq(u3 + delta3, xlims[2], length.out = npts)
xx4 <- seq(7, 17, 0.01)

#Draw the lines for the graphs
lines(xx1, indiffFn1(xx1), col = COLB[4], lwd = graphlinewidth)
lines(xx2, indiffFn2(xx2), col = COLB[4], lwd = graphlinewidth)
lines(xx3, indiffFn3(xx3), col = COLB[4], lwd = graphlinewidth)
lines(xx4, tangentLine(xx4), col = "darkgrey", lty = 2, lwd = 4)

#Customize ticks and labels for the plot
ticksy <- c(0, 1, 1.1)
ylabels <- c(0, 1, NA)
ticksx <- c(0, 40)
xlabels <- c(0, 40)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Annotation of the three indifference curves
text(7, 0.05, expression(paste(u[1])))
text(13, 0.05, expression(paste(u[2])))
text(19, 0.05, expression(paste(u[3])))

#Line for the max quality, q = 1 
segments(0, 1, 40, 1, lty = 2, col = "darkgray", lwd = 2)
#Annotating line of 100% quality
text(20, 1.025, expression(paste("Maximum level of effort, ", bar(e))))

#Add a point for the tangency
points(12, 0.75, pch = 16, col = "black", cex = 1.5)

#Arrow to Slope of BRF
Arrows(10, 0.8, 12, 0.8, col = "black", lty = 1, lwd = 1.5, arr.type = "triangle")
text(5, 0.8, expression(paste("Slope = -mrs(y,e) = ", -frac(1, u[e]))))
text(5.4, 0.73, expression(paste(" = ", frac((1-e)^2, a))))
dev.off()

