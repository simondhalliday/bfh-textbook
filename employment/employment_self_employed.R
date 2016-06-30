require(shape)
pdf(file = "employment/employment_self_employed.pdf", width = 9, height = 7)

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

indiffFn1 <- function(y, u1 = 4.5, a1 = 2) {
  1 - (a1) / (y - u1)
}

indiffFn2 <- function(y, u2 = 10.5, a2 = 2) {
  1 - (a2) / (y - u2)
}

indiffFn3 <- function(y, u3 = 16.5, a3 = 2) {
  1 - (a3) / (y - u3)
}

prodFn1 <- function(y, a = 5){
  exp((y-1)/a)
}

prodFn2 <- function(y, a = 10){
  (y/(2*a))^2
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


xpoly1 <- seq(from = xlims[1], to = 20, length.out = 500)
ypoly1 <- prodFn2(xpoly1)
polygon(x = c(xpoly1, rev(xpoly1[1])), y = c(ypoly1, rev(ypoly1)[1]), col=COLA[1], density=NULL, border = NA)


npts <- 500 
xx1 <- seq(u1 + a1 + 0.1, xlims[2], length.out = npts)
xx2 <- seq(u2 + a2 + 0.1, xlims[2], length.out = npts)
xx3 <- seq(u3 + a3 + 0.1, xlims[2], length.out = npts)
xx4 <- seq(7, 17, 0.01)
xx5 <- seq(xlims[1], xlims[2], length.out = npts)
xx6 <- seq(xlims[1], 20, length.out = npts)

#Draw the lines for the graphs
lines(xx1, indiffFn1(xx1), col = COLB[4], lwd = graphlinewidth)
lines(xx2, indiffFn2(xx2), col = COLB[4], lwd = graphlinewidth)
#lines(xx3, indiffFn3(xx3), col = COL[2], lwd = graphlinewidth)
#lines(xx4, tangentLine(xx4), col = "darkgrey", lty = 2, lwd = 4)
lines(xx6, prodFn2(xx6), col = COLA[4], lwd = graphlinewidth)

#Customize ticks and labels for the plot
ticksy <- c(0, prodFn2(y = 15.5), 1, 1.1)
ylabels <- c(0, expression(paste(e^S)), 1, NA)
ticksx <- c(0, 15.5, 40)
xlabels <- c(0, expression(paste(y^S)), NA)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Annotation of the three indifference curves
text(7.2, 0.05, expression(paste(u[1])))
text(13.2, 0.05, expression(paste(u[2])))
#text(19, 0.05, expression(paste(u[3])))

#Line for the max quality, q = 1 
segments(0, 1, 40, 1, lty = 2, col = "darkgray", lwd = 2)
#Annotating line of 100% quality
text(30, 1.025, expression(paste("Maximum level of effort, ", bar(e))))

#Add a point for the tangency
segments(15.5, 0, 15.5, prodFn2(y = 15.5), lty = 2, col = "darkgray", lwd = 2)
segments(0, prodFn2(y = 15.5), 15.5, prodFn2(y = 15.5), lty = 2, col = "darkgray", lwd = 2)
points(15.5, prodFn2(y = 15.5), pch = 16, col = "black", cex = 1.5)
text(15, prodFn2(y = 15.5) + 0.02, expression(paste(c)))


#Arrow to Slope of production function
Arrows(8, 0.9, 18, 0.9, col = "black", lty = 1, lwd = 1.5, arr.type = "triangle")
text(5, 0.93, expression(paste("Slope = ", frac(1, y[e]))))
text(6.4, 0.83, expression(paste(" = ", frac(e^0.5, phi))))
#text(6.2, 0.83, expression(paste(" = ", frac(y, 2*phi^2))))
text(20.8, 0.96, expression(paste(y(e))))

Arrows(19, 0.6, 16.5, 0.6, col = "black", lty = 1, lwd = 1.5, arr.type = "triangle")
text(21, 0.6, expression(paste(-frac(1, y[e]) == frac(u[y],u[e]))))

text(4, 0.5, expression(paste("Feasible")))
text(4, 0.45, expression(paste("set")))

#Arrow to Slope of BRF
Arrows(30, 0.73, 30, 0.85, col = "black", lty = 1, lwd = 1.5, arr.type = "triangle")
text(30, 0.7, expression(paste("Slope = ", -mrs(y, e) == -frac(1, u[e]))))
text(30.4, 0.63, expression(paste(" = ", frac((1-e)^2, a))))


dev.off()


