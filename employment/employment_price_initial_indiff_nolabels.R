require(shape)
pdf(file = "employment/employment_price_initial_indiff_nolabels.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

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

PCFn <- function(delta, mu = 16) {
  delta/mu
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
xlims <- c(0, 10)
ylims <- c(0, 1.05)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("Price, ", p)),
     ylab = expression(paste("Effort, ", e)),
     xaxt = "n", 
     yaxt = "n",
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i")


# xpoly1 <- seq(from = xlims[1] + 0.2, to = 8, length.out = 500)
# ypoly1 <- PCFn(xpoly1, mu = 8)
# polygon(x = c(xpoly1, rev(xpoly1[1])), y = c(ypoly1, rev(ypoly1)[1]), col=COLA[1], density=NULL, border = NA)
# 

npts <- 500 
xx1 <- seq(u1 + a1 + 0.1, xlims[2], length.out = npts)
xx2 <- seq(u2 + a2 + 0.1, xlims[2], length.out = npts)
xx3 <- seq(3, xlims[2], length.out = npts)
xx4 <- seq(7, 17, 0.01)
xx5 <- seq(xlims[1] + 0.2, xlims[2], length.out = npts)
xx6 <- seq(xlims[1], 20, length.out = npts)


#Draw the lines for the graphs
#lines(xx5, PCFn(xx5, mu = 8), col = COLA[4], lwd = graphlinewidth)
lines(xx5, indiffFn1(xx5, u = 0), col = COLA[4], lwd = graphlinewidth)
lines(xx5, indiffFn1(xx5, u = -1.5), col = COLA[4], lwd = graphlinewidth)
lines(xx3, indiffFn1(xx3, u = 1.5), col = COLA[4], lwd = graphlinewidth)


#Customize ticks and labels for the plot
ticksy <- c(0,  1, ylims[2])
ylabels <- c(0,  expression(paste(bar(e) == 1) ), NA)
ticksx <- c(0, xlims[2])
xlabels <- c(0,  NA)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Annotation of the three indifference curves
# text(0.8, 0.05, expression(paste(u[1])))
# text(2.65, 0.05, expression(paste(u[2] == u[z], phantom() == 0)))
# text(3.8, 0.05, expression(paste(u[3])))

#Line for the max quality, q = 1 
segments(0, 1, xlims[2], 1, lty = 2, col = "darkgray", lwd = 2)
#Annotating line of 100% quality
#text(1.4, 1.025, expression(paste("Maximum level of effort, ", bar(e))))

#Add a point for the tangency
# segments(4, 0, 4, PCFn(delta = 4, mu = 8), lty = 2, col = "darkgray", lwd = segmentlinewidth)
# segments(0, PCFn(delta = 4, mu = 8), 4, PCFn(delta = 4, mu = 8), lty = 2, col = "darkgray", lwd = segmentlinewidth)
# points(4, PCFn(delta = 4, mu = 8), pch = 16, col = "black", cex = 1.5)
# text(4, PCFn(delta = 4, mu = 8) + 0.03, expression(paste(c)))


#Arrow to Slope of production function
# Arrows(8, 0.9, 18, 0.9, col = "black", lty = 1, lwd = 1.5, arr.type = "triangle")
# text(5, 0.93, expression(paste("Slope = ", frac(1, y[e]))))
# text(6.4, 0.83, expression(paste(" = ", frac(e^0.5, phi))))
#text(6.2, 0.83, expression(paste(" = ", frac(y, 2*phi^2))))
# text(20.8, 0.96, expression(paste(y(e))))

# Arrows(19, 0.6, 16.5, 0.6, col = "black", lty = 1, lwd = 1.5, arr.type = "triangle")
# text(21, 0.6, expression(paste(-frac(1, y[e]) == frac(u[y],u[e]))))

# text(4, 0.5, expression(paste("Feasible")))
# text(4, 0.45, expression(paste("set")))



#Label the feasible frontier
# text(2, 0.75, expression("Better for"), cex = labelsize)
# text(2, 0.7, expression("employer"), cex = labelsize)
# text(3.2, 0.5, expression("(production possibilities frontier)"), cex = labelsize)
# Arrows(4.35, 0.95, 8.1, 0.95, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

# text(8, 0.25, expression("Better for"), cex = labelsize)
# text(8, 0.2, expression("employee"), cex = labelsize)


# text(4, 0.97, expression(paste("Slope of iso-profit")), cex = labelsize)
# text(4, 0.89, expression(paste(-mrt(p,e) == frac(de,dp),phantom()==frac(e, p))), cex = labelsize)
# Arrows(5.1, 0.89, 6.9, 0.89, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(8, 0.5, expression(paste(mrs(p,e) == frac(u[p],u[e]) )), cex = labelsize)
Arrows(6.4, 0.5, 4.25, 0.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


dev.off()