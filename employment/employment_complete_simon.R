require(shape)
pdf(file = "employment/employment_price_edv2.pdf", width = 9, height = 7)

#Set parameters for graphics
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

#Parameters for figures
u1 <- 4
u2 <- 10
u3 <- 16
a1 <- 2
a2 <- 2
a3 <- 2

indiffFn1 <- function(y, u1 = 4.5, a1 = 5) {
  1 - (a1) / (y - u1)
}

isovhigh1 <- function(w, delta = 5, v = 30){
  (sqrt(w^2 - 4 * delta * v) - w + 2*v)/( 2 * v )
}

isovlow1 <- function(w, delta = 5, v = 30){
  (-sqrt(w^2 - 4 * delta * v) - w + 2*v)/( 2 * v )
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

solowCondition <- function(w, delta = 5, slope = 4){
  (w*(1/(slope*delta)))
}


par(mar =  c(5, 5, 4, 6))
xlims <- c(0, 40)
ylims <- c(0, 1)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("Price, ", p)),
     ylab = expression(paste("Effort, ", e)),
     xaxt = "n", 
     yaxt = "n",
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i")


xpoly1 <- seq(from = xlims[1] + 0.2, to = 8, length.out = 500)
ypoly1 <- PCFn(xpoly1, mu = 8)
polygon(x = c(xpoly1, rev(xpoly1[1])), y = c(ypoly1, rev(ypoly1)[1]), col=COLB[1], density=NULL, border = NA)


npts <- 500 
xx1 <- seq(u1 + a1 + 0.1, xlims[2], length.out = npts)
xx2 <- seq(u2 + a2 + 0.1, xlims[2], length.out = npts)
xx3 <- seq(20, xlims[2], length.out = npts)
xx4 <- seq(7, 17, 0.01)
xx5 <- seq(5, xlims[2], length.out = npts)
xx6 <- seq(11.9, xlims[2], length.out = npts)
xx7 <- seq(xlims[1], 20, length.out = npts)


#Draw the lines for the graphs
xx3 <- seq(0, xlims[2], length.out = npts2)
xx4 <- seq(xlims[1], 10, length.out = npts2)
lines(xx3, isovhigh1(xx3, v = 0.01, delta = 5), col = COLA[4], lwd = graphlinewidth)
lines(xx4, isovlow1(xx4, v = 0.01, delta = 5), col = COLA[4], lwd = graphlinewidth)

# lines(xx3, isovhigh1(xx3, v = 5, delta = 5), col = COLA[4], lwd = graphlinewidth)
# lines(xx4, isovlow1(xx4, v = 5, delta = 5), col = COLA[4], lwd = graphlinewidth)


#lines(xx7, PCFn(xx7, mu = 28), col = COLB[4], lwd = graphlinewidth)
#lines(xx6, indiffFn1(xx6, u1 = 11.9), col = COLA[4], lwd = graphlinewidth)

solowCondition <- function(w, delta = 5, slope = 4){
  (w*(1/(slope*delta)))
}
xx5 <- seq(5, xlims[2], length.out = npts)
xx7 <- seq(xlims[1], 20, length.out = npts)
lines(xx5, indiffFn1(xx5, u1 = 0), col = COLA[4], lwd = graphlinewidth)
lines(xx7, solowCondition(xx7, delta = 5, slope = 4), col = COLB[4], lwd = graphlinewidth)

#lines(xx3, indiffFn1(xx3, u1 =20), col = COLA[4], lwd = graphlinewidth)
#lines(xx6, prodFn2(xx6), col = COLB[4], lwd = graphlinewidth)
lines(xx7, solowCondition(xx7, delta = 5, slope = 5.8), col = COLB[4], lwd = graphlinewidth)

#Customize ticks and labels for the plot
ticksy <- c(0, PCFn(delta = 4, mu = 8), 1, 1.1)
ylabels <- c(0, expression(paste(e^C)), expression(paste(bar(e) == 1) ), NA)
ticksx <- c(0, 4, xlims[2])
xlabels <- c(0, expression(paste(p^C)), NA)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1,cex.axis = labelsize)

#Annotation of the three indifference curves
text(6.5, 0.075, expression(paste(u[1])), cex = labelsize-0.05)
text(15, 0.075, expression(paste(u[2] == u[z])), cex = labelsize-0.05)
text(15, 0.035, expression(paste(phantom() == 0)), cex = labelsize-0.05)
text(18.5, 0.075, expression(paste(u[3])), cex = labelsize-0.05)

#Line for the max quality, q = 1 
segments(0, 1, xlims[2], 1, lty = 2, col = grays[20], lwd = 2)
#Annotating line of 100% quality
#text(1.4, 1.025, expression(paste("Maximum level of effort, ", bar(e))))

#Add a point for the tangency
segments(4, 0, 4, PCFn(delta = 4, mu = 8), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, PCFn(delta = 4, mu = 8), 4, PCFn(delta = 4, mu = 8), lty = 2, col = grays[22], lwd = segmentlinewidth)
points(4, PCFn(delta = 4, mu = 8), pch = 16, col = "black", cex = labelsize)
text(4, PCFn(delta = 4, mu = 8) + 0.03, expression(paste(c)),cex = labelsize)


#Label the feasible frontier
text(2, 0.75, expression("Better for"), cex = labelsize)
text(2, 0.7, expression("employer"), cex = labelsize)
Arrows(1.8, 0.78, 1, 0.89, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


text(8, 0.25, expression("Better for"), cex = labelsize)
text(8, 0.2, expression("employee"), cex = labelsize)
Arrows(8.8, 0.225, 10, 0.225, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


text(3.5, 0.97, expression(paste("Slope of isoprofit")), cex = labelsize)
text(3.5, 0.89, expression(paste(-mrs(p,e) == frac(Delta*e,Delta*p),phantom()==frac(e, p))), cex = labelsize)
Arrows(5.3, 0.89, 6.8, 0.89, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(8.5, 0.5, expression(paste(mrs(p,e) == frac(u[p],u[e]),phantom()==-frac(e, p),phantom() == mrt(p,e))), cex = labelsize, xpd = TRUE)
Arrows(5.9, 0.5, 4.3, 0.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


dev.off()