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

indiffFn2 <- function(y, u2 = 10.5, a2 = 5) {
  1 - (a2) / (y - u2)
}

indiffFn3 <- function(y, u3 = 16.5, a3 = 5) {
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


#xpoly1 <- seq(from = xlims[1] + 0.2, to = 40, length.out = 500)
#ypoly1 <- PCFn(xpoly1, mu = 20)
#polygon(x = c(xpoly1, rev(xpoly1[1])), y = c(ypoly1, rev(ypoly1)[1]), col=COLB[1], density=NULL, border = NA)


npts <- 500 
xx1 <- seq(u1 + a1 + 0.1, xlims[2], length.out = npts)
xx2 <- seq(u2 + a2xlims[2] + 0.1, xlims[2], length.out = npts)
xx3 <- seq(10, xlims[2], length.out = npts)
xx4 <- seq(7, 17, 0.01)
xx6 <- seq(5, xlims[2], length.out = npts)

#Draw the lines for the graphs
xx5 <- seq(5, xlims[2], length.out = npts)
xx7 <- seq(xlims[1], 20, length.out = npts)
lines(xx5, indiffFn1(xx5, u1 = 0), col = COLA[4], lwd = graphlinewidth)
lines(xx7, solowCondition(xx7, delta = 5), col = COLB[4], lwd = graphlinewidth)

#lines(xx5, PCFn(xx5, mu = 8), col = COLB[4], lwd = graphlinewidth)
lines(xx6, indiffFn1(xx6, u1 = 5), col = COLA[4], lwd = graphlinewidth)
lines(xx3, indiffFn1(xx3, u1 =10), col = COLA[4], lwd = graphlinewidth)
#lines(xx6, prodFn2(xx6), col = COLB[4], lwd = graphlinewidth)

#Customize ticks and labels for the plot
ticksy <- c(0, PCFn(delta = 4, mu = 8), 1, 1.1)
ylabels <- c(0, expression(paste(e^C)), expression(paste(bar(e) == 1) ), NA)
ticksx <- c(0, 10, xlims[2])
xlabels <- c(0, expression(paste(p^C) == 10), NA)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1,cex.axis = labelsize)

#Annotation of the three indifference curves
text(11.3, 0.05, expression(paste(u[1])), cex = labelsize-0.05)
text(7.2, 0.05, expression(paste(u[0] == 0)), cex = labelsize-0.05)
#text(11, 0.06, expression(paste(phantom() == 0)), cex = labelsize-0.05)
text(16.3, 0.05, expression(paste(u[2])), cex = labelsize-0.05)
text(18, 0.98, expression(paste(c[1])), cex = annotatesize)

#Line for the max quality, q = 1 
segments(0, 1, xlims[2], 1, lty = 2, col = grays[20], lwd = 2)
#Annotating line of 100% quality
#text(1.4, 1.025, expression(paste("Maximum level of effort, ", bar(e))))

#Add a point for the tangency
segments(10, 0, 10, PCFn(delta = 4, mu = 8), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, PCFn(delta = 4, mu = 8), 10, PCFn(delta = 4, mu = 8), lty = 2, col = grays[22], lwd = segmentlinewidth)
points(10, PCFn(delta = 4, mu = 8), pch = 16, col = "black", cex = labelsize)
text(9, PCFn(delta = 4, mu = 8) + 0.03, expression(paste(c)),cex = labelsize)


#Label the feasible frontier
text(4, 0.7, expression("Better for"), cex = labelsize)
text(4, 0.65, expression("employer"), cex = labelsize)
Arrows(4, 0.74, 2, 0.85, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


text(37, 0.35, expression("Better for"), cex = labelsize, xpd = TRUE)
text(37, 0.3, expression("employee"), cex = labelsize, xpd = TRUE)
Arrows(41, 0.33, 45, 0.33, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5, xpd = TRUE)


text(8.5, 0.97, expression(paste("Slope of isocost")), cex = labelsize)
text(8.5, 0.89, expression(paste(-mrt ==frac(e, p))), cex = labelsize)
Arrows(12, 0.9, 16, 0.9, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(30, 0.5, expression(paste(mrs == frac(u[p],u[e]),phantom()==-frac(e, p),phantom() == mrt)), cex = labelsize, xpd = TRUE)
Arrows(23, 0.5, 11, 0.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

Arrows(21, 0.25, 18, 0.25,  col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(24, 0.3, expression(paste("Slope of iso-u")), cex = labelsize, xpd =TRUE)
text(24, 0.25, expression(paste(phantom() == "-mrs ")), cex = labelsize)
text(22.8, 0.15, expression(paste(phantom() == frac(1, u[e]))), cex = labelsize)
text(27.5, 0.16, expression(paste(phantom() ==frac((1-e)^2, underline(u)))), cex = labelsize)
#text(24, 0.05, expression(paste(phantom() == -frac((1-e)^2, underline(u)))), cex = labelsize)




dev.off()