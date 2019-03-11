require(ggplot2)
require(shape)
pdf(file = "information_power/information_total_benefits_costs.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")


#Parameters for figures
u1 <- 4
u2 <- 10
u3 <- 16
delta1 <- 2
delta2 <- 2
delta3 <- 2

uA <- function(p, q, delta = 5) {
  p - (delta) / (1 - q)
}

costs <- function(q, delta = 5) {
  (delta) / (1 - q)
}

benefits <- function(q, p = 20, delta = 5) {
  q*(p - (delta) / (1 - q))*(1/(1-q))
}

benefits2 <- function(q, p = 20, delta = 5) {
  (p - (delta) / (1 - q))*(1/(1-q))
}





COL <- c("#bae4b3", "#74c476", "#238b45")
par(mar =  c(5, 5.5, 4, 2))
xlims <- c(0, 1.05)
ylims <- c(0, 46)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = "",
     ylab = "",
     xaxt = "n", yaxt = "n", cex.lab = axislabelsize, bty = "n")

npts <- 500 
xx1 <- seq(0, xlims[2], length.out = npts)
xx2 <- seq(0, 0.99, length.out = npts)
# xx2 <- seq(u2 + delta2, xlims[2], length.out = npts)
# xx3 <- seq(u3 + delta3, xlims[2], length.out = npts)
# xx4 <- seq(7, 17, 0.01)

#Draw the lines for the graphs
lines(xx2, costs(xx2), col = COLA[4], lwd = graphlinewidth)
lines(xx2, benefits(xx2, p = 20), col = COLB[4], lwd = graphlinewidth)
#lines(xx2, benefits(xx2, p = 30), col = COLB[4], lwd = graphlinewidth)
#lines(xx2, benefits(xx2, p = 38), col = COLB[4], lwd = graphlinewidth)
# lines(xx2, benefits2(xx2, p = 20), col = COLB[4], lwd = graphlinewidth)
# lines(xx2, benefits2(xx2, p = 15), col = COLB[4], lwd = graphlinewidth)
# lines(xx2, benefits2(xx2, p = 30), col = COLB[4], lwd = graphlinewidth)
#lines(xx2, mb(xx2, p = 30), col = COLA[4], lwd = graphlinewidth)
#lines(xx1, mc(xx1), col = COLB[4], lwd = graphlinewidth)
#lines(xx3, indiffFn3(xx3), col = COLA[4], lwd = graphlinewidth)
#lines(xx4, tangentLine(xx4), col = "darkgrey", lty = 2, lwd = graphlinewidth)

#Customize ticks and labels for the plot
ticksy <- c(0, 5, 10, ylims[2])
ylabels <- c(0, expression(paste(delta == 5)), expression(paste(10)),  NA)
ticksx <- c(0, 1/3, 0.5, 2/3, 1, xlims[2])
xlabels <- c(0, expression(paste(1/3)), expression(paste(1/2)), expression(paste(2/3)), 1, NA)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

mtext(expression(paste("Quality, ", q)), side=1, line = 2.5, cex = axislabelsize)
text(- 0.1, ylims[2] - 0.5*(ylims[2] - ylims[1]), expression(paste("Total benefit and costs,", list(TB, TC))), xpd = TRUE, cex = axislabelsize, srt = 90) 
#text(- 0.1, ylims[2] - 0.5*(ylims[2] - ylims[1]), expression(paste()), xpd = TRUE, cex = axislabelsize, srt = 90) 



#Annotation of the three indifference curves
# text(0.15, 5, expression(paste("Marginal Cost")))
# text(0.15, 2.5, expression(paste(mc == u[q], phantom() == frac(delta,(1-q)^2))))


text(0.2, 41, expression(paste("Total Benefits")))
text(0.2, 38, expression(paste(TB == (1-t)*v, phantom() == q*bgroup( "(",p - frac(delta,(1-q)),")" )*bgroup( "(", frac(1,1-q), ")") )))
# 
# text(0.58, 12, expression(paste(mb[1])))
# text(0.58, 10, expression(paste("at ", p == 15)))
# 
# text(0.76, 12, expression(paste(mb[2])))
# text(0.76, 10, expression(paste("at ", p == 20)))
# 
# text(0.88, 12, expression(paste(mb[3])))
# text(0.88, 10, expression(paste("at ", p == 30)))


#Line for the max quality, q = 1 
segments(1, 0, 1, ylims[2], lty = 2, col = "darkgray", lwd = segmentlinewidth)
#Annotating line of 100% quality
text(20, 1.025, expression(paste("Maximum level of quality, q")))


segments(0.5, 0, 0.5, 10, lty = 2, col = "darkgray", lwd = segmentlinewidth)
segments(0, 10, 0.5, 10, lty = 2, col = "darkgray", lwd = segmentlinewidth)
points(0.5, 10, pch = 16, col = "black", cex = 1.5)
text(0.5 + 0.015, 10 - 0.75, expression(paste(b)))


# segments(0.5, 0, 0.5, 20, lty = 2, col = "darkgray", lwd = segmentlinewidth)
# segments(0, 20, 0.5, 20, lty = 2, col = "darkgray", lwd = segmentlinewidth)
# points(0.5, 20, pch = 16, col = "black", cex = 1.5)
# text(0.5 + 0.015, 20 - 0.75, expression(paste(b)))
# 
# segments(2/3, 0, 2/3, 45, lty = 2, col = "darkgray", lwd = segmentlinewidth)
# segments(0, 45, 2/3, 45, lty = 2, col = "darkgray", lwd = segmentlinewidth)
# points(2/3, 45, pch = 16, col = "black", cex = 1.5)
# text(2/3 + 0.015, 45 - 0.75, expression(paste(c)))
# 
# segments(1/3, 0, 1/3, 11.25, lty = 2, col = "darkgray", lwd = segmentlinewidth)
# segments(0, 11.25, 1/3, 11.25, lty = 2, col = "darkgray", lwd = segmentlinewidth)
# points(1/3, 11.25, pch = 16, col = "black", cex = 1.5)
# text(1/3 + 0.015, 11.25 - 0.75, expression(paste(a)))

#Add a point for the tangency
points(12, 0.75, pch = 16, col = "black", cex = 1.5)

#Arrow to Slope of BRF
Arrows(10, 0.8, 12, 0.8, col = "black", lty = 1, lwd = 1.5, arr.type = "triangle")
text(5, 0.8, expression(paste("Slope = MRS = ", -frac(1, u[q]))))
text(5.4, 0.73, expression(paste(" = ", frac(delta, (1-q)^2))))
dev.off()

