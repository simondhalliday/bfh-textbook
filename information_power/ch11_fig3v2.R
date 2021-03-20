require(ggplot2)
require(shape)
pdf(file = "information_power/ch11_fig3v2.pdf", width = 9, height = 7)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

#Parameters for figures
u1 <- 0
u2 <- 5
u3 <- 15
delta1 <- 5
delta2 <- 5
delta3 <- 5

indiffFn1 <- function(p, u1 = 0, delta1 = 5) {
  1 - (delta1) / (p - u1)
}

indiffFn2 <- function(p, u2 = 5, delta2 = 5) {
  1 - (delta2) / (p - u2)
}

indiffFn3 <- function(p, u3 = 15, delta3 = 5) {
  1 - (delta3) / (p - u3)
}

#The derivative for the function q = 1 - delta/(p - u) = delta/p - u)^2;
#From that we get the derivative and substitute in the relevant values
#for delta, u and p; that is we get a slope of 1/32
#But we also need the intercept; 0.375 above, or q/2 as it stood
tangentLine <- function(m, x, b){
  m*x + b
}

COL <- c("#bae4b3", "#74c476", "#238b45")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0)

par(mar =  c(3, 3, .5, .5))
xlims <- c(0, 40)
ylims <- c(0, 1.1)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = "",
     ylab = "",
     xaxt = "n", yaxt = "n", cex.lab = axislabelsize, bty = "n")

mtext(expression(paste("Price, ", p)), side=1, line = 1.5, cex = axislabelsize)
text(-3, ylims[2] - 0.5*(ylims[2] - ylims[1]), expression(paste("Quality, ", q)), xpd = TRUE, cex = axislabelsize, srt = 90) 


npts <- 500 
#xx1 <- seq(0, xlims[2], length.out = npts)
xx1 <- seq(u1 + delta1, xlims[2], length.out = npts)
xx2 <- seq(u2 + delta2, xlims[2], length.out = npts)
xx3 <- seq(u3 + delta3, xlims[2], length.out = npts)
xx4 <- seq(14.2, 23, 0.01)
xx5 <- seq(25, 33, 0.01)



#Draw the lines for the graphs
lines(xx1, indiffFn1(xx1), col = COLA[5], lwd = graphlinewidth)
lines(xx2, indiffFn2(xx2), col = COLA[5], lwd = graphlinewidth)
lines(xx3, indiffFn3(xx3), col = COLA[5], lwd = graphlinewidth)
segments(9.5, 0.53, 19.5, 0.78, lty = 2, col = grays[22], lwd = graphlinewidth)
lines(xx4, tangentLine(x = xx4, m = 0.025, b = 0.17), col = grays[22], lty = 2, lwd = graphlinewidth)
lines(xx5, tangentLine(x = xx5, m = 0.0247, b = -0.07), col = grays[22], lty = 2, lwd = graphlinewidth)
# segments(9.5, 0.53, 19.5, 0.78, lty = 2, col = grays[22], lwd = graphlinewidth)

#Customize ticks and labels for the plot
ticksy <- c(0, 1, 1.1)
ylabels <- c(0, 1, NA)
ticksx <- c(0, 5, 10, 20, 40)
xlabels <- c(0, 5, 10, 20, 40)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#Annotation of the three indifference curves
text(6.2, 0.05, expression(paste(u[0])), cex = labelsize)
text(11.2, 0.05, expression(paste(u[1])), cex = labelsize)
text(21.2, 0.05, expression(paste(u[2])), cex = labelsize)

#Line for the max quality, q = 1 
segments(0, 1, 40, 1, lty = 2, col = grays[20], lwd = segmentlinewidth)

#Annotating line of 100% quality
text(20, 1.025, expression(paste("Maximum level of quality, q")), cex = labelsize)

#Add a point for the tangency
points(14.3, 0.65, pch = 16, col = "black", cex = 1.5)
points(19.2, 0.65, pch = 16, col = "black", cex = 1.5)
points(29.2, 0.65, pch = 16, col = "black", cex = 1.5)

#Add labels to points for the tangency
text(14.6, 0.615, expression(paste("a")), cex = annotatesize)
text(19.6, 0.615, expression(paste("b")), cex = annotatesize)
text(29.6, 0.615, expression(paste("c")), cex = annotatesize)

#Arrow to Slope of BRF
Arrows(11.7, .73, 15.7, .73, col = "black", lty = 1, lwd = 1.5, arr.type = "triangle")
text(9, 0.8, expression(paste("Slope", phantom() == phantom(), "-mrs",phantom() == phantom())), cex = labelsize)
text(9, 0.7, expression(paste(frac((1-q)^2, underline(u)))), cex = labelsize)
#text(5, 0.8, expression(paste(" = ", frac((1-q)^2,  underline("u")))), cex = labelsize)
dev.off()

