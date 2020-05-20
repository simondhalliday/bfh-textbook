require(ggplot2)
require(shape)
pdf(file = "information_power/information_icc_pc_leftpanel.pdf", width = 9, height = 7)

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

isov <- function(p, delta = 5) {
  (p - delta)/p
}

brfFn <- function(p, delta = 5) {
  1 - (2 * delta) /p
}

solowCondition <- function(p, delta = 5){
  (p*(1/(8*delta)))
}

par(mar =  c(4, 6, 0.5, 1))
xlims <- c(0, 40)
ylims <- c(0, 1)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")),
     xaxt = "n", yaxt = "n", cex.lab = axislabelsize, bty = "n")

mtext(expression(paste("Price, ", p)), side = 1, line = 2.5, cex = axislabelsize)
text(-7, 0.5*ylims[2], expression(paste("Quality, ", q)), xpd = TRUE, cex = axislabelsize, srt = 90) 



npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(10, xlims[2], length.out = npts)
xx2 <- seq(1, xlims[2], length.out = npts)
xx0 <- seq(5, xlims[2], length.out = npts)

#Customize ticks and labels for the plot
ticksy <- c(0, 0.5, 1)
ylabels <- c(0, expression(paste(q^C, phantom() == frac(1,2))), 1)
ticksx <- c(0, 5, 10, NA, 40)
xlabels <- c(0, expression(paste(underline("u"))), expression(paste(p^C == 2*underline("u"))), NA, 40)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#Annotation of the three graphs and the NE
#text(31.8, 0.98, expression(paste("Incomplete contract")), cex = annotatesize)
#text(31, 0.85, expression(paste(q == frac(p, 8*underline("u")))), cex = annotatesize)
#text(26, 0.34, expression(paste("Best-response")), cex = annotatesize)
#text(26, 0.3, expression(paste("function")), cex = annotatesize)
#text(26, 0.25, expression(paste("(ICC)")), cex = annotatesize)
#text(25, 0.21, expression(paste(q == 1 - frac(2*underline("u"), p))), cex = annotatesize)
#Arrows(26, 0.36, 26, 0.59, col = "black", lty = 1, lwd = 2, arr.type = "triangle")


# text(16, 0.57, expression(paste("Nash")), cex = annotatesize)
# text(16, 0.52, expression(paste("equilibrium")), cex = annotatesize)
#text(20.6, 0.48, expression(paste("n")), cex = annotatesize)


#text(13, 0.98, expression(paste("Complete contract")), cex = annotatesize)
text(18, 0.98, expression(paste(c[1])), cex = annotatesize)
text(37, 0.98, expression(paste(c[2])), cex = annotatesize)

#text(12, 0.85, expression(paste(q == frac(p, 4*underline("u")))), cex = annotatesize)
text(34.5, 0.55, expression(paste("Participation constraint")), cex = annotatesize)
text(34.5, 0.51, expression(paste("(willingness to sell)")), cex = annotatesize)
#text(35, 0.42, expression(paste(q == 1 - frac(underline("u"), p))), cex = annotatesize)


#Lines for the coordinates of the Nash equilbrium
#segments(20, 0, 20, 0.5, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, 0.5, 10, 0.5, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(10, 0, 10, 0.5, lty = 2, col = grays[20], lwd = segmentlinewidth)

#Draw the lines for the graphs
lines(xx0, isov(xx0, delta = 5), col = COLA[4], lwd = graphlinewidth)
#lines(xx1, brfFn(xx1), col = COL[2], lwd = graphlinewidth)
lines(xx2, solowCondition(xx2, delta = 5), col = COLB[3], lwd = graphlinewidth)

#Add a point for the NE
#points(20, 0.5, pch = 16, col = "black", cex = 1.5)

#Add a ray and a point for c  
segments(1, 0.05, 20, 1, lty = 1, lwd = graphlinewidth, col = COLB[4])
points(10, 0.5, pch = 16, col = "black", cex = 1.5)
text(9.6, 0.53, expression(paste("c")), cex = annotatesize)

Arrows(36, 0.58, 36, 0.83, col = "black", lty = 1, lwd = 2, arr.type = "triangle")

text(3.4, 0.03, expression(paste(u[0] == 0)), cex = annotatesize)

#text(18, 0.98, expression(paste(c[1])), cex = annotatesize)
#text(37, 0.98, expression(paste(c[2])), cex = annotatesize)
#text(39, 0.525, expression(paste(c[3])), cex = annotatesize)


dev.off()

