require(ggplot2)
require(shape)
pdf(file = "information_power/ch11_fig5_checkpoint.pdf", width = 9, height = 7)

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
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")

brfFn <- function(p, delta = 5) {
  1 - (delta) / p
}

solowCondition <- function(p, delta = 5){
  (p*(1/(4*delta)))
}


par(mar =  c(4, 4, 0.5, 0.5))
xlims <- c(0, 40)
ylims <- c(0, 1)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = "",
     ylab = "",
     xaxt = "n", yaxt = "n", cex.lab = axislabelsize, bty = "n")

npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(5, xlims[2], length.out = npts)
xx2 <- seq(1, xlims[2], length.out = npts)

#Draw the lines for the graphs
lines(xx1, brfFn(xx1), col = CBCols[4], lwd = graphlinewidth)
lines(xx2, solowCondition(xx2, delta = 5), col = CBCols[2], lwd = graphlinewidth)

#Customize ticks and labels for the plot
ticksy <- c(0, 0.5, 1)
ylabels <- c(0, expression(paste(frac(1,2))), 1)
ticksx <- c(0, 5,10, 20, 40)
xlabels <- c(0, expression(paste(underline("u"))),expression(paste(2*underline("u"))), expression(paste(4*underline("u"))), 40)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

mtext(expression(paste("Price, ", p)), side = 1, line = 2.5, cex = axislabelsize)
text(-4.5, 0.5*ylims[2], expression(paste("Quality, ", q)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Annotation of the three graphs and the NE
t#ext(32, 0.97, expression(paste("Incomplete contract")), cex = annotatesize)
#text(36, 1, expression(paste("Isocost")), cex = annotatesize)
text(18, 0.98, expression(paste(c[1])), cex = annotatesize)
#text(32, 0.92, expression(paste(q == frac(p, 8*underline("u")))), cex = annotatesize)
text(35, 0.75, expression(paste("Best-response")), cex = annotatesize)
text(35, 0.68, expression(paste("function (ICC)")), cex = annotatesize)
#text(35, 0.50, expression(paste(q == 1 - frac(2*underline("u"), p))), cex = annotatesize)
#text(16, 0.57, expression(paste("Nash")), cex = annotatesize)
#text(16, 0.52, expression(paste("equilibrium")), cex = annotatesize)

#Lines for the coordinates of the Nash equilbrium
segments(10, 0, 10, 0.5, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, 0.5, 10, 0.5, lty = 2, col = grays[20], lwd = segmentlinewidth)

#Iso-cost slope annotation
text(9, 0.81, expression(paste("Slope", phantom() == frac(q,p), phantom() == frac(1, 4*underline("u")))), cex = annotatesize)
Arrows(13.5, 0.84, 15.5, 0.84, col = "black", lty = 1, lwd = 2, arr.type = "triangle")

#BRF Slope annotation
#text(25, 0.41, expression(paste("Slope", phantom() == q[p], phantom() == frac(2*underline("u"), p^2))), cex = annotatesize)
#Arrows(25, 0.43, 25, 0.56, col = "black", lty = 1, lwd = 2, arr.type = "triangle")


#Add a ray and a point for c.  
points(10, 0.5, pch = 16, col = "black", cex = 1.5)
text(9.6, 0.53, expression(paste("n")), cex = annotatesize)

dev.off()

