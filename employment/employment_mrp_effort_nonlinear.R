require(shape, rootSolve)

#define functions

MRP<-function(n){
  20000/n
}
MRP2<-function(n){
  40000/n
}

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")
grays <- gray.colors(25, start = 1, end = 0)


ylims <- c(0, 80)
xlims <- c(0, 2200)
npts <- 501
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

# ----
# first plot: competitive vs monopsony
pdf(file = "employment/employment_mrp_effort_nonlinear.pdf", width = 10, height = 8)
par(mar =  c(5, 7, 3, 3))

#Notice the plot starts at x = 0.2 not 0
plot(xlims[1], 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i"
)

# ticksx <- seq(from = 0, to = xlims[2], by = 0.2)
# xlabels <- seq(from = 0, to = xlims[2], by = 0.2)
# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)

ticksx <- c(xlims[1], 500, 1000, xlims[2])
xlabels <- c(NA, expression(paste(italic(l[1]^{N}) == 500)), expression(paste(italic(l[2]^{N}) == 1000)),NA)
ticksy <- c(ylims[1], 40, ylims[2])
ylabels <- c(NA, expression(paste(c[italic(l)]^N== 40)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = axislabelsize*0.8)
axis(2, at = ticksy, pos = xlims[1], labels = ylabels, las = 1, cex.axis = axislabelsize*0.8)

npts <- 500 
xx2 <- seq(xlims[1], xlims[2], length.out = npts)

#Draw the graphs
lines(xx2, MRP(xx2), col = COLA[4], lwd = graphlinewidth)
lines(xx2, MRP2(xx2), lty = 2, col = COLA[4], lwd = graphlinewidth)


#Axis labels
#mtext(expression(paste("Labor used by the employer, ", italic(l) == e^N*h)), side = 1, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], -8, expression(paste("Labor used by the employer, ", italic(l) == e^N*h)), xpd = TRUE, cex = axislabelsize) 
text(-300, 0.5*ylims[2], expression(paste("Marginal cost & marginal revenue product of labor, ", list(c[italic(l)], r[italic(l)] ))), xpd = TRUE, cex = axislabelsize, srt = 90) 


#add segments
segments(0, 40, 1880, 40, lty = 1, col = COLB[3], lwd = graphlinewidth)
segments(500, 0, 500, 40, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(1000, 0, 1000, 40, lty = 2, col = "gray", lwd = segmentlinewidth)

## add points

points(500, 40, pch = 16, col = "black", cex = 1.5)
points(1000, 40, pch = 16, col = "black", cex = 1.5)

# label points
text(500 + 10, 43, expression(paste(n[1])), cex = labelsize)
text(1000 , 43, expression(paste(n[2])), cex = labelsize)



# add labels to graphs
text(2100, 11, expression(paste(mrp,italic(l)[1])), cex = labelsize)
text(2100, 21, expression(paste(mrp,italic(l)[2])), cex = labelsize)
text(2000, 49, expression(paste("Marginal cost")), cex = labelsize)
text(2000, 46, expression(paste("of labor")), cex = labelsize)
text(2000, 40, expression(paste(c[italic(l)]^N == frac(w^N,e^N))), cex = labelsize)

text(600, 85, expression(paste("Marginal revenue product of labor")), cex = labelsize, xpd = TRUE)
text(600, 81, expression(paste(mrp,italic(l) == r[italic(l)])), cex = labelsize, xpd = TRUE)


#text(0.58, ACL(0.85) - 1.1, "Minimum wage", cex = axislabelsize)
#text(0.58, ACL(0.85) - 1.6, "not binding", cex = axislabelsize)

# text(0.85, 3.7, "Competitive", cex = axislabelsize)
# text(0.85, 3.2, "Marginal cost", cex = axislabelsize)
# text(0.85, 2.7, "of labor", cex = axislabelsize)
# text(0.85, 2.2, expression(paste( (mcl[C]) )), cex = axislabelsize)

dev.off()
