require(shape)
pdf(file = "indmarketdemand/demand_cd_indiff_veblen_initial.pdf", width = 9, height = 7)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5
fadelevel <- 0.13

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")
grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")



#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 6, 1, 1))

uA <- function(x, y, alpha = 0.5) {
  x^(alpha)*y^(1 - alpha)
}

uA <- function(x, y, alpha = 0.5) {
  x^(alpha)*y^(1 - alpha)
}


bcA <- function(x, w = 1, h = 1) {
  h - w*x
}


xlims <- c(0, 5)
ylims <- c(0, 1.1)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(0.7,1,1.2)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

polygon(c(0,4,0,0), c(0,0,1,0), 
        col=rgb(0, 158/255, 115/255, fadelevel), 
        density=NULL, border = NA)

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
# ticksx <- seq(from = 0, to = xlims[2], by = 2)
# xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(0, 0.5, 1, ylims[2])
ylabels <- c(NA, expression(paste(1 - h[a])), expression(paste(f == 1)), NA)
ticksx <- c(0, 2,  4, xlims[2])
xlabels <- c(NA, expression(paste(x[a] == w*h[a])),  expression(paste(x == w)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, bcA(xx1, w = 0.25, h = 1), col = CBCols[1], lwd = graphlinewidth + 0.2)
#lines(xx1, indiffA1(xx1, uA = 20, rmax = 2.5, xmax = 10), col = COLB[4], lwd = graphlinewidth)


text(0.5*xlims[2], -0.15, expression(paste("Consumption, ", x == w*h)), xpd = TRUE, cex = axislabelsize) 
#mtext(expression(paste("Consumption, ", x == w*h)), side=1, line = 2.5, cex = axislabelsize)
text(-0.66, 0.5*ylims[2], expression(paste("Leisure time as a proportion, ", f == 1 - h)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Label the iso-welfare functions for the HG, Aisha
# text(4.6, 0.04, expression(u[1]^v))
# text(4.6, 0.13, expression(u[2]^v))
text(4.6, 0.25, expression(paste(u[2] == u[a])), cex = annotatesize)
text(4.6, 0.35, expression(paste(u[3])), cex = annotatesize)
text(4.6, 0.13, expression(paste(u[1])), cex = annotatesize)

contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = CBCols[2],
        lwd = graphlinewidth, 
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE)

segments(0, 0.5, 2, 0.5, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(2, 0, 2, 0.5, lty = 2, col = grays[20] , lwd = segmentlinewidth)
points(2, 0.5, pch = 16, col = "black", cex = 1.5)
text(1.9, 0.48, expression(a), cex = annotatesize)

#text(2, 0.9, expression(paste(mrs == mrt)), cex = annotatesize)
text(2.38, 0.51, expression(paste(mrs == mrt)), cex = annotatesize)
#Arrows(2, 0.88, 2, 0.53, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(0.7, 0.2, expression(paste("Feasible")), cex = annotatesize)
text(0.7, 0.15, expression(paste("bundles of")), cex = annotatesize)
text(0.7, 0.1, expression(paste("leisure and")), cex = annotatesize)
text(0.7, 0.05, expression(paste("consumption")), cex = annotatesize)


text(4, 1, expression(paste("Infeasible")), cex = annotatesize)
text(4, 0.95, expression(paste("bundles")), cex = annotatesize)


text(3.2, 0.1, expression(paste("Feasible")), cex = annotatesize)
text(3.2, 0.05, expression(paste("frontier")), cex = annotatesize)
dev.off()
