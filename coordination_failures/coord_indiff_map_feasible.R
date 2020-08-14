require(shape)
pdf(file = "coordination_failures/coord_indiff_map_feasible.pdf", width = 9, height = 7)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

Grays <- gray.colors(25, start = 1, end = 0)

par(mar =  c(5, 5, 1, 1))


uA <- function(ea, eb, alpha = 30, beta = 1/2) {
  (alpha - beta*(ea + eb))*ea - 0.5*(ea)^2
}

hANE <- function(alpha, beta = 1/2){
  alpha/(1 + 3*beta)
}

brfA <- function(ea, alpha = 30, beta = 1/2) {
  (alpha - ea*(1 + 2*beta))/(beta)
}


xlims <- c(0, 18.5)
ylims <- c(0, 18.5)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(uA(11, brfA(11)), 
       uA(hANE(alpha = 30, beta = 1/2), hANE(alpha = 30, beta = 1/2)), 
       uA(13, brfA(13))
)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs = "i", 
     yaxs = "i")

ticksy <- c(0,  brfA(13), 12,  brfA(11), ylims[2])
ylabels <- c(NA, 8, 12, 16, NA)


ticksx <- c(0, 6.1, 8, 13,  xlims[2])
xlabels <- c(NA, 6, 8, 13, NA)


axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize - 0.05)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize - 0.05)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Polygon for the feasible set
# xpoly1 <- c(xlims[1], xlims[2], xlims[2], xlims[1], xlims[1])
# ypoly1 <- c(brfA(13), brfA(13), ylims[2], ylims[2], brfA(13))
# polygon(x = xpoly1, y = ypoly1, col = COLA[1], density = NULL, border = NA)
#text(3, 17, expression(paste("Feasible Set")), xpd = TRUE, cex = annotatesize) 

contour(y, x, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLA[4],
        lwd = graphlinewidth,
        levels = a, 
        xaxs = "i", 
        yaxs = "i", 
        add = TRUE) 

text(0.5*xlims[2], -2.1, expression(paste("A's hours, ", h^A)), xpd = TRUE, cex = axislabelsize)
text(-1.75, 9.25, expression(paste("B's hours,", h^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the iso-welfare functions for the A
text(15, 6.3, expression(paste(u[j]^A == 169)), cex = annotatesize)
text(15, 9.6, expression(paste(u[n]^A == 144)), cex = annotatesize)
text(15, 12.5, expression(paste(u[k]^A == 121)), cex = annotatesize)


segments(6.1, 0, 6.1, brfA(13), lty = 2, col = Grays[18], lwd = segmentlinewidth)

segments(8, 0, 8, brfA(13), lty = 2, col = Grays[18], lwd = segmentlinewidth)

segments(13, 0, 13, brfA(13), lty = 2, col = Grays[18], lwd = segmentlinewidth)


#Label Nash Equilibrium 
points(12, 12, pch = 16, col = "black", cex = 1.5)

text(12 - 0.3, 12 - 0.6, expression(paste("n")), cex = annotatesize)



segments(0,  brfA(13),  xlims[2],  brfA(13), lty = 1, col = COLB[4], lwd = graphlinewidth)

points(13, brfA(ea = 13), pch = 16, col = "black", cex = 1.5)
text(13 - 0.3, brfA(ea = 13) - 0.6, expression(paste("j")), cex = annotatesize)

points(11, brfA(11), pch = 16, col = "black", cex = 1.5)
text(11 - 0.3, brfA(11) - 0.6, expression(paste("k")), cex = annotatesize)


dev.off()
