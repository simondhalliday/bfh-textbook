#' Graph Designer: Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

# TODO Add points and labels 
# TODO Add tangets at points

library("shape")

pdf(file = "risk/risk_averse_dara_c.pdf", width = 9, height = 7)

# Set parameters for graphics
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
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)

a <- c(2, 4, 6)
par(mar =  c(5, 8, 1, 1))
xlims <- c(0, 10)
ylims <- c(0, 40)


u <- function(y, delta, a = 3.5, b = 0.1, c = 2){
  y^a - b * delta^c
}

indiff <- function(u, delta, a = 1.5, b = 0.5, c = 4){
  (u + b * delta^c )^(1/a)
}

slopeA <- function(g, slope1 = 0.25, slope2 = 0.08){
  slope1 + 2*slope2*g
}

indiffA2 <- function(g, intercept = 7.8, slope1 = 0.25, slope2 = 0.055){
  intercept  + slope1*g + slope2*g^2
}

slopeA2 <- function(g, slope1 = 0.25, slope2 = 0.055){
  slope1 + 2*slope2*g
}


slopeA3 <- function(g, slope1 = 0.25, slope2 = 0.01){
  slope1 + 2*slope2*g
}

tangentline <- function(b, m, x){
  m*x + b
}

#Plot command 
plot(0, 0, xlim = xlims, ylim = ylims, 
     type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     axes = FALSE,
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs = "i", 
     yaxs = "i"
)

y1 <- indiff(5, 0.5*xlims[2], c = 2.7)
y2 <- indiff(64, 0.5*xlims[2], c = 2.7) - 4
y3 <- indiff(156, 0.5*xlims[2], c = 2.7)

ticksx <- c(0, 0.325*xlims[2], 0.5*xlims[2], 0.625*xlims[2], xlims[2])
xlabels <- c(NA, expression(paste(Delta[f])), expression(paste(Delta[e] == Delta[h])), expression(paste(Delta[g])), NA)
ticksy <- c(0, y1, y2, y3, ylims[2])
ylabels <- c(NA, expression(paste(hat(y)[e])), expression(paste(hat(y)[f] == hat(y)[g])), expression(paste(hat(y)[h])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#Axis labels and draw linear utility function
mtext(expression(paste("Difference in wealth (good versus bad outcome), ", Delta, ", risk")), side = 1, line = 2.5, cex = axislabelsize)
text(-1.8, 0.5*ylims[2], expression(paste("Expected wealth, ", hat(y))), xpd = TRUE, cex = axislabelsize, srt = 90) 

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0.5*xlims[2] - 1.5, 0.5*xlims[2] + 1.25, length.out = npts)

# tangent lines
lines(xx2, tangentline(x = xx2, m = 3.97626, b = -7.500294), col = grays[22], lwd = segmentlinewidth, lty = 2)
lines(xx2, tangentline(x = xx2, m = 2.39591, b = 21.5975), col = grays[22], lwd = segmentlinewidth, lty = 2)

# util curves
for (u in c(5, 64, 156) ) {
  lines(xx1, indiff(u, xx1, c = 2.7), col = COLB[4], lwd = graphlinewidth, lty = 1)  
}

# vertical delta segment
segments(0.5*xlims[2], 0, 0.5*xlims[2], ylims[2], col = grays[20], lwd = segmentlinewidth, lty = 2)

# horizontal segments
segments(0, y1, 0.5*xlims[2], y1, col = grays[20], lwd = segmentlinewidth, lty = 2)
segments(0, y2, indiff(5, 0.75*xlims[2], c = 2.7), y2, col = grays[20], lwd = segmentlinewidth, lty = 2)
segments(0, y3, 0.5*xlims[2], y3, col = grays[20], lwd = segmentlinewidth, lty = 2)

# points
points(0.5*xlims[2], y1, pch = 16, col = "black", cex = 1.5)
points(0.325*xlims[2], y2, pch = 16, col = "black", cex = 1.5)
points(0.625*xlims[2], y2, pch = 16, col = "black", cex = 1.5)
points(0.5*xlims[2], y3, pch = 16, col = "black", cex = 1.5)

# label points
text(0.5*xlims[2] + 0.25, y1 - 1, expression(paste(e)), cex = labelsize)
text(0.325*xlims[2], y2 + 1.5, expression(paste(f)), cex = labelsize)
text(0.625*xlims[2], y2 + 1.5, expression(paste(g)), cex = labelsize)
text(0.5*xlims[2] + 0.25, y3 - 1, expression(paste(h)), cex = labelsize)

# label indiff
text(0.95*xlims[2], 0.98*ylims[2], expression(paste(u[1])), cex = labelsize)
text(0.85*xlims[2], 0.98*ylims[2], expression(paste(u[2])), cex = labelsize)
text(0.625*xlims[2], 0.98*ylims[2], expression(paste(u[3])), cex = labelsize)

dev.off() 
