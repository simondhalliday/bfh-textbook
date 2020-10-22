#Graph Designer: Simon Halliday
#Authors: Bowles and Halliday
#Title: Microeconomics: Competition, Conflict and Coordination

library(shape)
pdf(file = "indmarketdemand/cd_coffee_margin.pdf", width = 6, height = 4)

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
grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")


#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 6.5, 0.5, 0.5))


# cd_demand_coffeedata ----------------------------------------------------


ppf <- function(x, slope = 1, bary = 20) {
  bary - slope * x
}

demand <- function(x, alpha = 0.5, endowment = 12){
  (endowment*alpha)/x
}

uFn <- function(x, y, alpha = 0.8){
  ((x)^alpha)*(y^(1-alpha))
}

indiffA <- function(x, ubar = uFn(8,2) - 1, alpha = 0.3) {
  (ubar/ ((x)^alpha))^(1/ (1 - alpha))
}

ylims <- c(0, 4)
xlims <- c(0, 13)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(uFn(16, 2), uFn(16, 4), uFn(16, 8)) #alpha = 0.8

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs = "i", 
     yaxs = "i"
)


ticksx <- c(0, 2, 4, 6, 8, 12, 13)
xlabels <- c(0, 2, 4, 6, 8, 12, NA)
ticksy <- c(ylims[1], 1, 1.5, 3, ylims[2])
ylabels <- c(NA, expression(paste(p[x] == 10)), expression(paste(p[x] == 15)), expression(paste(p[x] == 30)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)

#Draw the graphs
lines(xx1, demand(xx1), col = CBCols[1], lwd = graphlinewidth)

#Axis labels
text(0.5*xlims[2], -0.8, expression(paste("Kilograms of coffee, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-3.2, 0.5*ylims[2], expression(paste("Price of coffee, ", p[x])), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Add mrs = mrt at i
text(11, 16.75, expression(paste("for ", alpha == 0.5, " and ", m == 100)), cex = labelsize)
text(11, 15.25, expression(paste("then ", x == 5)), cex = labelsize)
text(11, 16, expression(paste("when ", p[x] == 10)), cex = labelsize)
Arrows(10, 15, 5.5, 10.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the indifference curves
text(8.1, 38, expression(u[1]^A), cex = labelsize)
text(9.5, 38, expression(u[2]^A), cex = labelsize)
text(11.3, 38, expression(u[3]^A), cex = labelsize)

#Label the feasible frontiers
text(10.5, 1.1, expression(paste("Demand curve")), cex = labelsize)
text(10.5, 0.8, expression(paste("for coffee")), cex = labelsize)
text(14, 6, expression(paste(x == frac(alpha*m, p[x]), " or")), cex = labelsize)
text(14, 4.5, expression(paste(p[x] == frac(alpha*m, x))), cex = labelsize)

#Vector of quantity demanded from the adjacent figure
demandx <- c(2, 4, 6)
#Segments for the prices and quantities
segments(0, demand(demandx[1]), demandx[1], demand(demandx[1]), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(demandx[1], 0, demandx[1], demand(demandx[1]), lty = 2, col = grays[20], lwd = segmentlinewidth, xpd = TRUE)
segments(0, demand(demandx[2]), demandx[2], demand(demandx[2]), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(demandx[2], 0, demandx[2], demand(demandx[2]), lty = 2, col = grays[20], lwd = segmentlinewidth, xpd = TRUE)
segments(0, demand(demandx[3]), demandx[3], demand(demandx[3]), lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(demandx[3], 0, demandx[3], demand(demandx[3]), lty = 2, col = grays[20], lwd = segmentlinewidth, xpd = TRUE)

points(demandx[1], demand(demandx[1]), pch = 16, col = "black", cex = 1.5)
points(demandx[2], demand(demandx[2]), pch = 16, col = "black", cex = 1.5)
points(demandx[3], demand(demandx[3]), pch = 16, col = "black", cex = 1.5)

text(demandx[1] + 0.3, demand(demandx[1]) + 0.15, 
     expression(paste(a)), cex = annotatesize)

text(demandx[2] + 0.3, demand(demandx[2]) + 0.15, 
     expression(paste(b)), cex = annotatesize)

text(demandx[3] + 0.3, demand(demandx[3]) + 0.15, 
     expression(paste(c)), cex = annotatesize)

dev.off()

