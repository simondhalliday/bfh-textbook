#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "credit/non_contractual_equity_new.pdf", width = 8, height = 6)

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
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 6, 1, 1))

brfFn <- function(delta, mu = 1, k = 1) {
  .5 + (delta / (2 * mu)) * (1 - k)
}

PCFn <- function(delta, mu = 1) {
  delta/mu
}

isoreturnFn <- function(delta, pi=0.125) {
  1 - (pi)/delta
}

yFn <- function(d1, f1, mu = 1){
  mu*f1*(1 - f1) - d1*(1 - f1)
}

ylow <- function(delta, mu = 1, ybar = 0.0625){
  (-sqrt(delta^2 - 2*delta*mu + mu^2 - 4*mu*ybar) + delta + mu)/(2*mu)
}

yhigh <- function(delta, mu = 1, ybar = 0.0625){
  (sqrt(delta^2 - 2*delta*mu + mu^2 - 4*mu*ybar) + delta + mu)/(2*mu)
}

xlims <- c(0, 2)
ylims <- c(0, 1.05)

npts <- 501 
d1 <- seq(xlims[1], xlims[2], length.out = npts)
f1 <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(0.0625)

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


ticksy <- c(ylims[1], 0.5, 0.605, 0.75, ylims[2])
ylabels <- c(NA, expression(paste(frac(1,2))), expression(paste(f[a])), expression(paste(f[e])), NA)
ticksx <- c(xlims[1], 0.64, 1, xlims[2])
xlabels <- c(NA, expression(paste(delta[a])),expression(paste(delta[e])), NA)
#xlabels <- c(NA, expression(paste(delta[1](k[3]))),expression(paste(delta[2](k[2]))), expression(paste(delta[3](k[3]))), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 503 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Draw the graphs
lines(xx1, brfFn(xx1, k = 0), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfFn(xx1, k = 0.5), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfFn(xx1, k = 0.666), col = COLA[4], lwd = graphlinewidth)
lines(xx1, isoreturnFn(xx1, pi = 0.25), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, isoreturnFn(xx1, pi = 0.375), col = COLB[4], lwd = graphlinewidth)


#Axis labels
mtext(expression(paste("Interest factor, ", delta)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.25, 0.5*(ylims[2]), expression(paste("Probability of failure (risk), ", f)), xpd = TRUE, cex = axislabelsize, srt = 90) 

segments(0, 0.605, 0.64, 0.605, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, 0.75, 1, 0.75, lty = 2, col = grays[20] , lwd = segmentlinewidth)
# segments(1.5, 0, 1.5, brfFn(delta = 1.5, k = 0.666), lty = 2, col = grays[20] , lwd = segmentlinewidth)
# points(1.5, brfFn(1.5, k = 0.666), pch = 16, col = "black", cex = 1.5)
# text(1.5 + 0.04, brfFn(1.5, k = 0.666) - 0.03, expression(paste(b)), cex = labelsize)

segments(1, 0, 1, brfFn(delta = 1, k = 0.5), lty = 2, col = grays[20] , lwd = segmentlinewidth)
points(1, brfFn(1, k = 0.5), pch = 16, col = "black", cex = 1.5)
text(1, brfFn(1, k = 0.5) + 0.04, expression(paste(e)), cex = labelsize)

segments(0.64, 0, 0.64, brfFn(delta = 0.64, k = 0.666), lty = 2, col = grays[20] , lwd = segmentlinewidth)
points(0.64, brfFn(0.64, k = 0.666), pch = 16, col = "black", cex = 1.5)
text(0.64 + 0.04, brfFn(0.64, k = 0.666) - 0.03, expression(paste(a)), cex = labelsize)


text(0.53, 1, expression(paste("Excluded borrower's BRF")), cex = labelsize, xpd = TRUE)
text(0.55, 0.94, expression(paste(f(delta, k[1] < k[2]))), cex = labelsize)

text(1.51, 1, expression(paste("Marginal borrower's BRF")), cex = labelsize, xpd = TRUE)
text(1.45, 0.94, expression(paste(f(delta, k[2]))), cex = labelsize)


text(1.725, 0.51, expression(paste("BRF for borrower")), cex = labelsize, xpd = TRUE)
text(1.725, 0.45, expression(paste("with more than")), cex = labelsize, xpd = TRUE)
text(1.725, 0.39, expression(paste("minimum collateral")), cex = labelsize, xpd = TRUE)
text(1.725, 0.32, expression(paste(f(delta, k[3] > k[2]))), cex = labelsize, xpd = TRUE)
Arrows(1.75, 0.55, 1.75, 0.77, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#text(0.825, 0.425, expression(paste(hat(pi) > 0)), cex = labelsize)

text(0.225, 0.45, expression(paste("Competition")), cex = labelsize)
text(0.225, 0.39, expression(paste("condition")), cex = labelsize)
text(0.225, 0.34, expression(paste(b > 0)), cex = labelsize)


text(0.2, 0.05, expression(paste(hat(pi)[1])), cex = labelsize)
#text(0.34, 0.05, expression(paste(hat(pi)[1])), cex = labelsize)


dev.off()
