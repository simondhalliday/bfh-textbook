require(shape)
pdf(file = "coordination_failures/production_mb.pdf", width = 9, height = 12)

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


#Need to create a stacked graph and 
#use the option mfrow = c(2,1) for that
par(mar =  c(4, 6, 1, 1), mfrow = c(2,1))

indiffA <- function(ea, uA = 46.08) {
  uA + (1/2)*(ea)^2
}

output <- function(ea, eb = 12, alpha = 30, beta = 1/2){
  (alpha - beta*(ea+eb))*ea
}

uA <- function(ea, ya = output) {
  ya - 0.5*(ea)^2
}

mrsA <- function(ea){
  ea
}

slopeline <- function(ea, yint = 0.5, slope = 2){
  yint + slope*ea
}


xlims <- c(0, 31)
ylims <- c(0, 500)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(uA(ea = 12, output(ea = 12, eb = 12)), uA(ea = 15, output(ea = 15, eb = 0)), 300)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
ticksx <- c(seq(xlims[1], xlims[2], 3), xlims[2])
xlabels <- c(seq(xlims[1], xlims[2], 3), NA)
ticksy <- c(0, output(ea = 18), output(ea = 18, eb = 0), ylims[2])
ylabels <- c(NA, output(ea = 18), output(ea = 18, eb = 0), NA)
# ticksy <- c(0, 144, 225, 300, ylims[2])
# ylabels <- c(NA, expression(paste(y[1]^A)), expression(paste(y[2]^A)), expression(paste(y[3]^A)), NA)
# ticksx <- c(0, 6.9, 12, 9.6, 16, 24, 26)
# xlabels <- c(NA, expression(paste(e^A,"*")), expression(paste(1/2*beta)), expression(paste(e^{AN})), expression(paste(alpha)), expression(paste(1/beta)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis= labelsize - 0.05)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis= labelsize - 0.05)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(xlims[1], 24, length.out = npts)
xx3 <- seq(14, 22, length.out = npts)

lines(xx1, output(xx1, eb = 0), col = COLA[4], lwd = graphlinewidth)
lines(xx2, output(xx2, eb = 12), col = COLA[4], lwd = graphlinewidth)

#output(ea = 18), output(ea = 18, eb = 0),
segments(0, output(ea = 18), 18, output(ea = 18), lty = 2, col = "gray", lwd = segmentlinewidth, xpd = 350)
segments(0, output(ea = 18, eb = 0), 18, output(ea = 18, eb = 0), lty = 2, col = "gray", lwd = segmentlinewidth, xpd = 350)

segments(18, -500, 18, 500, lty = 2, col = "gray", lwd = segmentlinewidth, xpd = 350)
# segments(0, output(ea = 18), 18, output(ea = 18), lty = 2, col = "gray", lwd = segmentlinewidth, xpd = 350)
# segments(0, output(ea = 18, eb = 0), 18, output(ea = 18, eb = 0), lty = 2, col = "gray", lwd = segmentlinewidth, xpd = 350)

lines(xx3, slopeline(ea = xx3, slope = 6, yint = 162), lty = 2, col = COLB[4], lwd = graphlinewidth)
lines(xx3, slopeline(ea = xx3, slope = 12, yint = 162), lty = 2, col = COLB[4], lwd = graphlinewidth)

text(-4, 0.5*ylims[2], expression(paste("Consumption, ", y^A," (pounds, lb)")), xpd = TRUE, cex = axislabelsize, srt = 90)


points(18, output(ea = 18), pch = 16, col = "black", cex = 1.5)
text(18 - 0.75, output(ea = 18) + 14, expression(paste(j)), cex = annotatesize)

points(18, output(ea = 18, eb = 0), pch = 16, col = "black", cex = 1.5)
text(18 - 0.75, output(ea = 18, eb = 0) + 12, expression(paste(k)), cex = annotatesize)

text(21.8, 360, expression(paste(y(h^A, h^B == 0)) ), cex = annotatesize)
text(21.8, 250, expression(paste(y(h^A, h^B > 0))), cex = annotatesize)

MBenefit <- function(ea, eb = 12, alpha = 30, beta = 1/2) {
  (alpha - beta*eb - 2*beta*ea)
}

MCost <- function(ea, slope = 1, intercept = 0){
  slope*ea
}


xlims <- c(0, 31)
ylims <- c(0, 31)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
# ticksx <- seq(from = 0, to = xlims[2], by = 2)
# xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(0, MBenefit(18, eb = 12), MBenefit(18, eb = 0), MBenefit(0, eb = 12), MBenefit(0, eb = 0), ylims[2])
#ylabels <- c(NA, 6, 12, expression(paste(alpha - beta*h^B)), expression(paste(alpha)), NA)
ylabels <- c(NA, 6, 12, 24, 30, NA)
# ticksx <- c(0, 12, 15, 24, 30, xlims[2])
# xlabels <- c(0, 12, 15, 24, 30, xlims[2])
ticksx <- c(seq(xlims[1], xlims[2], 3), xlims[2])
xlabels <- c(seq(xlims[1], xlims[2], 3), NA)
#xlabels <- c(NA, expression(paste(h[1]^A)), expression(paste(h[2]^A)), expression(paste(frac(alpha - beta*h^b,2*beta) )), expression(paste(frac(alpha,2*beta) )), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels ,las = 1, cex.axis = labelsize - 0.05)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize - 0.05)
#text(x = c(0, 12, 15, 24, 30, xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE, cex = labelsize - 0.05)

#mtext(expression(paste("Aram's hours, ", h^A)), side=1, line = 4, cex = axislabelsize)
text(0.5*xlims[2], -3.8, expression(paste("A's hours, ", h^A)), xpd = TRUE, cex = axislabelsize)
text(-4, 0.5*ylims[2], expression(paste("A's marginal benefit (ounces, oz) ")), xpd = TRUE, cex = axislabelsize, srt = 90) 

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(4, 8, length.out = npts)

#Marginal benefit when the other does produce
lines(xx1, MBenefit(xx1, eb = 0), col = COLA[4], lwd = graphlinewidth)

#Marginal benefit when B produce NE number of hours
lines(xx1, MBenefit(xx1, eb = 12), col = COLA[4], lwd = graphlinewidth)

segments(0, MBenefit(ea = 18), 18, MBenefit(ea = 18), lty = 2, col = "gray", lwd = segmentlinewidth, xpd = 350)
segments(0, MBenefit(ea = 18, eb = 0), 18, MBenefit(ea = 18, eb = 0), lty = 2, col = "gray", lwd = segmentlinewidth, xpd = 350)

segments(18, 0, 18, 500, lty = 2, col = "gray", lwd = segmentlinewidth, xpd = 350)

text(10, 25.5, expression(paste("Marginal Benefit")), cex = labelsize)
text(10, 24, expression(paste("when, ", h^B== 0 )), cex = labelsize)


text(10, 9.5, expression(paste("Marginal Benefit ")), cex = labelsize)
text(10, 8, expression(paste("when, ", h^B > 0 )), cex = labelsize)


points(18, MBenefit(ea = 18), pch = 16, col = "black", cex = 1.5)
text(18 + 0.75, MBenefit(ea = 18) + 1, expression(paste(j)), cex = annotatesize)

points(18, MBenefit(ea = 18, eb = 0), pch = 16, col = "black", cex = 1.5)
text(18 + 0.75, MBenefit(ea = 18, eb = 0) + 1, expression(paste(k)), cex = annotatesize)

dev.off()
