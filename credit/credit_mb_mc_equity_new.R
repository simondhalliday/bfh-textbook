library(shape)
library(plotrix)
pdf(file = "credit/credit_mb_mc_equity_new.pdf", width = 7, height = 7)

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
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442","#D55E00")

par(mar =  c(4, 6, 1, 1))
deltalvl <- c(0.2, 0.5, 0.8)

brfFn <- function(delta, q = 15, k = 0) {
  .5 + (delta / (2 * q)) * (1 - k)
}

mcFn <- function(f, q = 1.2) {
  q*f
}

mbFn <- function(f, q = 1.2, delta = 0.5) {
  q*(1 - f) + delta 
}

mbEquityFn <- function(f, q = 1.2, delta = 0.5, k = 0.5) {
  q*(1 - f) + delta*(1-k)
}

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
eqH <- uniroot(function(x)  mcFn(x) - mbFn(x, delta = deltalvl[3])  , c(.01,10), tol=1e-8)   
eqN <- uniroot(function(x)  mcFn(x) - mbFn(x, delta = deltalvl[2])  , c(.01,10), tol=1e-8)   
eqL <- uniroot(function(x)  mcFn(x) - mbEquityFn(x, delta = deltalvl[2], k = 0.5)  , c(.01,10), tol=1e-8)   
fls <- c(as.numeric(eqL[1]), as.numeric(eqN[1]), as.numeric(eqH[1]), mbEquityFn(f = 0), mbFn(f = 0, delta = deltalvl[2]))

xlims <- c(0, 1)
ylims <- c(0, 1.05)

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
     xaxs = "i", 
     yaxs = "i")

ticksy <- c(0, mcFn(f = fls[1]), mcFn(f = fls[2]), fls[4], fls[5], ylims[2])
ylabels <- c(0, expression(paste(mb[p])), expression(paste(mb[w])), expression(paste(q + delta*(1-k))), expression(paste(q + delta)), NA)
# ticksy <- c(0, mcFn(f = flevels[3]), mcFn(f = flevels[4]), mcFn(f = flevels[5]), ylims[2])
# ylabels <- c(0, expression(paste(delta[b])), expression(paste(delta[n])), expression(paste(delta[e])), NA)
ticksx <- c(0, 0.62, 0.8,  1, xlims[2])
xlabels <- c(0, expression(paste(f[w])), expression(paste(f[p] )), 1, NA)
#xlabels <- c(NA, expression(paste(f[h] )), 0.5, expression(paste(f[b] )), expression(paste(f[n])), expression(paste(f[e])), expression(paste(f[g])), 1.0, NA)


axis(1, at = ticksx, pos = 0, labels = FALSE, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

text(x = c(0, 
           eqL[1], 
           eqN[1],
           xlims[2]
), 
par("usr")[3] - 0.05, cex = labelsize,
labels = xlabels, srt = 0, pos = 1, xpd = TRUE)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Lines for mrs graph
lines(xx1, mcFn(xx1, q = 1.2), col = CBCols[2], lwd = graphlinewidth)
#lines(xx1, mbFn(xx1, q = 1.2, delta = deltalvl[3]), col = CBCols[1], lwd = graphlinewidth)
lines(xx1, mbFn(xx1, q = 1.2, delta = deltalvl[2]), col = CBCols[1], lwd = graphlinewidth)
lines(xx1, mbEquityFn(xx1, q = 1.2, delta = deltalvl[2]), col = CBCols[1], lwd = graphlinewidth)

#Label axes
#mtext(expression(paste("Speed of the machine, ", f)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.31, 0.5*(ylims[2] + ylims[1]), expression(paste("Marginal benefits and costs,", list(mb, mc) )), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(0.5*(xlims[2]), -0.25, expression(paste("Speed of the machine, ", f)), xpd = TRUE, cex = axislabelsize) 

text(0.9, 1.23, expression(paste(mc == q*f )), cex = labelsize)

text(0.15, 1.07, expression(paste(mb[1])), cex = labelsize)
text(0.15, 1.61, expression(paste(mb[2])), cex = labelsize)
#text(0.15, 1.9, expression(paste(mb[3])), cex = labelsize)

#Point p
segments(fls[1], 0, fls[1], mcFn(f = fls[1]), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, mcFn(f = fls[1]), fls[1], mcFn(f = fls[1]), lty = 2, col = grays[20] , lwd = segmentlinewidth)

#Point w
segments(fls[2], 0, fls[2], mcFn(f = fls[2]), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, mcFn(f = fls[2]), fls[2], mcFn(f = fls[2]), lty = 2, col = grays[20] , lwd = segmentlinewidth)

#Point e
# segments(fls[3], 0, fls[3], mcFn(f = fls[3]), lty = 2, col = grays[20] , lwd = segmentlinewidth)
# segments(0, mcFn(f = fls[3]), fls[3], mcFn(f = fls[3]), lty = 2, col = grays[20] , lwd = segmentlinewidth)


#point b
#segments(flevels[3], 0, flevels[3], mc(f = flevels[3]), lty = 2, col = grays[20] , lwd = segmentlinewidth)


#point n
segments(0, mc(f = flevels[4]), xlims[2], mc(f = flevels[4]), lty = 1, col = COLB[4] , lwd = segmentlinewidth)
segments(flevels[4], 0, flevels[4], mc(f = flevels[4]), lty = 2, col = grays[20] , lwd = segmentlinewidth)

#point e
segments(0, mc(f = flevels[5]), xlims[2], mc(f = flevels[5]), lty = 1, col = COLB[4] , lwd = segmentlinewidth)
segments(flevels[5], 0, flevels[5], mc(f = flevels[5]), lty = 2, col = grays[20] , lwd = segmentlinewidth)

#Point h on delta = delta^b
segments(flevels[6], 0, flevels[6], mc(f = flevels[3]), lty = 2, col = grays[20] , lwd = segmentlinewidth)


#segments(0, mc(f = flevels[3]), xlims[2], mc(f = flevels[3]), lty = 1, col = COLB[4] , lwd = segmentlinewidth)
#segments(1, 0, 1, mc(f = 1), lty = 2, col = grays[20] , lwd = segmentlinewidth)
#segments(0, mc(f = 1), xlims[2], mc(f = 1), lty = 1, col = COLB[4] , lwd = segmentlinewidth)

points(fls[1], mcFn(fls[1]), pch = 16, col = "black", cex = 1.5)
points(fls[2], mcFn(fls[2]), pch = 16, col = "black", cex = 1.5)


# points(0.58, mc(f = 0.85), pch = 16, col = "black", cex = 1.5)
# points(0.93, mc(f = 0.85), pch = 16, col = "black", cex = 1.5)
# text(0.61, mc(f = 0.85) - 0.07, expression(paste(g)), cex = labelsize)
# text(0.96, mc(f = 0.85) - 0.07, expression(paste(h)), cex = labelsize)


#text(fls[1], mcFn(fls[1]) + 0.075, expression(paste(b)), cex = labelsize)
text(fls[2], mcFn(fls[2]) + 0.075, expression(paste(w)), cex = labelsize)
text(fls[1], mcFn(fls[1]) + 0.075, expression(paste(p)), cex = labelsize)

#text(fls[3], mcFn(fls[3]) + 0.075, expression(paste(e)), cex = labelsize)


dev.off()
