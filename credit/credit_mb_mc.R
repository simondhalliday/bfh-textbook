require(shape)
pdf(file = "credit/credit_mb_mc.pdf", width = 7, height = 7)

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
grays <- gray.colors(25, start = 1, end = 0)

par(mar =  c(5, 6, 1, 1))

mc <- function(f, q = 2) {
  -q + 2*q*f
}

xlims <- c(0, 1.05)
ylims <- c(0, 2.05)

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

flevels <- c(ylow(delta = 0.34, ybar = 0.0625), #point h
             0.5,
             brfFn(delta = 0.34), #point b
             brfFn(delta = 0.5), #point n
             brfFn(delta = 0.655), #point e
             yhigh(delta = 0.34, ybar = 0.0625) #point g
             )

ticksy <- c(0, mc(f = flevels[3]), mc(f = flevels[4]), mc(f = flevels[5]), ylims[2])
ylabels <- c(0, expression(paste(delta[b])), expression(paste(delta^N)), expression(paste(delta[e])), NA)
ticksx <- c(0, flevels, 1, xlims[2])
xlabels <- c(NA, expression(paste(f[h] )), 0.5, expression(paste(f[b] )), expression(paste(f^{N} )), expression(paste(f[e])), expression(paste(f[g])), 1.0, NA)



axis(1, at = ticksx, pos = 0, labels = FALSE, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

text(x = c(0, 
           ylow(delta = 0.34, ybar = 0.0625) - 0.02, 
           0.5,
           brfFn(delta = 0.34), 
           brfFn(delta = 0.5), 
           brfFn(delta = 0.655),
           yhigh(delta = 0.34, ybar = 0.0625), 
           1, 
           xlims2[2]
           ), 
     par("usr")[3] - 0.05, cex = labelsize,
     labels = xlabels, srt = 0, pos = 1, xpd = TRUE)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Lines for mrs graph
lines(xx1, mc(xx1, q = 2), col = COLA[4], lwd = graphlinewidth)

#Label axes
#mtext(expression(paste("Speed of the machine, ", f)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.15, 0.5*(ylims[2] + ylims[1]), expression(paste("Marginal benefit and marginal costs,", list(mb, mc) )), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(0.5*(xlims[2]), -0.25, expression(paste("Speed of the machine, ", f)), xpd = TRUE, cex = axislabelsize) 

text(0.8, 1.9, expression(paste(mc == -q*(1 - 2*f) )), cex = labelsize)
text(0.25, mc(flevels[5]) + 0.07, expression(paste(mb[e] == delta[e] )), cex = labelsize)
text(0.25, mc(flevels[4]) + 0.07, expression(paste(mb^N == delta^N )), cex = labelsize)
text(0.25, mc(flevels[3]) + 0.07, expression(paste(mb[b] == delta[b] )), cex = labelsize)

#Point h on delta = delta^b
segments(flevels[1], 0, flevels[1], mc(f = flevels[3]), lty = 2, col = grays[20] , lwd = segmentlinewidth)

#point b
segments(flevels[3], 0, flevels[3], mc(f = flevels[3]), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, mc(f = flevels[3]), xlims[2], mc(f = flevels[3]), lty = 1, col = COLB[4] , lwd = segmentlinewidth)

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

points(flevels[1], mc(flevels[3]), pch = 16, col = "black", cex = 1.5)
points(flevels[3], mc(flevels[3]), pch = 16, col = "black", cex = 1.5)
points(flevels[6], mc(flevels[3]), pch = 16, col = "black", cex = 1.5)


points(flevels[4], mc(flevels[4]), pch = 16, col = "black", cex = 1.5)
points(flevels[5], mc(flevels[5]), pch = 16, col = "black", cex = 1.5)

# points(0.58, mc(f = 0.85), pch = 16, col = "black", cex = 1.5)
# points(0.93, mc(f = 0.85), pch = 16, col = "black", cex = 1.5)
# text(0.61, mc(f = 0.85) - 0.07, expression(paste(g)), cex = labelsize)
# text(0.96, mc(f = 0.85) - 0.07, expression(paste(h)), cex = labelsize)



text(flevels[5] + 0.025, mc(flevels[5]) - 0.05, expression(paste(e)), cex = labelsize)
text(flevels[4] + 0.025, mc(flevels[4]) - 0.05, expression(paste(n)), cex = labelsize)
text(flevels[3] + 0.025, mc(flevels[3]) - 0.05, expression(paste(b)), cex = labelsize)
text(flevels[1] + 0.025, mc(flevels[3]) - 0.05, expression(paste(h)), cex = labelsize)
text(flevels[6] + 0.025, mc(flevels[3]) - 0.05, expression(paste(g)), cex = labelsize)


dev.off()
