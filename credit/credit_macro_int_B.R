# CHANGE COLOR AND TRANSPARENCY OF RECT

require(shape)
pdf(file = "credit/credit_macro_int_B.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(6, 6, 4, 4))

xlims <- c(0, 45)
ylims <- c(0, 30)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(46.08, 55, 64)
b <- c(46.08, 55, 64)

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
ticksy <- c(0, 5, 10, 15, 20, 25, ylims[2])
ylabels <- c(NA, expression(paste("1 %")), expression(paste("2 %")), expression(paste("3 %")), expression(paste("4 %")), expression(paste("5 %")), NA)
ticksx <- c(0, xlims[2])
#ticksx2 <- c(0, 20)
#ticksx3 <- c(25, 45)
xlabels <- c(NA, expression(paste(I[A] + I[B])))


axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

text(-4.5, 0.5*(ylims[2] + ylims[1]), expression(paste("Interest Rate, Profit Rate")), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(0.5*(xlims[2]), -4, expression(paste("Aggregate Economy: Both Firms")), xpd = TRUE, cex = axislabelsize) 


# I_A RECT
rect(0, 0, 5, 29,
     col=COLA[1], border=TRUE, lty=1)

rect(5, 0, 15, 22.5,
     col=COLA[1], border=TRUE, lty=1)

rect(33, 0, 36, 5, # width = 3
     col=COLA[1], border=TRUE, lty=1)

# I_B RECT
rect(15, 0, 20, 20, # width = 5
     col=COLB[1], border=TRUE, lty=1)

rect(20, 0, 30, 15, # width = 10
     col=COLB[1], border=TRUE, lty=1)

rect(30, 0, 33, 10.5, # width = 3
     col=COLB[1], border=TRUE, lty=1)


# Segments
segments(0, 10, xlims[2], 10, col = "darkgray", lty = 2, lwd = segmentlinewidth)
segments(0, 25, xlims[2], 25, col = "darkgray", lty = 2, lwd = segmentlinewidth)

# Labels
text(33, 17.5, expression(paste("Fall in Interest Rate")), cex = labelsize)
Arrows(39, 11, 39, 24, col = "black", code = 3, lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
#Arrows(39, 23, 39, 12, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(17.5, -1.6, expression(paste("Increase in Investment")), cex = labelsize, xpd = TRUE)
Arrows(6, -1, 29, -1, col = "black", code = 3, lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5, xpd = TRUE)


dev.off()