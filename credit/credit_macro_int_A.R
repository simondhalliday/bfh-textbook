# CHANGE COLOR AND TRANSPARENCY OF RECT

require(shape)
pdf(file = "credit/credit_macro_int_A.pdf", width = 9, height = 7)

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
ticksx2 <- c(0, 20)
ticksx3 <- c(25, 45)
xlabels1 <- c(NA, expression(paste(I[A])))
xlabels2 <- c(NA, expression(paste(I[B])))

axis(1, at = ticksx2, pos = 0, labels = xlabels1)
axis(1, at = ticksx3, pos = 0, labels = xlabels2)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

text(-4.5, 0.5*(ylims[2] + ylims[1]), expression(paste("Interest Rate, Profit Rate")), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(0.5*(xlims[2]) - 12.5, -2.5, expression(paste("Firm A")), xpd = TRUE, cex = axislabelsize) 
text(0.5*(xlims[2]) + 12.5, -2.5, expression(paste("Firm B")), xpd = TRUE, cex = axislabelsize) 


# I_A RECT
rect(0, 0, 5, 29,
     col=COLA[2], border=TRUE, lty=1)

rect(5, 0, 15, 22.5,
     col=COLA[2], border=TRUE, lty=1)

rect(15, 0, 18, 5,
     col=COLA[2], border=TRUE, lty=1)

# I_B RECT
rect(25, 0, 30, 20,
     col=COLB[2], border=TRUE, lty=1)

rect(30, 0, 40, 15,
     col=COLB[2], border=TRUE, lty=1)

rect(40, 0, 43, 10.5,
     col=COLB[2], border=TRUE, lty=1)


# Segments
segments(0, 10, xlims[2], 10, col = "gray", lty = 2, lwd = segmentlinewidth)
segments(0, 25, xlims[2], 25, col = "gray", lty = 2, lwd = segmentlinewidth)

# Labels --- Fig A

text(11.25, 29, expression(paste("Expected Profit Rate")), cex = labelsize)
text(8, 27.5, expression(paste("Project 1")), cex = labelsize)
text(18, 22.5, expression(paste("Project 2")), cex = labelsize)
text(18, 6, expression(paste("Project 3")), cex = labelsize)

# Labels --- Fig B

text(36.22, 20, expression(paste("Expected Profit Rate")), cex = labelsize)
text(33, 18.5, expression(paste("Project 1")), cex = labelsize)
text(42.6, 15, expression(paste("Project 2")), cex = labelsize)
text(42.6, 11.5, expression(paste("Project 3")), cex = labelsize)


dev.off()