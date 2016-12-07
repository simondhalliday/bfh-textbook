require(shape)
pdf(file = "employment/relationships_w_e.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(0, 0, 0, 0))

xlims <- c(0, 20)
ylims <- c(2, 16)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(72, 81, 90, 91.125)
b <- c(61, 72, 81, 90)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

ticksy <- seq(from = 0, to = ylims[2], by = 2)
ylabels <- seq(from = 0, to = ylims[2], by = 2)
ticksx <- seq(from = 0, to = xlims[2], by = 2)
xlabels <- seq(from = 0, to = xlims[2], by = 2)


text(2, 14, expression(paste(bold("Unemployment"))))
text(2, 13.3, expression(paste(bold("benefit, B"))))
Arrows(4, 13.65, 5.5, 12.6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


text(2, 11, expression(paste(bold("Expected duration"))))
text(2, 10.3, expression(paste(bold("of employment, s"))))
Arrows(4, 10.65, 5.5, 11.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


text(7, 12, expression(paste("reservation wage")))
text(7, 11.3, expression(paste("(fallback), ", bar(w))))
Arrows(8.5, 11.65, 10, 10.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(7.2, 8, expression(paste("wage, ", w)), col = "red")
Arrows(8, 8, 10, 9.2, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(11, 10.5, expression(paste("cost of")))
text(11, 9.8, expression(paste("job loss, ", Delta)))
Arrows(11.8, 10.2, 13.3, 9.4, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(13.5, 12.5, expression(paste("termination")), col = "red")
text(13.5, 11.8, expression(paste("schedule, ", t == (1 - e))), col = "red")
Arrows(13.6, 11.4, 14.3, 9.6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


text(15, 9, expression(paste("marginal benefit")))
text(15, 8.3, expression(paste("of effort")))
Arrows(16, 8.5, 18, 7.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(14.7, 6.2, expression(paste(bold("marginal cost of"))))
text(14.7, 5.7, expression(paste(bold("effort as a function"))))
text(14.7, 5.2, expression(paste(bold("of effort level"))))
text(14.7, 4.6, expression(paste(bold("(disutility)"))))
Arrows(16, 5.3, 18, 6.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(19, 7, expression(paste("effort, ", e)), col = "blue")


dev.off()
