# TO DO
# Figure out wages shading


require(shape)
require(plotrix)
pdf(file = "employment/employment_wage_macro_zpc.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 3

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

WageFn <- function(H, delta = 5) {
  delta /(1 - H)
}

#COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")
par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 1.2)
ylims <- c(0, 40)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("Total hours of employment as a proportion of labor supply, ", H)),
     ylab = expression(paste("Wage, ", w)),
     xaxt = "n", 
     yaxt = "n",
     cex.lab = axislabelsize, 
     line = 2.5,
     bty = "n", 
     xaxs="i", 
     yaxs="i")

npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], 0.9, length.out = npts)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
xx4 <- seq(xlims[1], 25, length.out = npts2)
xx5 <- seq(xlims[1], 0.75, length.out = npts2)

# Shade below green line
polygon(c(0, xx2, xlims[2]), c(0, WageFn(xx2), 0), border = FALSE, col = COLC[3])

#Draw the lines for the graphs
lines(xx1, WageFn(xx1), col = COL[1], lwd = 4)
#lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 4)
#lines(xx2, solowInfeas(xx2, delta = 5), col = COL[1], lwd = 4, lty = 2)

#Customize ticks and labels for the plot
ticksy <- c(0, 20, 30, 40)
ylabels <- c(0, expression(paste(w^c)), expression(paste(gamma[0])), NA)
ticksx <- c(0, 0.75, xlims[2])
xlabels <- c(0, expression(paste(H,"*")), NA)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Annotation of the  graphs
text(0.72, 35, expression(paste("Wage Curve ", w(H))))

#segments(1, 0, 1, 42, lty = 2, lwd = 3, col = "darkgray")
segments(0.75, 0, 0.75, 20, lty = 2, lwd = 2, col = "darkgray")

#Arrows(0.8, 15, 0.8, 19, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#Arrows(0.8, 15, 0.8, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#text(0.92, 12.5, expression(paste("Employment Rent")))

#Zero profit condition 
segments(0, 20, 0.75, 20, lty = 1, lwd = graphlinewidth, col = COLB[3])
segments(0.75, 20, 1.2, 20, lty = 2, lwd = segmentlinewidth, col = COLB[3])

# Gamma
segments(0, 30, xlims[2], 30, lty = 2, lwd = 2, col = COLA[3])
text(1.05, 31, expression(paste("Output per worker, ", gamma)))


points(0.75, 20, pch = 16, col = "black", cex = 1.5)
text(0.74, 21, expression(paste("n")))


#Unemployment benefits & a
#segments(0, 5, 1.2, 5, lty = 2, lwd = 2, col = "darkgray")
#segments(0, 2.5, 1.2, 2.5, lty = 2, lwd = 2, col = "darkgray")

#Zero profit condition
text(1.02, 21, expression(paste("Competition condition ", w == w^c)))
text(0.25, 23, expression(paste("Firms leaving")))
text(0.25, 17, expression(paste("Firms entering")))
text(0.25, 4, expression(paste("No production")))

#text(0.97, 6, expression(paste(B + a/t)))
#text(0.97, 3.5, expression(paste(B, " (unemployment benefits)")))
#text(1.08, 36, expression(paste("level of")))
#text(1.08, 34, expression(paste("employment, ", bar(H))))


# Arrows 
text(0.55, 25, expression(paste("Profits")))
Arrows(0.5, 22, 0.5, 28, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
Arrows(0.5, 28, 0.5, 22, col = "black", lty = 1, lwd = 2, arr.type = "triangle")

text(0.55, 15, expression(paste("Wages")))
Arrows(0.5, 18, 0.5, 12, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
Arrows(0.5, 12, 0.5, 18, col = "black", lty = 1, lwd = 2, arr.type = "triangle")


dev.off()

