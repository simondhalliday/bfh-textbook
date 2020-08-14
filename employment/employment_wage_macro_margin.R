require(shape)
require(plotrix)
pdf(file = "employment/employment_wage_macro_margin.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 2
labelsize <- 1.8
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

WageFn <- function(h, ubar = 3, B = 2, t = 0.8) {
  B + ubar + (ubar - ubar*t +  ubar*t*h )/(t*(1 - h))
}


#COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")
par(mar =  c(5, 5, 3, 2))
xlims <- c(0, 1.001)
ylims <- c(0, 40)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")),
     xaxt = "n", 
     yaxt = "n",
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i")

npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], 0.99, length.out = npts)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
xx4 <- seq(xlims[1], 25, length.out = npts2)
xx5 <- seq(xlims[1], 0.75, length.out = npts2)

# Shade below green line
# polygon(c(0, xx2, xlims[2]), 
#         c(0, WageFn(xx2), 0), border = FALSE, col = COLA[1])
# rect(0.99, 0, 1.2, 40, border = FALSE, col = COLA[1])

#Axis labels
text(0.5, -6, expression(paste("Hours of employment as a fraction of labor supply, ", H)), cex = axislabelsize, xpd = TRUE)
text(-0.105, 20, expression(paste("Wage, ", w)), cex = axislabelsize, xpd = TRUE, srt = 90)


#Draw the lines for the graphs
lines(xx1, WageFn(xx1), col = COLA[4], lwd = graphlinewidth)

#Customize ticks and labels for the plot
ticksy <- c(0, 20, 40)
ylabels <- c(0, expression(paste(w^C)), NA)
ticksx <- c(0, 0.7917, xlims[2])
xlabels <- c(0, expression(paste(H^N)), 1)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#Annotation of the  graphs
text(0.91, 43, expression(paste("Wage curve")), cex = labelsize, xpd = TRUE)
text(0.91, 40.5, expression(paste(w^N*(H))), cex = labelsize, xpd = TRUE)
#text(0.75, 38, expression(paste("Employment level, ", H^N)), cex = labelsize,  xpd = TRUE)
text(0.67, 30, expression(paste("Employment")), cex = labelsize,  xpd = TRUE)
text(0.67, 28, expression(paste("level, ", H^N)), cex = labelsize,  xpd = TRUE)

segments(0.7917, 0, 0.7917, ylims[2], lty = 2, lwd = segmentlinewidth, col = grays[20])

#Arrows(0.8, 15, 0.8, 19, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#Arrows(0.8, 15, 0.8, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#text(0.92, 12.5, expression(paste("Employment Rent")))

#Zero profit condition 
segments(0, 20, 0.7917, 20, lty = 1, lwd = graphlinewidth, col = COLB[3])
segments(0.7917, 20, 1.2, 20, lty = 2, lwd = segmentlinewidth, col = COLB[3])
# Gamma
# segments(0, 30, xlims[2], 30, lty = 2, lwd = 2, col = COLA[3])
# text(1.11, 33, expression(paste("Output per")), cex = labelsize,  xpd = TRUE)
# text(1.11, 31, expression(paste("worker, ", gamma)), cex = labelsize,  xpd = TRUE)

# Supply of labor
segments(xlims[2], 0, xlims[2], ylims[2], lty = 2, lwd = segmentlinewidth, col = grays[20], xpd = TRUE)
text(0.9, 6, expression(paste("Supply of")), cex = labelsize,  xpd = TRUE)
text(0.9, 4, expression(paste("labor, ", H == 1)), cex = labelsize,  xpd = TRUE)


points(0.7917, 20, pch = 16, col = "black", cex = 1.5)
text(0.77, 21, expression(paste("n")), cex = labelsize)


#Unemployment benefits & a
#segments(0, 5, 1.2, 5, lty = 2, lwd = 2, col = "darkgray")
#segments(0, 2.5, 1.2, 2.5, lty = 2, lwd = 2, col = "darkgray")

#Zero profit condition
# text(1.12, 19, expression(paste("Competition")), cex = labelsize, xpd = TRUE)
# text(1.12, 17, expression(paste("condition, ", w^C)), cex = labelsize, xpd = TRUE)
#text(1.02, 21.25, expression(paste(w == bar(Delta))), cex = labelsize, xpd = TRUE)

# text(0.25, 23, expression(paste("Firms leaving")), cex = labelsize)
# text(0.25, 17, expression(paste("Firms entering")), cex = labelsize)

# text(0.325, 17, expression(paste("Firms entering")), cex = labelsize)
# Arrows(0.2, 15, 0.5, 15, col = "black", lty = 1, lwd = 2, arr.type = "triangle")



# Arrows(0.6, 19, 0.6, 10, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
# text(0.42, 28, expression(paste("If the ", w>w^c,  " profits will")), cex = labelsize)
# text(0.42, 26, expression(paste("be insufficient and firms")), cex = labelsize)
# text(0.42, 24, expression(paste("will leave")), cex = labelsize)
# text(0.32, 24, expression(paste("Firms exiting")), cex = labelsize)
# Arrows(0.43, 22, 0.15, 22, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
# 
# 
# text(0.6, 7, expression(paste("No")), cex = labelsize)
# text(0.6, 5, expression(paste("production")), cex = labelsize)

#text(0.97, 6, expression(paste(B + a/t)))
#text(0.97, 3.5, expression(paste(B, " (unemployment benefits)")))
#text(1.08, 36, expression(paste("level of")))
#text(1.08, 34, expression(paste("employment, ", bar(H))))

#Profits
# brackets(x1 = 1.02, y1 = 29.8, x2 = 1.02, y2 = 20.2,  
#          ticks = 0.5, curvature = 0.5, type = 1, 
#          col = "black", lwd = 2, h = 0.03,
#          lty = 1, xpd = TRUE)
# text(1.14, 25, expression(paste("Profits, ", pi^E)), cex = labelsize, xpd = TRUE)

# text(0.55, 15, expression(paste("Wages")))
# Arrows(0.5, 18, 0.5, 12, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
# Arrows(0.5, 12, 0.5, 18, col = "black", lty = 1, lwd = 2, arr.type = "triangle")


dev.off()

