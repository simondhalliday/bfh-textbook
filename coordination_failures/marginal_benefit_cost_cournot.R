require(shape)
pdf(file = "coordination_failures/marginal_benefit_cost_cournot.pdf", width = 9, height = 7)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(5, 7, 1, 1))

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
a <- c(12, 46.08, 90)

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
ticksy <- c(0, MBenefit(12), MBenefit(15, eb = 0), MBenefit(ea = 0), 30, ylims[2])
ylabels <- c(NA, expression(paste(u[h^A1]^A)), expression(paste(u[h^A2]^A)), expression(paste(alpha - beta*h^B)), expression(paste(alpha)), NA)
ticksx <- c(0, 12, 15, 24, 30, xlims[2])
xlabels <- c(NA, expression(paste(h[1]^A)), expression(paste(h[2]^A)), expression(paste(frac(alpha - beta*h^b,2*beta) )), expression(paste(frac(alpha,2*beta) )), NA)

axis(1, at = ticksx, pos = 0, labels = FALSE ,las = 1, cex.axis = labelsize - 0.05)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize - 0.05)
text(x = c(0, 12, 15, 24, 30, xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE, cex = labelsize - 0.05)

mtext(expression(paste("Aram's hours, ", h^A)), side=1, line = 4, cex = axislabelsize)
text(-5, 0.5*ylims[2], expression(paste("Aram's marginal utility and disutility, ", u[h^A]^A)), xpd = TRUE, cex = axislabelsize, srt = 90) 

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(4, 8, length.out = npts)

lines(xx1, MBenefit(xx1, eb = 12), col = COLA[4], lwd = graphlinewidth)
lines(xx1, MBenefit(xx1, eb = 0), col = COLA[4], lwd = graphlinewidth)
lines(xx1, MCost(xx1), col = COLB[4], lwd = graphlinewidth)

#For NE hours = 12
segments(0, 12, 12, 12, lty = 2, col =  "gray", lwd = segmentlinewidth)
segments(12, 0, 12, MBenefit(12), lty = 2, col = "gray", lwd = segmentlinewidth)

#For P-efficient hours = 10
segments(0, MBenefit(15, eb = 0), 15, MBenefit(15, eb = 0), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(15, 0, 15, MBenefit(15, eb = 0),  lty = 2, col = "gray" , lwd = segmentlinewidth)


text(11.5, 26, expression(paste("Marginal Benefit")), cex = labelsize)
text(11.5, 24.3, expression(paste(mb^A== alpha - 2*beta*h^A )), cex = labelsize)
text(11.5, 22.5, expression(paste("when, ", h^B== 0 )), cex = labelsize)

text(25.5, 20, expression(paste("Marginal Disutility,")), cex = labelsize)
text(25.5, 18.6, expression(paste(mc^A == h^A)), cex = labelsize)


text(24.5, 15, expression(paste("Marginal Benefit ")), cex = labelsize)
text(24.5, 13.5, expression(paste(mb^A == alpha - beta*h^B - 2*beta*h^A)), cex = labelsize)
text(24.5, 12, expression(paste("when, ", h^B > 0 )), cex = labelsize)
Arrows(21.5, 12.5, 17, 8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


points(12, 12, pch = 16, col = "black", cex = 1.5)
text(12 + 0.85, 12, expression(paste(f)), cex = labelsize)

points(15, 15, pch = 16, col = "black", cex = 1.5)
text(15 + 0.85, 15, expression(paste(g)), cex = labelsize)

#Label the iso-welfare functions for the HG, Aisha
text(9.7, 60, expression(u[1]^A),  cex= labelsize - 0.05)
text(7.6, 60, expression(u[2]^A), cex= labelsize - 0.05)
text(4.8, 60, expression(u[3]^A), cex= labelsize - 0.05)



dev.off()
