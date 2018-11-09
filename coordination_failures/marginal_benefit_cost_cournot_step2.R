require(shape)
pdf(file = "coordination_failures/marginal_benefit_cost_cournot_step2.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 6, 4, 4))

MBenefit <- function(ea, eb = 12, alpha = 30, beta = 1/2) {
  (alpha - beta*eb - 2*beta*ea)
}

MCost <- function(ea, slope = 1, intercept = 0){
  slope*ea
}


xlims <- c(0, 30)
ylims <- c(0, 30)

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
ticksy <- c(0, MBenefit(12), MBenefit(ea = 0), ylims[2])
ylabels <- c(NA, expression(paste(u[h^A1]^A)), expression(paste(alpha - beta*h^B)), NA)
ticksx <- c(0, 12,  24, xlims[2])
xlabels <- c(NA, expression(paste(h[1]^A)),  expression(paste(frac(alpha - beta*h^b,2*beta) )), NA)

axis(1, at = ticksx, pos = 0, labels = FALSE)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)
text(x = c(0, 12, 24, xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE)

mtext(expression(paste("Aram's hours, ", h^A)), side=1, line = 2.5, cex = axislabelsize)
text(-3.8, 0.5*ylims[2], expression(paste("Aram's marginal utility and disutility, ", u[h^A]^A)), xpd = TRUE, cex = axislabelsize, srt = 90) 

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(4, 8, length.out = npts)

lines(xx1, MBenefit(xx1, eb = 12), col = COLA[4], lwd = graphlinewidth)
#lines(xx1, MBenefit(xx1, eb = 0), col = COLA[4], lwd = graphlinewidth)
lines(xx1, MCost(xx1), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, indiffA(xx1, ua = 46.08), col = COLA[4], lwd = graphlinewidth)
#lines(xx2, mrsA(xx2), col = "gray", lwd = graphlinewidth, lty = 2)


#segments(0, 18, xlims[2], 18, lty = 2, col =  COLB[3] , lwd = segmentlinewidth)

#segments(0, 6, xlims[2], 6, lty = 2, col = COLB[3] , lwd = segmentlinewidth)

#For NE hours = 12
segments(0, 12, 12, 12, lty = 2, col =  "gray", lwd = segmentlinewidth)
segments(12, 0, 12, MBenefit(12), lty = 2, col = "gray", lwd = segmentlinewidth)

#For P-efficient hours = 10
# segments(0, MBenefit(15, eb = 0), 15, MBenefit(15, eb = 0), lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(15, 0, 15, MBenefit(15, eb = 0),  lty = 2, col = "gray" , lwd = segmentlinewidth)


#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
# text(9.5, 30, expression(paste("Slope at ", (list(e^A, y^A)) == (list(6, 28)), " is ", e^A == 6)))

# text(10, 26, expression(paste("Marginal Benefit")), cex = labelsize)
# text(10, 24.5, expression(paste(mb^A== alpha - 2*beta*h^A )), cex = labelsize)
# text(10, 23, expression(paste("when, ", h^B== 0 )), cex = labelsize)
#text(2, 12.5, expression(paste(mb[3]^A == alpha, " when ", h^b == 0)), cex = labelsize)
#text(0.7, 4.5, expression(paste(mb[1]^A)), cex = labelsize)

text(24, 18.9, expression(paste("Marginal Disutility, ", mc^A == h^A)), cex = labelsize)
#text(24, 16, expression(paste("Marginal Disutility", phantom() == h[2]^A)), cex = labelsize)

text(24, 6, expression(paste("Marginal Benefit ")), cex = labelsize)
text(24, 4.5, expression(paste(mb^A == alpha - beta*h^B - 2*beta*h^A)), cex = labelsize)
text(24, 3, expression(paste("when, ", h^B > 0 )), cex = labelsize)
#Arrows(21.5, 12.5, 17, 8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


# text(3, 11, expression(paste(alpha, " increases, or ")), cex = labelsize)
# text(3, 10.5, expression(paste(h^B, " decreases")), cex = labelsize)
# 
# Arrows(1.5, 7.5, 1.5, 4.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# text(3, 5.5, expression(paste(alpha, " decreases, or ")), cex = labelsize)
# text(3, 5, expression(paste(h^B, " increases")), cex = labelsize)

# segments(28, 0, 28, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(12, 12, pch = 16, col = "black", cex = 1.5)
text(12 + 0.75, 12, expression(paste(f)), cex = labelsize)
# 
# segments(16, 0, 16, 18, lty = 2, col = "gray" , lwd = segmentlinewidth)
# points(15, 15, pch = 16, col = "black", cex = 1.5)
# text(15 + 0.75, 15, expression(paste(g)), cex = labelsize)
# 
# segments(40, 0, 40,  MBenefit(40), lty = 2, col = "gray" , lwd = segmentlinewidth)
# points(40, MBenefit(40), pch = 16, col = "black", cex = 1.5)
# text(40 + 0.75, MBenefit(40) + 0.75, expression(paste(e)), cex = labelsize)

#Label the iso-welfare functions for the HG, Aisha
text(9.7, 60, expression(u[1]^A))
text(7.6, 60, expression(u[2]^A))
text(4.8, 60, expression(u[3]^A))



dev.off()
