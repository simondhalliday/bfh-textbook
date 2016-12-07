require(shape)
pdf(file = "coordination_failures/worksheet_brf_indiffs.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 4, 4, 4))

uA <- function(ea, eb, alpha = 20, beta = 1/60) {
  alpha*(1 - beta*eb)*ea - 0.5*(ea)^2
}

uB <- function(ea, eb, alpha = 20, beta = 1/60) {
  alpha*(1 - beta*ea)*eb - 0.5*(eb)^2
}

brfB <- function(ea, alpha = 20, beta = 1/60) {
  alpha*(1 - beta*ea)
}

brfA <- function(ea, alpha = 20, beta = 1/60) {
  (alpha - ea)/(alpha * beta)
}

brfPE <- function(ea, alpha = 20, beta = 1/60) {
  alpha*(1 - beta*ea)
}

indiffA <- function(ea, alpha = 20, beta = 1/60, uA = 46.08) {
  1/beta - (0.5*ea)/(alpha*beta) - uA/(alpha*beta*ea)
}

indiffB <- function(eb, alpha = 20, beta = 1/60, uB = 46.08) {
  (alpha*eb - uB - 0.5*eb^2)/(alpha*beta*eb)
}

indiffB2 <- function(ea, alpha = 20, beta = 1/60, uB = 46.08) {
  sqrt(alpha^2*(beta*ea - 1)^2 - 2*uB) - alpha*beta*ea + alpha
}

indiffB3 <- function(ea, alpha = 20, beta = 1/60, uB = 46.08) {
  -sqrt(alpha^2*(beta*ea - 1)^2 - 2*uB) - alpha*beta*ea + alpha
}


xlims <- c(0, 62)
ylims <- c(0, 62)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(18, 72, 112.5)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

ticksy <- seq(from = 0, to = ylims[2], by = 5)
ylabels <- seq(from = 0, to = ylims[2], by = 5)
ticksx <- seq(from = 0, to = xlims[2], by = 5)
xlabels <- seq(from = 0, to = xlims[2], by = 5)
ticksy <- c(0, brfA(ea = 15), brfA(ea = 12), brfA(ea = 6), 60, 62)
ylabels <- c(NA, expression(paste(e^{BN})), expression(paste(e[1]^B)), expression(paste(e[2]^B)),expression(paste(frac(1, beta) ==60)) , NA)
ticksx <- c(0, 6, 12, 15, 20, 60)
xlabels <- c(NA, NA, NA, expression(paste(e^{AN})), expression(paste(alpha == 20)), NA)


axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
lines(xx1, brfA(xx1, alpha = 20, beta = 1/60), col = COLA[4], lwd = graphlinewidth)


contour(y, x, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLA[3],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE) 

mtext(expression(paste("A's effort,", e^A)), side=1, line = 2.5, cex = axislabelsize)
text(-5, 0.5*ylims[2], expression(paste("B's effort,", e^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the iso-welfare functions for the HG, Aisha
text(2.2, 10, expression(u[1]^A))
text(6, 10, expression(u[2]^A))
text(10.8, 10, expression(u[3]^A))

#Label Nash Equilibrium 
segments(0, 15, 15, 15, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(15, 0, 15, 15, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(15, 15, pch = 16, col = "black", cex = 1.5)
#text(11.3, 10.1, expression(paste("Nash Equilibrium")))
text(14, 14, expression(paste("n")))



segments(0, brfA(ea = 12), 12 + 2, brfA(ea = 12), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(12, brfA(ea = 12) - 2, 12, brfA(ea = 12), lty = 2, col = "gray" , lwd = segmentlinewidth)
points(12, brfA(ea = 12), pch = 16, col = "black", cex = 1.5)
text(12 - 1, brfA(ea = 12) - 1, expression(paste("j")))

segments(0, brfA(ea = 6), 6 + 2, brfA(ea = 6), lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(6, brfA(ea = 6) - 2, 6, brfA(ea = 6), lty = 2, col = "gray" , lwd = segmentlinewidth)
points(6, brfA(ea = 6), pch = 16, col = "black", cex = 1.5)
text(6 - 1, brfA(ea = 6) - 1, expression(paste("k")))

text(15, 55, expression(paste("A's best-response function")))
text(15, 50, expression(paste(e^A*(e^B) == 20(1 - frac(1,60)*e^B) )))
Arrows(3, 52.5, 7, 52.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


# text(10.2, 17, expression(paste(mrs^{A}*(list(e^A, e^B)) == frac(u[e^A], u[e^B]), phantom() == -frac(alpha*(1 - beta*e^B) - e^A, alpha*beta*e^A), phantom() == 0, " at maximum of ", u[1]^A)))
# Arrows(6.3, 16.5, 6.3, 15.25, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)



dev.off()
