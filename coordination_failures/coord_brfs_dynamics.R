require(shape)
pdf(file = "coordination_failures/coord_brfs_dynamics.pdf", width = 9, height = 7)

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

par(mar =  c(7, 7, 1, 1))

uA <- function(ea, eb, alpha = 30, beta = 1/2) {
  (alpha - beta*(ea+eb))*ea - 0.5*(ea)^2
}

uB <- function(ea, eb, alpha = 30, beta = 1/2) {
  (alpha - beta*(ea+eb))*eb - 0.5*(eb)^2
}

brfB <- function(ea, alpha = 30, beta = 1/2) {
  (alpha - beta*ea)/(1 + 2*beta)
}

brfA <- function(ea, alpha = 30, beta = 1/2) {
  (alpha - ea*(1 + 2*beta))/(beta)
}

brfPE <- function(ea, alpha = 30, beta = 1/8) {
  alpha*(1 - beta*ea)
}

hANE <- function(alpha, beta = 1/2){
  alpha/(1 + 3*beta)
}

intercept1 <- function(alpha, beta = 1/2){
  alpha/(1 + 2*beta)
}

xlims <- c(0, 24)
ylims <- c(0, 24)

npts <- 501 

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

ticksy <- c(0, hANE(alpha = 30), intercept1(alpha = 30), ylims[2])
ylabels <- c(NA, 12, 15, NA)
ticksx <- c(0, hANE(alpha = 30), intercept1(alpha = 30), xlims[2])
xlabels <- c(NA, 12, 15, NA)

axis(1, at = ticksx,  pos = 0, labels = FALSE)
text(x = c(0, hANE(alpha = 30), intercept1(alpha = 30), xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE, cex = labelsize)

axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(9.4, 10.55, length.out = npts)


lines(xx1, brfA(xx1, alpha = 30, beta = 1/2), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfB(xx1, alpha = 30, beta = 1/2), col = COLB[4], lwd = graphlinewidth)

text(0.5*xlims[2], -3.5, expression(paste("A's hours, ", h^A)), xpd = TRUE, cex = axislabelsize)
text(-3, 9, expression(paste("B's hours, ", h^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 


segments(0, 12, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(12, 0, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)


# Label Nash Eq. ----------------------------------------------------------

points(hANE(alpha = 30), hANE(alpha = 30), pch = 16, col = "black", cex = 1.5)
text(hANE(alpha = 31.5), hANE(alpha = 30) + 0.4, expression(paste(n)), cex = labelsize)

# B's BRF -----------------------------------------------------------------

text(13, 23, expression(paste("B's best response")), cex = labelsize - 0.05)
text(13, 22, expression(paste("function")), cex = labelsize  - 0.05)
#text(5, 15.2, expression(paste(h^B*(h^A) == frac(alpha - beta*h^A, 1 + 2*beta) )), cex = labelsize)

# A' BRF ----------------------------------------------------- -------------

text(20, 12, expression(paste("A's best response")), cex = labelsize - 0.05)
text(20, 11, expression(paste("function")), cex = labelsize - 0.05)
#text(17, 3.2, expression(paste(h^A*(h^B) == frac(alpha - beta*h^B, 1 + 2*beta) )), cex = labelsize)

# Dynamic Arrows ----------------------------------------------------------

# Q1 
Arrows(19, 19, 19, 16, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
Arrows(19, 19, 16, 19, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

# Q2
Arrows(5, 19, 5, 16, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
Arrows(5, 19, 8, 19, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

# Q3
Arrows(5, 5, 5, 8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
Arrows(5, 5, 8, 5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

# Q4
Arrows(19, 5, 16, 5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
Arrows(19, 5, 19, 8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


# Dynamics Labels ---------------------------------------------------------



# Q1
text(21.5, 17, expression(MB^B < MC^B), cex = labelsize - 0.05)
text(17, 20.5, expression(MB^A < MC^A), cex = labelsize - 0.05)

# Q2
text(7, 20.5, expression(MB^A > MC^A), cex = labelsize - 0.05)
text(2.5, 17, expression(MB^B < MC^B), cex = labelsize - 0.05)

# Q3
text(2.5, 7, expression(MB^B > MC^B), cex = labelsize - 0.05)
text(7, 4, expression(MB^A > MC^A), cex = labelsize - 0.05)

# Q4
text(21.5, 7, expression(MB^B > MC^B), cex = labelsize - 0.05)
text(17, 4, expression(MB^A < MC^A), cex = labelsize - 0.05)




dev.off()