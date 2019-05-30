require(shape)
pdf(file = "coordination_failures/coord_brfs_only_cournot/coord_brfs_only_cournot.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(4, 6, 1, 1))

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

xlims <- c(0, 22)
ylims <- c(0, 22)

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
ylabels <- c(NA,  expression(paste(h^{BN})), expression(paste(frac(alpha, 1 + 2*beta))), NA)
ticksx <- c(0, hANE(alpha = 30), intercept1(alpha = 30), xlims[2])
xlabels <- c(NA, expression(paste(h^{AN})), expression(paste(frac(alpha, 1 + 2*beta))), NA)

axis(1, at = ticksx,  pos = 0, labels = FALSE)
text(x = c(0, hANE(alpha = 30), intercept1(alpha = 30), xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE)

axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(9.4, 10.55, length.out = npts)


lines(xx1, brfA(xx1, alpha = 30, beta = 1/2), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfB(xx1, alpha = 30, beta = 1/2), col = COLB[4], lwd = graphlinewidth)


text(0.5*xlims[2], -1.9, expression(paste("A's hours, ", h^A)), xpd = TRUE, cex = axislabelsize)
text(-1.7, 9, expression(paste("B's hours, ", h^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 


segments(0, 12, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(12, 0, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)


#Label Nash Equilibrium 
points(hANE(alpha = 30), hANE(alpha = 30), pch = 16, col = "black", cex = 1.5)
text(hANE(alpha = 30) + 2.2, hANE(alpha = 30) + 0.4, expression(paste("Nash Equilibrium")))
text(hANE(alpha = 30) - 0.4, hANE(alpha = 30) - 0.4, expression(paste(n)))


#B's brf
text(3, 17, expression(paste("B's best response")))
text(3, 16.3, expression(paste("function")))
text(3, 15.2, expression(paste(h^B*(h^A) == frac(alpha - beta*h^A, 1 + 2*beta) )))

#A's brf
text(16, 5, expression(paste("A's best response")))
text(16, 4.3, expression(paste("function")))
text(16, 3.2, expression(paste(h^A*(h^B) == frac(alpha - beta*h^B, 1 + 2*beta) )))


dev.off()
