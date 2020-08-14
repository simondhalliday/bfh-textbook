require(shape)
library(plotrix)
pdf(file = "coordination_failures/coord_brfs_stackelberg_zoom_close.pdf", width = 9, height = 7)

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

par(mar =  c(5, 7, 2, 2))

uA <- function(ea, eb, alpha = 30, beta = 1/2) {
  (alpha - beta*(ea+eb))*ea - 0.5*(ea)^2
}

uB <- function(ea, eb, alpha = 30, beta = 1/2) {
  (alpha - beta*(ea+eb))*eb - 0.5*(eb)^2
}

yA <- function(ea, eb, alpha = 30, beta = 1/2){
  ea*(alpha - beta*(ea + eb))
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

hApEff <- function(alpha, beta = 1/2){
  alpha/(2 + 4*beta)
}

hAST <- function(alpha, beta = 1/2){
  (alpha*(1 + beta))/(1 + 4*beta + 2*beta^2)
}

hApEff2 <- function(alpha, beta = 1/2){
  alpha/(1 + 4*beta)
}

intercept1 <- function(alpha, beta = 1/2){
  alpha/(1 + 2*beta)
}

PEC <- function(ea, alpha = 30, beta = 1/2){
  a = beta*(1+2*beta);
  b = -alpha*(1+3*beta)+(1+2*beta)^2*ea;
  c = alpha^2 - alpha*(1+3*beta)*ea+ beta*(1+2*beta)*ea^2
  delta = b^2-4*a*c
  return((-b-sqrt(delta))/(2*a))
}


indiffA <- function(ea, alpha = 30, beta = 1/2, uA = 144) {
  (alpha*ea - beta*(ea)^2 - 0.5*(ea)^2 - uA)/(beta*ea)
}

indiffBroot1 <- function(ea, alpha = 30, beta = 30, uB = 144){
  (-sqrt(alpha^2 - 2*alpha*beta*ea + beta^2*ea^2 - 4*beta*uB - 2*uB) + alpha - beta*ea)/(2*beta + 1)
}

indiffBroot2 <- function(ea, alpha = 30, beta = 30, uB = 144){
  (sqrt(alpha^2 - 2*alpha*beta*ea + beta^2*ea^2 - 4*beta*uB - 2*uB) + alpha - beta*ea)/(2*beta + 1)
}


indiffB <- function(eb, alpha = 30, beta = 1/2, uB = 144) {
  (alpha*eb - uB - 0.5*eb^2)/(alpha*beta*eb)
}

indiffB2 <- function(ea, alpha = 30, beta = 1/2, uB = 144) {
  sqrt(alpha^2*(beta*ea - 1)^2 - 2*uB) - alpha*beta*ea + alpha
}

indiffB3 <- function(ea, alpha = 30, beta = 1/2, uB = 144) {
  -sqrt(alpha^2*(beta*ea - 1)^2 - 2*uB) - alpha*beta*ea + alpha
}


xlims <- c(9.15, 13.25)
ylims <- c(9.15, 13.25)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(uA(hANE(alpha = 30, beta = 1/2), hANE(alpha = 30, beta = 1/2)), 
       uA(hAST(alpha = 30, beta = 1/2), brfB(ea = hAST(alpha = 30, beta = 1/2))),
       155.65)
b <- c(uA(hANE(alpha = 30, beta = 1/2),hANE(alpha = 30, beta = 1/2)), 
       uA(brfB(ea = hAST(alpha = 30, beta = 1/2)), hAST(alpha = 30, beta = 1/2)),
       155.65)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")


ticksy <- c(ylims[1], 9.4, brfB(ea = hAST(alpha = 30, beta = 1/2)), hANE(alpha = 30), intercept1(alpha = 30), ylims[2])
ylabels <- c(NA, expression(paste(h[t^A]^B == 9.4)), expression(paste(h^{BF}==11.8)), expression(paste(h^{BN})==12), expression(paste(frac(alpha, 1 + 2*beta))), NA)
ticksx <- c(xlims[1], 10.55, hANE(alpha = 30), hAST(alpha = 30, beta = 1/2), intercept1(alpha = 30), xlims[2])
xlabels <- c(NA, expression(paste(h[t^A]^A == 10.55)), expression(paste(h^{AN}==12)), expression(paste(h^{AF}==12.9)), expression(paste(frac(alpha, 1 + 2*beta))), NA)


axis(1, at = ticksx,  pos = xlims[1], labels = FALSE, cex.axis = labelsize)
text(x = c(0, 10.55, hANE(alpha = 30), hAST(alpha = 30, beta = 1/2), intercept1(alpha = 30), xlims[2]), par("usr")[3] - 0.1, labels = xlabels, srt = 0, pos = 1, xpd = TRUE, cex = labelsize)

axis(2, at = ticksy, pos = ylims[1], labels = ylabels, las = 1, cex.axis = labelsize)

axis.break(axis=1,breakpos=NULL,pos=xlims[1],bgcol="white",breakcol="black",
           style="slash",brw=0.02)
axis.break(axis=2,breakpos=NULL,pos=ylims[1],bgcol="white",breakcol="black",
           style="slash",brw=0.02)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

xx2 <- seq(9.4, 10.55, length.out = npts)
xx3 <- seq(8.4, 11.55, length.out = npts)

#I need something like xx1 with npts for 
xpoly1 <- seq(from = 8, to = 12, length.out = 500)
ypoly1 <- indiffA(xpoly1, uA = 144, alpha = 30, beta = 1/2)
ypoly2 <- indiffBroot1(xpoly1, uB = 144, alpha = 30, beta = 1/2)
polygon(x = c(xpoly1, rev(xpoly1)), y = c(ypoly1, rev(ypoly2)), col=COL[4], density=NULL, border = NA)



lines(xx1, brfA(xx1, alpha = 30, beta = 1/2), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfB(xx1, alpha = 30, beta = 1/2), col = COLB[4], lwd = graphlinewidth)
lines(xx3, PEC(xx3, alpha = 30, beta = 1/2), col = COL[2], lwd = graphlinewidth, lty = 2)
lines(xx2, PEC(xx2, alpha = 30, beta = 1/2), col = COL[2], lwd = graphlinewidth)

contour(y, x, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLA[3],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE) 

text( 0.5*(9.25 + 13), ylims[1] - 0.5, expression(paste("A's hours, ", h^A)), xpd = TRUE, cex = axislabelsize)
text(xlims[1] - 0.5, 0.5*(9.25 + 13) - 0.1, expression(paste("B's hours, ", h^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 


contour(x, y,
        outer(x, y, uB),
        drawlabels = FALSE,
        col = COLB[2],
        lwd = graphlinewidth,
        levels = b,
        add = TRUE
)


#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)

text(10.3, 10.3, expression("Pareto-efficient"), cex = labelsize)
text(10.3,  10.1, expression("curve"), cex = labelsize)

# text(11.9, 8.8, expression("Pareto-improving"), cex = labelsize)
# text(11.9,  8.5, expression("lens"), cex = labelsize)
# Arrows(12, 9.1, 11.5, 10.8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#Label the iso-welfare functions for A (leader)
text(13.2, 12, expression(u[n]^A), cex = labelsize, xpd = TRUE)
text(13.2, 11.5, expression(u[f]^A), cex = labelsize, xpd = TRUE)
text(13.2, 9.85, expression(u[3]^A), cex = labelsize, xpd = TRUE)

#Label the indifference curves for B (follower)
text(12.75, 13, expression(u[f]^B), cex = labelsize)
text(11.95, 13, expression(u[n]^B), cex = labelsize)
text(10.15, 13, expression(u[2]^B), cex = labelsize)


#Label Nash Equilibrium 
segments(0, 12, 12, 12, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(12, 0, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)

points(hANE(alpha = 30), hANE(alpha = 30), pch = 16, col = "black", cex = 1.5)
#text(11.3, 10.1, expression(paste("Nash Equilibrium")))
text(hANE(alpha = 29.4) + 0.1, hANE(alpha = 30.1) + 0.1, expression(paste(n)), cex = labelsize - 0.05)

segments(0, brfB(ea = hAST(alpha = 30, beta = 1/2)), hAST(alpha = 30, beta = 1/2), brfB(ea = hAST(alpha = 30, beta = 1/2)), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(hAST(alpha = 30, beta = 1/2), 0, hAST(alpha = 30, beta = 1/2), brfB(ea = hAST(alpha = 30, beta = 1/2)), lty = 2, col = "gray" , lwd = segmentlinewidth)
points(hAST(alpha = 30, beta = 1/2), brfB(ea = hAST(alpha = 30, beta = 1/2)), pch = 16, col = "black", cex = 1.5)
text(hAST(alpha = 30, beta = 1/2) - 0.1, brfB(ea = hAST(alpha = 30, beta = 1/2)) - 0.15, expression(paste(f)), cex = labelsize)

#Annotate Pareto Efficient Curve and relevant points

segments(0, 9.4, 10.55, 9.4, lty = 2, col = "gray", lwd = segmentlinewidth)
segments(10.55, 0, 10.55, 9.4, lty = 2, col = "gray" , lwd = segmentlinewidth)

points(10.55, PEC(10.55), pch = 16, col = "black", cex = 1.5)
text(10.55, PEC(10.55) + 0.2, expression(paste(t^A)), cex = labelsize)

points(9.4, PEC(9.4), pch = 16, col = "black", cex = 1.5)
text(9.4, PEC(9.4) + 0.2, expression(paste(t^B)), cex = labelsize)


# points(hApEff2(alpha = 30), hApEff2(alpha = 30), pch = 16, col = "black", cex = 1.5)
# text(hApEff2(alpha = 30)- 0.05, hApEff2(alpha = 30) - 0.05, expression(paste(i)), cex = labelsize)


#B's brf
text(9.6, 12.75, expression(paste("B's BRF")), cex = labelsize)
#text(8.4, 12.2, expression(paste("function")), cex = labelsize)

#A's brf
text(12.33, 9.5, expression(paste("A's BRF")), xpd = TRUE, cex = labelsize)
#text(12.1, 7.7, expression(paste("function")), xpd = TRUE, cex = labelsize)



dev.off()
