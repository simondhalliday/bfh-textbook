require(shape)
pdf(file = "coordination_failures/coord_brfs_cournot.pdf", width = 9, height = 7)

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

par(mar =  c(6, 9, 1, 1))

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


xlims <- c(0, 20)
ylims <- c(0, 20)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(112, 
       uA(hANE(alpha = 30, beta = 1/2), hANE(alpha = 30, beta = 1/2)), 
       155.8)
b <- c(112, 
       uA(hANE(alpha = 30, beta = 1/2),hANE(alpha = 30, beta = 1/2)), 
       155.8)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

# Keeping if necessary. 
# ticksy <- c(0, hApEff2(alpha = 30), hANE(alpha = 30), intercept1(alpha = 30), ylims[2])
# ylabels <- c(NA, expression(paste(h^B,"*")), expression(paste(h^{BN})), expression(paste(frac(alpha, 1 + 2*beta))), NA)
# ticksx <- c(0, hApEff2(alpha = 30), hANE(alpha = 30), intercept1(alpha = 30), xlims[2])
# xlabels <- c(NA, expression(paste(h^A,"*")), expression(paste(h^{AN})), expression(paste(frac(alpha, 1 + 2*beta))), NA)

ticksy <- c(0, hApEff2(alpha = 30), hANE(alpha = 30), intercept1(alpha = 30), ylims[2])
ylabels <- c(NA, expression(paste(h^B,"*", phantom()==10)), expression(paste(h^{BN} == 12)), expression(paste(15 )), NA)
ticksx <- c(0, hApEff2(alpha = 30), hANE(alpha = 30), intercept1(alpha = 30), xlims[2])
xlabels <- c(NA, expression(paste(h^A,"*", phantom()==10)), expression(paste(h^{AN} == 12)), NA, NA)


axis(1, at = ticksx,  pos = 0, labels = FALSE)
text(x = c(0, hApEff2(alpha = 30)-1, hANE(alpha = 30) + 0.5, intercept1(alpha = 30), xlims[2]), 
     par("usr")[3] - 0.3, labels = xlabels, srt = 0, pos = 1, 
     xpd = TRUE, cex = labelsize)
#15 in the labels above didn't work, so it must be indivudally located
text(15, -1.2, expression(paste(15)), xpd = TRUE, cex = labelsize) 

axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(9.4, 10.55, length.out = npts)

#I need something like xx1 with npts for 
xpoly1 <- seq(from = 8, to = 12, length.out = 500)
ypoly1 <- indiffA(xpoly1, uA = 144, alpha = 30, beta = 1/2)
ypoly2 <- indiffBroot1(xpoly1, uB = 144, alpha = 30, beta = 1/2)
polygon(x = c(xpoly1, rev(xpoly1)), y = c(ypoly1, rev(ypoly2)), col=COL[4], density=NULL, border = NA)

#Draw functions
lines(xx1, brfA(xx1, alpha = 30, beta = 1/2), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfB(xx1, alpha = 30, beta = 1/2), col = COLB[4], lwd = graphlinewidth)

xx2 <- seq(1, 14.5, length.out = npts)
xx3 <- seq(9.4, 10.55, length.out = npts)

lines(xx2, PEC(xx2, alpha = 30, beta = 1/2), col = COL[2], lty = 2, lwd = segmentlinewidth)
lines(xx3, PEC(xx3, alpha = 30, beta = 1/2), col = COL[2], lwd = graphlinewidth)


contour(y, x,
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLA[3],
        lwd = graphlinewidth,
        levels = a,
        xaxs="i",
        yaxs="i",
        add = TRUE)

text(0.5*xlims[2], -3, expression(paste("A's hours, ", h^A)), xpd = TRUE, cex = axislabelsize)
text(-4.5, 9, expression(paste("B's hours, ", h^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 


contour(x, y,
        outer(x, y, uB),
        drawlabels = FALSE,
        col = COLB[2],
        lwd = graphlinewidth,
        levels = b,
        add = TRUE
)


text(3, 12.4, expression("Pareto-efficient"), cex = annotatesize)
text(3, 11.4, expression("Curve"), cex = annotatesize)

#Arrows(6.5, 10.1, 9.3, 10.1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# 
#Label the iso-welfare functions for the HG, Aisha
text(4, 0.8, expression(u[z]^A), cex = annotatesize)
text(5.6, 0.8, expression(u[n]^A), cex = annotatesize)
text(7.4, 0.8, expression(u[3]^A), cex = annotatesize)


#Label the indifference curves for the HG, Betty
text(11.2, 19, expression(u[z]^B), cex = annotatesize)
text(7.8, 19, expression(u[n]^B), cex = annotatesize)
text(4.5, 19, expression(u[3]^B), cex = annotatesize)


#Label Nash Equilibrium
points(hANE(alpha = 30), hANE(alpha = 30), pch = 16, col = "black", cex = 1.5)
text(hANE(alpha = 30) + 0.4, hANE(alpha = 30) + 0.4, expression(paste("n")), cex = annotatesize)


#Annotate Pareto Efficient Curve and relevant points
#points(10.55, PEC(10.55), pch = 16, col = "black", cex = 1.5)
#text(10.55 + 0.4, PEC(10.55) - 0.4, expression(paste(t^A)), cex = annotatesize)
#points(9.4, PEC(9.4), pch = 16, col = "black", cex = 1.5)
#text(9.4 - 0.2, PEC(9.4) + 0.3, expression(paste(t^B)), cex = annotatesize)

points(hApEff2(alpha = 30), hApEff2(alpha = 30), pch = 16, col = "black", cex = 1.5)
text(hApEff2(alpha = 30)- 0.3, hApEff2(alpha = 30) - 0.3, expression(paste(i)), cex = annotatesize)

#B's brf
text(3, 15.7, expression(paste("B's best-response")), cex = labelsize)
text(3, 15.0, expression(paste("function")), cex = labelsize)

#A's brf
text(17.2, 3, expression(paste("A's best-response")), cex = labelsize, xpd = TRUE)
text(17.2, 2.3, expression(paste("function")), cex = labelsize)

dev.off()
