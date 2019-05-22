require(shape)
pdf(file = "coordination_failures/coord_brfs_cournot/coord_brfs_cournot_6.pdf", width = 9, height = 7)

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
a <- c(uA(hANE(alpha = 30, beta = 1/2), hANE(alpha = 30, beta = 1/2)), 
       155.8)
b <- c(uA(hANE(alpha = 30, beta = 1/2),hANE(alpha = 30, beta = 1/2)), 
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


ticksy <- c(0, hApEff2(alpha = 30), hANE(alpha = 30), intercept1(alpha = 30), ylims[2])
ylabels <- c(NA, expression(paste(h^B,"*")), expression(paste(h^{BN})), expression(paste(frac(alpha, 1 + 2*beta))), NA)
ticksx <- c(0, hApEff2(alpha = 30), hANE(alpha = 30), intercept1(alpha = 30), xlims[2])
xlabels <- c(NA, expression(paste(h^A,"*")), expression(paste(h^{AN})), expression(paste(frac(alpha, 1 + 2*beta))), NA)



axis(1, at = ticksx,  pos = 0, labels = FALSE)
text(x = c(0, hApEff2(alpha = 30), hANE(alpha = 30), intercept1(alpha = 30), xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE)

axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(9.4, 10.55, length.out = npts)

#I need something like xx1 with npts for 
xpoly1 <- seq(from = 8, to = 12, length.out = 500)
ypoly1 <- indiffA(xpoly1, uA = 144, alpha = 30, beta = 1/2)
ypoly2 <- indiffBroot1(xpoly1, uB = 144, alpha = 30, beta = 1/2)
polygon(x = c(xpoly1, rev(xpoly1)), y = c(ypoly1, rev(ypoly2)), col=COL[4], density=NULL, border = NA)



lines(xx1, brfA(xx1, alpha = 30, beta = 1/2), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfB(xx1, alpha = 30, beta = 1/2), col = COLB[4], lwd = graphlinewidth)
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

text(0.5*xlims[2], -1.9, expression(paste("A's hours, ", h^A)), xpd = TRUE, cex = axislabelsize)
text(-1.7, 9, expression(paste("B's hours, ", h^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 


contour(x, y,
        outer(x, y, uB),
        drawlabels = FALSE,
        col = COLB[2],
        lwd = graphlinewidth,
        levels = b,
        add = TRUE
)


text(4, 10.2, expression("Pareto-efficient"))
text(4, 9.6, expression("Curve"))
Arrows(5.7, 10.1, 9.3, 10.1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# 
#Label the iso-welfare functions for the HG, Aisha
text(5.3, 0.8, expression(u[1]^A == u[n]^A))
text(7.2, 0.8, expression(u[2]^A))


#Label the indifference curves for the HG, Betty
text(7.8, 19, expression(u[1]^B == u[n]^B))
text(4.9, 19, expression(u[2]^B))


#Label Nash Equilibrium
points(hANE(alpha = 30), hANE(alpha = 30), pch = 16, col = "black", cex = 1.5)

text(hANE(alpha = 30) + 0.3, hANE(alpha = 30) + 0.3, expression(paste("n")))


#Annotate Pareto Efficient Curve and relevant points
points(10.55, PEC(10.55), pch = 16, col = "black", cex = 1.5)
text(10.55 + 0.3, PEC(10.55) - 0.3, expression(paste(t^A)))


points(9.4, PEC(9.4), pch = 16, col = "black", cex = 1.5)
text(9.4 - 0.3, PEC(9.4) + 0.2, expression(paste(t^B)))

points(hApEff2(alpha = 30), hApEff2(alpha = 30), pch = 16, col = "black", cex = 1.5)
text(hApEff2(alpha = 30)- 0.3, hApEff2(alpha = 30) - 0.3, expression(paste(i)))

#B's brf
text(2, 15.7, expression(paste("B's best response")))
text(2, 15.2, expression(paste("function")))

#A's brf
text(16, 4, expression(paste("A's best response")))
text(16, 3.5, expression(paste("function")))

dev.off()
