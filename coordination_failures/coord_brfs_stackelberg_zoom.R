require(shape)
library(plotrix)
pdf(file = "coordination_failures/coord_brfs_stackelberg_zoom/coord_brfs_stackelberg_zoom.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(4, 6, 1.5, 1.5))

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


xlims <- c(7.5, 13.5)
ylims <- c(7.5, 13.5)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(uA(hANE(alpha = 30, beta = 1/2), hANE(alpha = 30, beta = 1/2)), 
       uA(hAST(alpha = 30, beta = 1/2), brfB(ea = hAST(alpha = 30, beta = 1/2))),
       155.8)
b <- c(uA(hANE(alpha = 30, beta = 1/2),hANE(alpha = 30, beta = 1/2)), 
       uA(brfB(ea = hAST(alpha = 30, beta = 1/2)), hAST(alpha = 30, beta = 1/2)),
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


ticksy <- c(ylims[1], hApEff2(alpha = 30),brfB(ea = hAST(alpha = 30, beta = 1/2)), hANE(alpha = 30), intercept1(alpha = 30), ylims[2])
ylabels <- c(NA, expression(paste(h^B,"*")), expression(paste(h^{BF})), expression(paste(h^{BN})), expression(paste(frac(alpha, 1 + 2*beta))), NA)
ticksx <- c(xlims[1], hApEff2(alpha = 30), hANE(alpha = 30), hAST(alpha = 30, beta = 1/2), intercept1(alpha = 30), xlims[2])
xlabels <- c(NA, expression(paste(h^A,"*")), expression(paste(h^{AN})), expression(paste(h^{AF})), expression(paste(frac(alpha, 1 + 2*beta))), NA)


axis(1, at = ticksx,  pos = xlims[1], labels = FALSE)
text(x = c(0, hApEff2(alpha = 30), hANE(alpha = 30), hAST(alpha = 30, beta = 1/2), intercept1(alpha = 30), xlims[2]), par("usr")[3] - 0.1, labels = xlabels, srt = 0, pos = 1, xpd = TRUE)

axis(2, at = ticksy, pos = ylims[1], labels = ylabels, las = 1)

axis.break(axis=1,breakpos=NULL,pos=NA,bgcol="white",breakcol="black",
           style="slash",brw=0.01)
axis.break(axis=2,breakpos=NULL,pos=NA,bgcol="white",breakcol="black",
           style="slash",brw=0.01)

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

text(10.5, ylims[1] - 0.6, expression(paste("A's hours, ", h^A)), xpd = TRUE, cex = axislabelsize)
text(xlims[1] - 0.6, 10.5, expression(paste("B's hours, ", h^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 


contour(x, y,
        outer(x, y, uB),
        drawlabels = FALSE,
        col = COLB[2],
        lwd = graphlinewidth,
        levels = b,
        add = TRUE
)


#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)

text(10.1, 10.5, expression("Pareto-efficient"))
text(10.1,  10.3, expression("curve"))

text(11.1, 8.5, expression("Pareto-improving"))
text(11.1,  8.3, expression("lens"))
Arrows(11.1, 8.6, 11.1, 10.8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#Label the iso-welfare functions for A (leader)
text(13.3, 11.95, expression(u[1]^A == u[n]^A))
text(13.3, 11.45, expression(u[2]^A == u[f]^A))
text(13.3, 9.8, expression(u[3]^A))

#Label the indifference curves for B (follower)
text(12.75, 13.3, expression(u[1]^B == u[f]^B))
text(12, 13.3, expression(u[2]^B == u[n]^B))
text(10.2, 13.3, expression(u[2]^B))


#Label Nash Equilibrium 
points(hANE(alpha = 30), hANE(alpha = 30), pch = 16, col = "black", cex = 1.5)
#text(11.3, 10.1, expression(paste("Nash Equilibrium")))
text(hANE(alpha = 30) + 0.1, hANE(alpha = 30) + 0.1, expression(paste(n)))

points(hAST(alpha = 30, beta = 1/2), brfB(ea = hAST(alpha = 30, beta = 1/2)), pch = 16, col = "black", cex = 1.5)
text(hAST(alpha = 30, beta = 1/2) - 0.1, brfB(ea = hAST(alpha = 30, beta = 1/2)) - 0.1, expression(paste(f)))

#Annotate Pareto Efficient Curve and relevant points
points(10.55, PEC(10.55), pch = 16, col = "black", cex = 1.5)
text(10.55, PEC(10.55) - 0.2, expression(paste(t^A)))

points(9.4, PEC(9.4), pch = 16, col = "black", cex = 1.5)
text(9.4 - 0.1, PEC(9.4) + 0.05, expression(paste(t^B)))

points(hApEff2(alpha = 30), hApEff2(alpha = 30), pch = 16, col = "black", cex = 1.5)
text(hApEff2(alpha = 30)- 0.1, hApEff2(alpha = 30) - 0.1, expression(paste(i)))


#B's brf
text(8.3, 12.7, expression(paste("B's best response")))
text(8.3, 12.5, expression(paste("function")))

#A's brf
text(12.4, 8, expression(paste("A's best response")), xpd = TRUE)
text(12.4, 7.8, expression(paste("function")), xpd = TRUE)



dev.off()
