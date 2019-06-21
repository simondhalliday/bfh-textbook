require(shape)
require(plotrix)
pdf(file = "more/coord_brfs_cournot_jee_article.pdf", width = 9, height = 7)

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


xlims <- c(7, 14)
ylims <- c(7, 14)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(uA(hANE(alpha = 30, beta = 1/2), hANE(alpha = 30, beta = 1/2)), 
       #uA(hApEff2(alpha = 30, beta = 1/2), hApEff2(alpha = 30, beta = 1/2)),
       155.8)
b <- c(uA(hANE(alpha = 30, beta = 1/2),hANE(alpha = 30, beta = 1/2)), 
       #uB(hApEff2(alpha = 30, beta = 1/2), hApEff2(alpha = 30, beta = 1/2)), 
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

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
# ticksx <- seq(from = 0, to = xlims[2], by = 2)
# xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(ylims[1], hApEff2(alpha = 30), hANE(alpha = 30), intercept1(alpha = 30), ylims[2])
ylabels <- c(NA, expression(paste(h[B],"*")), expression(paste(h[B]^{N})), expression(paste(frac(bar(y), 1 + 2*beta))), NA)
ticksx <- c(xlims[1], hApEff2(alpha = 30), hANE(alpha = 30), intercept1(alpha = 30), xlims[2])
xlabels <- c(NA, expression(paste(h[A],"*")), expression(paste(h[A]^{N})), expression(paste(frac(bar(y), 1 + 2*beta))), NA)

axis(1, at = ticksx,  pos = xlims[1], labels = FALSE)
text(x = c(0, hApEff2(alpha = 30), hANE(alpha = 30), intercept1(alpha = 30), xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE)

axis(2, at = ticksy, pos = ylims[1], labels = ylabels, las = 1)

axis.break(axis=1, breakpos=NULL, pos=NA, bgcol="white",breakcol="black",
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
#lines(xx1, PECroot2(xx1, alpha = 30, beta = 1/2), col = COLB[2], lwd = graphlinewidth)
#lines(xx1, indiffB(xx1, alpha = 30, beta = 1/2, uB = 42), col = COL[2], lwd = graphlinewidth)
#lines(xx1, indiffB2(xx1, alpha = 30, beta = 1/2, uB = 46.08), col = COL[2], lwd = graphlinewidth)
#lines(xx1, indiffB3(xx1, alpha = 30, beta = 1/2, uB = 46.08), col = COL[2], lwd = graphlinewidth)
#lines(xx1, indiffAlow(xx1, uA = 42, alpha = 30, beta = 1/2), col = COL[2], lwd = graphlinewidth)
#lines(xx1, indiffAgain(xx1, uA = 42, alpha = 30, beta = 1/2), col = COL[2], lwd = graphlinewidth)


#persp(x, y, outer(x, y, u), ticktype="detailed") 
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

#Add arrows:
#arrows(-0.8, 10, -0.8, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
#arrows(6.2, -1.7, 9, -1.7, xpd = TRUE, length=0.1,angle=40,lwd=3)


contour(x, y, 
        outer(x, y, uB),
        #labels = c("v1", "v2", "v3"),
        drawlabels = FALSE,
        col = COLB[2],
        #xlab = expression(paste("")),
        #ylab = expression(paste("")),
        lwd = graphlinewidth,
        levels = b, 
        add = TRUE
        #xaxs="i", 
        #yaxs="i"
) 

#segments(0, 12, 12, 12, lty = 1, col = COL[2] , lwd = graphlinewidth)

#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
text(10.1, 10.5, expression("Contract"))
text(10.1,  10.3, expression("curve"))

text(11.1, 8.5, expression("Pareto-improving"))
text(11.1,  8.3, expression("lens"))
Arrows(11.1, 8.6, 11.1, 10.8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#Label the iso-welfare functions for the HG, Aisha
text(8, 7.2, expression(u[1]^A == u[n]^A))
text(9.1, 7.2, expression(u[2]^A))

#Label the indifference curves for the HG, Betty
text(7.4, 8.05, expression(u[1]^B == u[n]^B))
text(7.4, 9.205, expression(u[2]^B))


#Label Nash Equilibrium 
#segments(0, 9.6, 9.6, 9.6, lty = 2, col = "gray" , lwd = segmentlinewidth)
#segments(9.6, 0, 9.6, 9.6, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(hANE(alpha = 30), hANE(alpha = 30), pch = 16, col = "black", cex = 1.5)
#text(11.3, 10.1, expression(paste("Nash Equilibrium")))
text(hANE(alpha = 30) + 0.1, hANE(alpha = 30) + 0.1, expression(paste(n)))


#Annotate Pareto Efficient Curve and relevant points
points(10.55, PEC(10.55), pch = 16, col = "black", cex = 1.5)
text(10.55 + 0.1, PEC(10.55) - 0.1, expression(paste(t[A])))

points(9.4, PEC(9.4), pch = 16, col = "black", cex = 1.5)
text(9.4 - 0.1, PEC(9.4) + 0.1, expression(paste(t[B])))

points(hApEff2(alpha = 30), hApEff2(alpha = 30), pch = 16, col = "black", cex = 1.5)
text(hApEff2(alpha = 30)- 0.1, hApEff2(alpha = 30) - 0.1, expression(paste(i)))

#points(5.84, 8.77, pch = 16, col = "black", cex = 1.5)

#B's brf
text(8, 13.4, expression(paste("B's best-response")))
text(8, 13.2, expression(paste("function")))
#Arrows(2, 16.5, 2, 15.2, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#A's brf
text(13.6, 8.3, expression(paste("A's best-response")), xpd = TRUE)
text(13.6, 8.1, expression(paste("function")), xpd = TRUE)
#Arrows(15.5, 3, 15.5, 1.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


# points(2.8, 11.2, pch = 16, col = "black", cex = 1.5)
# text(15.5, 4, expression(paste("A's best response")))
# 
# points(11.2, 2.8, pch = 16, col = "black", cex = 1.5)
# text(15.5, 4, expression(paste("A's best response")))

dev.off()
