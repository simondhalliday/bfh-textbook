require(shape)
pdf(file = "coordination_failures/coord_fallback_pareto_new.pdf", width = 9, height = 7)

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

par(mar =  c(4, 4, 1, 1))

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


xlims <- c(0, 24)
ylims <- c(0, 24)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(112, 
       uA(hANE(alpha = 30, beta = 1/2), hANE(alpha = 30, beta = 1/2)),
       #uA(hApEff2(alpha = 30, beta = 1/2), hApEff2(alpha = 30, beta = 1/2)),
       155.8)
b <- c(112, 
       uA(hANE(alpha = 30, beta = 1/2),hANE(alpha = 30, beta = 1/2)), 
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


# ticksy <- c(ylims[1], ylims[2])
# ylabels <- c(NA, NA)
# ticksx <- c(xlims[1], xlims[2])
# xlabels <- c(NA, NA)
ticksy <- seq(ylims[1], ylims[2], 3)
ylabels <- seq(ylims[1], ylims[2], 3)
ticksx <- seq(xlims[1], xlims[2], 3)
xlabels <- seq(xlims[1], xlims[2], 3)


axis(1, at = ticksx,  pos = 0, labels = xlabels)
#text(x = c(0, hApEff2(alpha = 30), hANE(alpha = 30), intercept1(alpha = 30), xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE)

axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(8.05, 11.55, length.out = npts)

#Polygon for the P-Improving Lens
#xpoly1 <- seq(from = 6.3, to = 13.65, length.out = 500)
xpoly1 <- seq(from = 5, to = 14.2, length.out = 500)
ypoly1 <- indiffA(xpoly1, uA = 112, alpha = 30, beta = 1/2)
ypoly2 <- indiffBroot1(xpoly1, uB = 112, alpha = 30, beta = 1/2)
polygon(x = c(xpoly1, rev(xpoly1)), y = c(ypoly1, rev(ypoly2)), col = COL[4], density = NULL, border = NA)

xpoly1 <- seq(from = 14.2, to = 17.65, length.out = 500)
ypoly1 <- indiffBroot2(xpoly1, uB = 112, alpha = 30, beta = 1/2)
ypoly2 <- indiffBroot1(xpoly1, uB = 112, alpha = 30, beta = 1/2)
polygon(x = c(xpoly1, rev(xpoly1)), y = c(ypoly1, rev(ypoly2)), col = COL[4], density = NULL, border = NA)

#xx2 <- seq(8.05, 11.55, length.out = npts)
xx2 <- seq(1, 14.5, length.out = npts)
xx3 <- seq(6.5, 12.4, length.out = npts)

#Pareto-efficient curve line
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

text(0.5*xlims[2], -1.9, expression(paste("A's hours, ", h^A)), xpd = TRUE, cex = axislabelsize)
text(-1.7, 0.5*ylims[2], expression(paste("B's hours, ", h^B)), xpd = TRUE, cex = axislabelsize, srt = 90)

contour(x, y,
        outer(x, y, uB),
        drawlabels = FALSE,
        col = COLB[2],
        lwd = graphlinewidth,
        levels = b,
        add = TRUE
)

text(3, 12.4, expression("Pareto-efficient"), cex = annotatesize)
text(3, 11.4, expression("curve"), cex = annotatesize)
#Arrows(5.7, 10, 9.6, 10, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

# text(11.1, 3.5, expression("Pareto-improving"), cex = annotatesize)
# text(11.1, 3, expression("lens"), cex = annotatesize)
# Arrows(11.1, 3.8, 11.1, 10.8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the indifference curves for A
# text(3.5, 0.8, expression(u[z]^A), cex = labelsize -.05)
# text(5.7, 0.8, expression(u[2]^A), cex = labelsize - 0.05)
# text(7.3, 0.8, expression(u[3]^A), cex = labelsize - 0.05)

text(21, 8.6, expression(u[z]^A), cex = labelsize -.05)
text(21, 5.2, expression(u[1]^A), cex = labelsize - 0.05)
text(21, 2, expression(u[3]^A), cex = labelsize - 0.05)

text(21, 9.8, expression(paste("A's PC")), xpd = TRUE, cex = annotatesize)

#Label the indifference curves for B

text(11.2, 19, expression(u[z]^B), cex = labelsize - 0.05)
text(7.7, 19, expression(u[1]^B == 144), cex = labelsize - 0.05)
text(4.2, 19, expression(u[3]^B), cex = labelsize - 0.05)

text(11.2, 20.2, expression(paste("B's PC")), xpd = TRUE, cex = annotatesize)

dev.off()
