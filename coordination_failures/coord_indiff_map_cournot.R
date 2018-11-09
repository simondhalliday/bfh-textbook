require(shape)
pdf(file = "coordination_failures/coord_indiff_map_cournot.pdf", width = 9, height = 7)

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


xlims <- c(0, 20)
ylims <- c(0, 20)

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
ticksy <- c(0, 8, 9.6, 12, ylims[2])
ylabels <- c(NA, expression(paste(h[Low]^B)), expression(paste(h^{BN})), expression(paste(h[High]^B)), NA)
ticksx <- c(0, hApEff2(alpha = 30), hANE(alpha = 30), intercept1(alpha = 30), xlims[2])
xlabels <- c(NA, expression(paste(h^A,"*")), expression(paste(h^{AN})), expression(paste(frac(alpha, 1 + 2*beta))), NA)

axis(1, at = ticksx,  pos = 0, labels = FALSE)
text(x = c(00, hApEff2(alpha = 30), hANE(alpha = 30), intercept1(alpha = 30), xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE)

axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

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


#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
# text(4, 10, expression("Pareto-efficient"))
# text(4, 9.4, expression("Curve"))
# Arrows(4, 10.2, 4, 13, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# 
# #Label the iso-welfare functions for the HG, Aisha
text(5.3, 0.8, expression(u[1]^A == u[z]^A))
text(7.2, 0.8, expression(u[2]^A))
# text(4.4, 0.8, expression(u[3]^A))
# text(5.2, 0.8, expression(u[4]^A))
# text(9.2, 0.8, expression(u[5]^A))

#Label Nash Equilibrium 
#segments(0, 9.6, 9.6, 9.6, lty = 2, col = "gray" , lwd = segmentlinewidth)
#segments(9.6, 0, 9.6, 9.6, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(hANE(alpha = 30), hANE(alpha = 30), pch = 16, col = "black", cex = 1.5)
#text(11.3, 10.1, expression(paste("Nash Equilibrium")))
text(hANE(alpha = 30) + 0.3, hANE(alpha = 30) + 0.3, expression(paste("n")))


#Annotate Pareto Efficient Curve and relevant points
# points(10.55, PEC(10.55), pch = 16, col = "black", cex = 1.5)
# text(10.55, PEC(10.55) - 0.5, expression(paste("g")))
# 
# 
# points(9.4, PEC(9.4), pch = 16, col = "black", cex = 1.5)
# text(9.4 - 0.5, PEC(9.4), expression(paste(f)))
# 
# points(hApEff2(alpha = 30), hApEff2(alpha = 30), pch = 16, col = "black", cex = 1.5)
# text(hApEff2(alpha = 30)- 0.3, hApEff2(alpha = 30) - 0.3, expression(paste(i)))

#points(5.84, 8.77, pch = 16, col = "black", cex = 1.5)

#B's brf
# text(2, 15.7, expression(paste("B's best response")))
# text(2, 15.2, expression(paste("function")))
#Arrows(2, 16.5, 2, 15.2, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#A's brf
# text(16, 4, expression(paste("A's best response")))
# text(16, 3.5, expression(paste("function")))
#Arrows(15.5, 3, 15.5, 1.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


# points(2.8, 11.2, pch = 16, col = "black", cex = 1.5)
# text(15.5, 4, expression(paste("A's best response")))
# 
# points(11.2, 2.8, pch = 16, col = "black", cex = 1.5)
# text(15.5, 4, expression(paste("A's best response")))

dev.off()
