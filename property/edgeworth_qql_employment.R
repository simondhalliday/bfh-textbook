require(shape)
require(pBrackets)
# pdf(file = "edgeworthbox_qql_employmentSTEP1.pdf", width = 9, height = 7)
# pdf(file = "edgeworthbox_qql_employmentSTEP2.pdf", width = 9, height = 7)
# pdf(file = "edgeworthbox_qql_employmentSTEP3.pdf", width = 9, height = 7)
# pdf(file = "edgeworthbox_qql_employmentSTEP4.pdf", width = 9, height = 7)

pdf(file = "property/edgeworthbox_qql_employment.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.8
annotatesize <- 1.5
labelsize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5
namesize <- 1.8

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")
grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")

par(mar =  c(4, 4, 4, 4))

uB <- function(x, y, xbar = 16, ybar= 400, rmax = 32, xmax = 16) {
  (ybar - y) + rmax*(xbar - x) - (1/2)*(rmax/xmax)*(xbar - x)^2
}

#slope for log function = 9*(28 - (28/16)*8) = (17 - x)(rmax - (rmax/xmax)*x)



uA <- function(x, y, ybar = 400, xbar = 16) {
  (y) + 144*log(1 + x)
}

indiffA <- function(x, utility = 256, slope = 144) {
  utility -  slope*log(1 + x)
}

# uB <- function(x, y, rmax = 50, xmax = 16, xbar = 16, ybar = 400) {
#   (ybar - y) + rmax*(xbar - x) - (1/2)*(rmax/xmax)*(xbar - x)^2
# }

indiffB <- function(x, utility = 256, rmax = 32, xmax = 16, xbar = 16, ybar = 400) {
  ybar - utility + rmax*(xbar - x) - (1/2)*(rmax/xmax)*(xbar - x)^2
}

WalrasP <- function(x, intercept = 9) {
  intercept - x
}

xlims <- c(0, 16)
ylims <- c(0, 400)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

a <- c(uA(0, 400), uA(8,200), uA(0,400) + 254)

#B's value when at A's bliss point
#0.35*log(5.88) + 0.35*log(8.88) + 0.5*log(10 - 5.88) + 0.5*log(15 - 8.88) 

#B's bliss point x = 4.11765; y = 6.17647
#A's value when at A's bliss point
#0.5*log(4.11765) + 0.5*log(6.17647) + 0.35*log(10 - 4.11765) + 0.35*log(15 - 6.17647) 


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

ticksy <- seq(from = 0, to = ylims[2], by = 100)
ylabels <- seq(from = 0, to = ylims[2], by = 100)
ticksx <- seq(from = 0, to = 16, by = 2)
xlabels <- seq(from = 0, to = 16, by = 2)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0, cex.axis = labelsize)

#Pareto-improving lens
xpoly1 <- seq(from = 0, to = 16, length.out = 500)
ypoly1 <- indiffA(xpoly1, utility = 400)
ypoly2 <- indiffB(xpoly1, utility = 256)
polygon(x = c(xpoly1, rev(xpoly1)), y = c(ypoly1, rev(ypoly2)), col = COL[4], density = NULL, border = NA)



segments(8, 84, 8, 84+252, lty = 1, col = CBCols[4] , lwd = graphlinewidth)
segments(8, 0, 8, 84, col = CBCols[4] , lwd = segmentlinewidth, lty = 2)
segments(8, 84+252, 8, 400, col = CBCols[4] , lwd = segmentlinewidth, lty = 2)
text(10, 247.7, expression("Pareto-efficient"), cex = annotatesize)
text(10, 230, expression("curve"), cex = annotatesize)


# xx2 <- seq(xlims[1], xlims[2], length.out = npts)
# lines(xx2, indiffA(xx2, utility = 400), col = "purple", lwd = segmentlinewidth)
# lines(xx2, indiffB(xx2, utility = 256), col = "purple", lwd = segmentlinewidth)


contour(x, y,
        outer(x, y, uA),
        drawlabels = FALSE,
        col = CBCols[1],
        lwd = graphlinewidth,
        levels = a,
        xaxs="i",
        yaxs="i",
        add = TRUE)

#mtext(expression(paste("A's Hours hired of B's Work, ", x^A)), side=1, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], -40,  expression(paste("A's hours hired of B's Work, ", x^A)), xpd = TRUE, cex = axislabelsize) 
text(-1.3, 0.5*ylims[2], expression(paste("A's money, ", y^A)), xpd = TRUE, cex = axislabelsize, srt = 90) 

xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Add arrows:
Arrows(-1.2, 270, -1.2, 380, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5, xpd = TRUE)
Arrows(12.5, -42, 15, -42,  col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5, xpd = TRUE)
#arrows(-1.2, 270, -1.2, 380, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)
#arrows(12.5, -42, 15, -42, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)

xx2 <- seq(2, 10, length.out = npts)

#Label the indifference curves for A
text(11.3, 55, expression(paste(u[z]^A) == phantom()), cex = annotatesize)
text(12.3, 55, paste(uA(0, 400)), cex = annotatesize)
text(11.8, 80, expression(paste("A's PC")), cex = annotatesize)
text(11.3, 175, expression(paste(u[2]^A) == phantom()), cex = annotatesize)
text(12.3, 175, paste(516), cex = annotatesize)

text(11.3, 310, expression(paste(u[3]^A) == phantom()), cex = annotatesize)
text(12.3, 310, paste(652), cex = annotatesize)


#text(14, 280, expression(u[2]^A), cex = annotatesize)

#Label the indifference curves for B
#text(2, 380, expression(u[z]^B), cex = annotatesize)

#text(2, 125, expression(u[2]^B), cex = annotatesize)


text(-0.5, -40, expression("Ayanda"), xpd = TRUE, cex = namesize, col = CBCols[1])
text(16.4, 440, expression("Biko"), xpd = TRUE, cex = namesize, col = CBCols[2])


par(new = TRUE)

#Use the same x and ylims as previously, but with locations switched
xlims2 <- c(16, 0)
ylims2 <- c(400, 0)

#Leave the ylab and xlab blank to ensure no axes titles
plot(0, 0, xlim = xlims2, ylim = ylims2, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")),
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = 1.3, 
     bty = "n",
     xaxs="i", 
     yaxs="i")


uB2 <- function(x, y, rmax = 32, xmax = 16) {
  (y) + rmax*(x) - (1/2)*(rmax/xmax)*(x)^2
}
b <- c(uB2(16, 0),  uB2(8, 400-200) , uB2(16,0) + 254)

contour(x, y, 
        outer(x, y, uB2),
        drawlabels = FALSE,
        col = CBCols[2],
        lwd = graphlinewidth,
        levels = b, 
        add = TRUE
) 

#Set up axes at sides 3 and 4 (top and right)
axis(side = 3, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(side = 4, at = ticksy, pos = 0, labels = ylabels, las = 0, cex.axis = labelsize)
#mtext(expression(paste("B's Hours of Living,", x^B)), side=3, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], -45, expression(paste("B's hours of Living,", x^B)), xpd = TRUE, cex = axislabelsize) 
text(-1.2, 0.5*ylims[2], expression(paste("B's money, ", y^B)), xpd = TRUE, cex = axislabelsize, srt = 270) 

#Add arrows:
Arrows(-1.2, 270, -1.2, 380, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5, xpd = TRUE)
Arrows(11.2, -45, 15, -45, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5, xpd = TRUE)
#arrows(-1.2, 270, -1.2, 380, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)
#arrows(11.2, -45, 15, -45, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)

points(x = 16, y = 0, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(15.7, 10, expression(paste(z)), cex = annotatesize)

points(8, 200, pch = 16, col = "black", cex = 1.5)
text(8.3, 205, expression(paste(j)), cex = annotatesize)


points(8, 318, pch = 16, col = "black", cex = 1.5)
text(7.7, 305, expression(paste(t^{B})), cex = annotatesize)

points(8, 64, pch = 16, col = "black", cex = 1.5)
text(7.7, 50, expression(paste(t^{A})), cex = annotatesize)

text(16 - 3.3, 400 - 365, expression(paste(u[z]^B) == phantom()), cex = annotatesize)
text(16 - 4.3, 400 - 365, paste(uB2(16, 0)), cex = annotatesize)
text(16 - 4, 400 - 340, expression(paste("B's PC")), cex = annotatesize)
text(16 - 3.3, 400 - 230, expression(paste(u[2]^B) == phantom()), cex = annotatesize)
text(16 - 4.3, 400 - 230, paste(uB2(8,200)), cex = annotatesize)
text(16 - 3.3, 400 - 110, expression(paste(u[3]^B) == phantom()), cex = annotatesize)
text(16 - 4.3, 400 - 110, paste(uB2(8,316)), cex = annotatesize)


#Customize ticks and labels for the plot
#ticksy <- seq(from = 0, to = 15, by = 1)
#ylabels <- seq(from = 0, to = 15, by = 1)
#ticksx <- seq(from = 0, to = 10, by = 1)
#xlabels <- seq(from = 0, to = 10, by = 1)
#axis(1, at = ticksx, pos = 0, labels = xlabels)
#axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0)

dev.off()

