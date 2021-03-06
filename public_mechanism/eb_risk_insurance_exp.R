require(shape)
require(pBrackets)
# pdf(file = "edgeworthbox_qql_employmentSTEP1.pdf", width = 9, height = 7)
# pdf(file = "edgeworthbox_qql_employmentSTEP2.pdf", width = 9, height = 7)
# pdf(file = "edgeworthbox_qql_employmentSTEP3.pdf", width = 9, height = 7)
# pdf(file = "edgeworthbox_qql_employmentSTEP4.pdf", width = 9, height = 7)

pdf(file = "risk/eb_risk_insurance_exp.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5
namesize <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(4, 4, 4, 4))

#u^N = x^N - (\delta * slopeA)^2

uA <- function(x, y, slopeA = 0.75, c = -3) {
  y - (1 - x*slopeA)^c
}

#u^W = X^W - ((1-\delta)*4)^2

uB <- function(x, y, slopeB = 8) {
  y - (x*slopeB)^2
}

# uB <- function(x, y, rmax = 50, xmax = 16, xbar = 16, ybar = 400) {
#   (ybar - y) + rmax*(xbar - x) - (1/2)*(rmax/xmax)*(xbar - x)^2
# }

xlims <- c(0, 1)
ylims <- c(0, 200)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

a <- c(uA(1,75), uA(0.7,50))

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

ticksy <- seq(from = 0, to = ylims[2], by = 50)
ylabels <- seq(from = 0, to = ylims[2], by = 50)
ticksx <- seq(from = 0, to = 16, by = 1)
xlabels <- seq(from = 0, to = 16, by = 1)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0)

#Pareto-improving lens
# xpoly1 <- seq(from = 0, to = 16, length.out = 500)
# ypoly1 <- indiffA(xpoly1, utility = 400)
# ypoly2 <- indiffB(xpoly1, utility = 256)
# polygon(x = c(xpoly1, rev(xpoly1)), y = c(ypoly1, rev(ypoly2)), col = COL[4], density = NULL, border = NA)



segments(8, 84, 8, 84+252, lty = 1, col = COL[2] , lwd = graphlinewidth)
segments(8, 0, 8, 84, col = COL[2] , lwd = segmentlinewidth, lty = 2)
segments(8, 84+252, 8, 400, col = COL[2] , lwd = segmentlinewidth, lty = 2)
text(9.3, 207.7, expression("Pareto-efficient"))
text(9.3, 192.5, expression("Curve"))


# xx2 <- seq(xlims[1], xlims[2], length.out = npts)
# lines(xx2, indiffA(xx2, utility = 400), col = "purple", lwd = segmentlinewidth)
# lines(xx2, indiffB(xx2, utility = 256), col = "purple", lwd = segmentlinewidth)


contour(x, y,
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLA[3],
        lwd = graphlinewidth,
        levels = a,
        xaxs="i",
        yaxs="i",
        add = TRUE)

#mtext("A's Hours hired of B's Work, x", side=1, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], -15, expression(paste("N's Risk Exposure, ", Delta^N)), xpd = TRUE, cex = axislabelsize) 
text(-0.08, 0.5*ylims[2], expression(paste("N's Expected wealth, ", omega^N)), xpd = TRUE, cex = axislabelsize, srt = 90) 

xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Add arrows:
arrows(-1.2, 260, -1.2, 380, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)
arrows(11.8, -45, 15, -45, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)

xx2 <- seq(2, 10, length.out = npts)
# lines(xx2, WalrasP(xx2, intercept = 11), col = "purple", lwd = segmentlinewidth)
# lines(xx2, WalrasP(xx2, intercept = 9.4), col = "purple", lwd = segmentlinewidth, lty = 1)




#segments(0, 4.4, 10, 4.4, col = COL[2] , lwd = segmentlinewidth, lty = 2)


#Label the PEC
# text(8, 5, expression("Pareto Efficient Curve"))
# Arrows(6.8, 5, 5.2, 5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the walrasian P
# 
# Arrows(2.25, 5.2, 2.25, 6.85, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# text(2.25, 5, expression(slope == -p[n] ))

#Label the indifference curves for A
text(14, 20, expression(u[1]^A))
text(14, 275, expression(u[2]^A))

#Label the indifference curves for B
text(2, 380, expression(u[1]^B))
text(2, 130, expression(u[2]^B))
#text(2.6, 8.1, expression(v[3]^B))
#text(3.4, 6.9, expression(v[4]^B))

#Label point i. 
# points(5, 3.95, pch = 16, col = "black", cex = 1.5)
# text(4.8, 3.8, expression(paste(f)))
# 
# points(5, 4.4, pch = 16, col = "black", cex = 1.5)
# text(5.2, 4.4, expression(paste(n)))
# 
# 


#Initial Allocations
# points(x = 8.48, y = 0.92, pch = 16, col = "black", cex = 1.5)
# text(8.3, 0.8, expression(paste(e)))




#Braces for labels
# brackets(x1 = 8.9, y1 = -0.3, x2 = 5, y2 = -0.3,  
#          ticks = 0.5, curvature = 0.5, type = 1, 
#          col = "black", lwd = 2, lty = 1, xpd = TRUE)
# text(7, -1, expression(paste("Quantity of the good, x")), xpd = TRUE)
# text(7, -1.4, expression(paste("A sells to B")), xpd = TRUE)
# 
# brackets(x1 = 10.2, y1 = 5, x2 = 10.2, y2 = 1.1,  
#          ticks = 0.5, curvature = 0.5, type = 1, 
#          col = "black", lwd = 2, lty = 1, xpd = TRUE)
# text(11.2, 3, expression(paste("Quantity of money, y")), xpd = TRUE, srt = 270)
# text(10.9, 3, expression(paste("B pays A")), xpd = TRUE, srt = 270)
# 


text(-0.06, -10, expression("Nadya"), xpd = TRUE, cex = namesize, col = COLA[4])
text(1.06, 210, expression("Walter"), xpd = TRUE, cex = namesize, col = COLB[4])


par(new = TRUE)

#Use the same x and ylims as previously, but with locations switched
xlims2 <- c(1, 0)
ylims2 <- c(200, 0)

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
#b <- c(uB2(16, 0),  uB2(16,0) + 254)
b <- c(uB(0,125), uB(0.3,150))

contour(x, y, 
        outer(x, y, uB),
        drawlabels = FALSE,
        col = COLB[2],
        lwd = graphlinewidth,
        levels = b, 
        add = TRUE
) 

#Set up axes at sides 3 and 4 (top and right)
axis(side = 3, at = ticksx, pos = 0, labels = xlabels)
axis(side = 4, at = ticksy, pos = 0, labels = ylabels, las = 0)
#mtext(, side = 3, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], -15, expression(paste("W's Risk Exposure, ", Delta^W == 1 - Delta^N)), xpd = TRUE, cex = axislabelsize) 
text(-0.08, 0.5*ylims[2], expression(paste("W's Expected wealth, ", omega^W)), xpd = TRUE, cex = axislabelsize, srt = 270) 

#Add arrows:
arrows(-1.2, 260, -1.2, 380, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)
arrows(10.6, -50, 15, -50, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)

points(x = 0, y = 125, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(+ 0.03, 130, expression(paste(z)))

points(8, 318, pch = 16, col = "black", cex = 1.5)
text(7.7, 310, expression(paste(t^{B})))

points(8, 64, pch = 16, col = "black", cex = 1.5)
text(7.7, 56, expression(paste(t^{A})))


#Customize ticks and labels for the plot
#ticksy <- seq(from = 0, to = 15, by = 1)
#ylabels <- seq(from = 0, to = 15, by = 1)
#ticksx <- seq(from = 0, to = 10, by = 1)
#xlabels <- seq(from = 0, to = 10, by = 1)
#axis(1, at = ticksx, pos = 0, labels = xlabels)
#axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0)

dev.off()

