require(shape)
#library(extrafont)
library(pBrackets)
pdf(file = "risk/eb_risk_qql_initial.pdf", width = 8, height = 8)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(4, 5, 4, 5))

uA <- function(x, y, alpha = 300) {
  y - alpha*(x)^2
}

indiffA <- function(x, 
                    utility =  uA(x = 1, y = ylims[2]/3), 
                    alpha = 300) {
  utility + alpha*(x)^2
}

uB <- function(x, y, beta = 200, Tb = 1, ybar = 1000) {
  (ybar-y) - beta*(Tb - x)^2
}

indiffB <- function(x, 
                    utility = uB(x = 1, y = ylims[2]/3), 
                    beta = 200, Tb = 1, ybar = 1000) {
  ybar - utility - beta*((Tb - x))^2
}

priceIns <- function(x, slope = 275, intercept = 1000 - 941.6667) {
  intercept + slope*x
  }
  
  

# WalrasP <- function(x, slope = 1, intercept = 9) {
#   intercept - slope*x
# }




xlims <- c(0, 1)
ylims <- c(0, 1000)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(uA(x = 1, y = ylims[2]/3), uA(x = 0.4, y = 263))
b <- c(uB(x = 1, y = ylims[2]/3), uB(x = 0.4, y = 79))

#Use the same x and ylims as previously, but with locations switched
xlims2 <- c(1, 0)
ylims2 <- c(1000, 0)

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

axis(side = 3, at = ticksx, pos = 0, labels = xlabels)
axis(side = 4, at = ticksy, pos = 0, labels = ylabels, las = 0)


#Set up axes at sides 3 and 4 (top and right)
#text(5, -1, expression(paste("B's Good, x")), xpd = TRUE, cex = axislabelsize) 
#mtext("B's Good, x", side = 3, line = 2.5, cex = axislabelsize)
text(-0.1, 0.5*ylims[2], expression(paste("W's Expected wealth, ", omega^W)), xpd = TRUE, cex = axislabelsize, srt = 270) 
text(0.5*xlims[2], -100, expression(paste("W's Risk, ", Delta^W == 1 - Delta^N)), xpd = TRUE, cex = axislabelsize) 



#Add arrows for W:
arrows(-0.1, 740, -0.1, 950, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(0.7, -100, 0.9, -100, xpd = TRUE, length=0.1,angle=40,lwd=3)


priceLine <- function(x, slope = 275, intercept = 2*ylims[2]/3) {
  intercept + slope*x
}

xx1 <- seq(xlims[1], xlims[2], length.out = npts)
#lines(xx1, priceLine(xx1), col = "gray", lwd = segmentlinewidth)



par(new = TRUE)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

ticksy <- seq(from = ylims[1], to = ylims[2], by = 100)
ylabels <- seq(from = ylims[1], to = ylims[2], by = 100)
ticksx <- seq(from = 0, to = xlims[2], by = 0.2)
xlabels <- seq(from = 0, to = xlims[2], by = 0.2)


axis(side = 1, at = ticksx, pos = 0, labels = xlabels)
axis(side = 2, at = ticksy, pos = 0, labels = ylabels, las = 0)

#Pareto-improving lens
xpoly1 <- seq(from = xlims[1], 
              to = xlims[2], 
              length.out = 500)
ypoly1 <- indiffA(xpoly1)
ypoly2 <- indiffB(xpoly1)
polygon(x = c(xpoly1, rev(xpoly1)), 
        y = c(ypoly1, rev(ypoly2)), 
        col = COL[4], density = NULL, 
        border = NA)

# lines(xx1, priceIns(xx1), col = "gray", 
#       lwd = segmentlinewidth, lty = 2)

contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLA[3],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE) 

text(0.5*xlims[2], -100, expression(paste("N's Risk, ", Delta^N)), xpd = TRUE, cex = axislabelsize) 
#mtext("A's Good, x", side = 1, line = 2.5, cex = axislabelsize)
text(-0.1, 0.5*ylims[2], expression(paste("N's Expected Wealth,", omega^N)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Add arrows for N:
arrows(-0.1, 740, -0.1, 950, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(0.63, -100, 0.9, -100, xpd = TRUE, length=0.1,angle=40,lwd=3)


xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(2.5, xlims[2], length.out = npts)
#lines(xx1, WalrasP(xx1, intercept = 11), col = "gray", lwd = segmentlinewidth)
#lines(xx2, WalrasP(xx2, intercept = 9.4), col = "purple", lwd = segmentlinewidth, lty = 1)
#lines(xx1, WalrasP(xx1, intercept = 10.9, slope = 8.2/7), col = "purple", lwd = segmentlinewidth, lty = 1)

contour(x, y, 
        outer(x, y, uB),
        drawlabels = FALSE,
        col = COLB[2],
        lwd = graphlinewidth,
        levels = b, 
        add = TRUE
) 

# segments(5, 3.95, 5, 6.05, lty = 1, col = COL[2] , lwd = graphlinewidth)
# segments(5, 0, 5, 3.95, col = COL[2] , lwd = segmentlinewidth, lty = 2)
# segments(5, 6.05, 5, 10, col = COL[2] , lwd = segmentlinewidth, lty = 2)

#Label the PEC
# text(8, 8.8, expression("Pareto Efficient Curve"))
# Arrows(6.8, 8.8, 5.2, 8.8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the walrasian P
# text(4, 9.6, expression(paste("Price line")))
# text(4, 9.2, expression(slope == -p[n] ))
# Arrows(4, 9, 4, 5.8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# 

#Label indiffs for N
text(0.85, 220, expression(u[1]^N))
text(0.85, 410, expression(u[2]^N))

#Label the indiffs for B
text(0.7, 290, expression(u[1]^W))
text(0.7, 110, expression(u[2]^W))

#Point for seeing where the indifference curves intersect on the LHS
#segments(0.4, ylims[1], 0.4, ylims[2], col = COL[2] , lwd = segmentlinewidth, lty = 2)
#segments(4, ylims[1], 4, ylims[2], col = COL[2] , lwd = segmentlinewidth, lty = 2)
#segments(0, 0, xlims[2], 0, col = COL[2] , lwd = segmentlinewidth, lty = 2)

points(1, ylims[2]/3, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(1 - 0.025, ylims[2]/3 + 20, expression(z))

points(0.4, indiffA(0.4), pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(0.4, indiffA(0.4) - 20, expression(b))

points(0.7, indiffA(0.7), pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(0.7, indiffA(0.7) - 20, expression(c))


# points(0.45, indiffA(x = 0.45, 
#                      utility = uA(x = 1, y = ylims[2]/3) + 90), 
#        pch = 16, col = "black", cex = 1.5, xpd = TRUE)
# text(0.45, indiffA(x = 0.45, 
#                    utility = uA(x = 1, y = ylims[2]/3) + 90) - 20, 
#      expression(a))


#Label point f. 
points(4, -3.75, pch = 16, col = "black", cex = 1.5)
text(3.9, -3.5, expression(paste(f)))

text(0.5, 200, expression(paste("Pareto-improving")))
text(0.5, 170, expression(paste("lens")))
# 
# 
points(4, -8.25, pch = 16, col = "black", cex = 1.5)
text(4.1, -8.75, expression(paste(g)))

#Initial Allocations
# segments(8.48, 0, 8.48, 0.88, col = COL[2] , lwd = segmentlinewidth, lty = 2)
# segments(10, 0.88, 8.48, 0.88, col = COL[2] , lwd = segmentlinewidth, lty = 2)
# 
# points(x = 8.48, y = 0.92, pch = 16, col = "black", cex = 1.5)
# text(8.3, 0.8, expression(paste(e)))

#Braces for labels
# brackets(x1 = 8.5, y1 = -0.3, x2 = 5, y2 = -0.3,  
#          ticks = 0.5, curvature = 0.5, type = 1, 
#          col = "black", lwd = 2, lty = 1, xpd = TRUE)
# text(6.6, -1, expression(paste("Quantity of the good, x")), xpd = TRUE)
# text(6.6, -1.4, expression(paste("A sells to B")), xpd = TRUE)
# 
# brackets(x1 = 10.2, y1 = 4.4, x2 = 10.2, y2 = 0.9,  
#          ticks = 0.5, curvature = 0.5, type = 1, 
#          col = "black", lwd = 2, lty = 1, xpd = TRUE)
# text(11.2, 2.6, expression(paste("Quantity of money, y")), xpd = TRUE, srt = 270)
# text(10.9, 2.6, expression(paste("B pays A")), xpd = TRUE, srt = 270)





dev.off()

