require(shape)
library(extrafont)
library(pBrackets)
pdf(file = "what_can_markets_do/edgeworthbox_curfew.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 7, 4, 5))

uA <- function(x, y, alpha = 1/4, Ta = 1) {
  y - alpha*(Ta - x)^2
}

indiffA <- function(x, utility = -9, alpha = 1/4, Ta = 1) {
  utility + alpha*(Ta - x)^2
}

uB <- function(x, y, beta = 1/4, Tb = 7) {
  -(y) - beta*(Tb - x)^2
}

indiffB <- function(x, utility = 0, beta = 1/4, Tb = 7) {
  utility - beta*(x - Tb)^2
}


# WalrasP <- function(x, slope = 1, intercept = 9) {
#   intercept - slope*x
# }




xlims <- c(0, 8)
ylims <- c(-12, 12)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
#a <- seq(-400, -200, by = 20)
a <- c(-9, -6.75, -4.5, -9/4, 0)
b <- c(-9, -9/4, 0, 2.25, 4.5)

#Use the same x and ylims as previously, but with locations switched
xlims2 <- c(8, 0)
ylims2 <- c(12, -12)

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


ylabels2 <- seq(ylims[1], ylims[2], by = 2)
#Set up axes at sides 3 and 4 (top and right)
axis(side = 3, at = ticksx, pos = -12, labels = NA)
axis(side = 4, at = ticksy, pos = 0, labels = ylabels2, las = 0)
#text(5, -1, expression(paste("B's Good, x")), xpd = TRUE, cex = axislabelsize) 
#mtext("B's Good, x", side = 3, line = 2.5, cex = axislabelsize)
text(-0.8, 0.15*ylims[2], expression(paste("B's Money, y")), xpd = TRUE, cex = axislabelsize, srt = 270) 



#Add arrows:
# arrows(-0.8, 3, -0.8, 5, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)
# arrows(6, -1, 9, -1, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)

par(new = TRUE)



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

ticksy <- seq(from = -12, to = 12, by = 2)
ylabels <- seq(from = -12, to = 12, by = 2)
#ylabels <- c(-12, -10, -8, -6, NA, NA, 0, 2, 4, 6, 8, 10, 12)
ticksx <- seq(from = 0, to = 8, by = 1)
xlabels <- c(paste("8pm"), paste("9pm"), paste("10pm"), paste("11pm"),
             paste("12am"), paste("1am"), paste("2am"), paste("3am"), paste("4am"))
#xlabels <- c("8pm", "9pm", "10pm", "11pm", "12am", "1am", "2am", "3am", "4am")
#xlabels <- seq(from = 0, to = 8, by = 1)
axis(1, at = ticksx, pos = -12, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0)

#Pareto-improving lens
xpoly1 <- seq(from = 1, to = 7, length.out = 500)
ypoly1 <- indiffA(xpoly1)
ypoly2 <- indiffB(xpoly1)
polygon(x = c(xpoly1, rev(xpoly1)), y = c(ypoly1, rev(ypoly2)), col = COL[4], density = NULL, border = NA)


contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLA[3],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE) 

text(0.5*xlims[2], -15, expression(paste("The curfew, x")), xpd = TRUE, cex = axislabelsize) 
#mtext("A's Good, x", side = 1, line = 2.5, cex = axislabelsize)
text(-0.6, 0.5*ylims[2], expression(paste("A's Money, y")), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Add arrows:
#arrows(-0.6, 6.5, -0.6, 9, xpd = TRUE, length=0.1,angle=40,lwd=3)
#arrows(2.5, -1.2, 4.5, -1.2, xpd = TRUE, length=0.1,angle=40,lwd=3)


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
text(6, -9, expression("Pareto Efficient Curve"))
Arrows(5, -9, 4.2, -9, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the walrasian P
# text(4, 9.6, expression(paste("Price line")))
# text(4, 9.2, expression(slope == -p[n] ))
# Arrows(4, 9, 4, 5.8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# 

#Label the indifference curves for A
text(0.5, -9.6, expression(u[1]^A))
text(0.5, -7.4, expression(u[2]^A))
text(0.5, -5.1, expression(u[3]^A))
text(0.5, -2.9, expression(u[4]^A))
text(0.5, 0.8, expression(u[5]^A))

#Label the indifference curves for B
text(7.5, 9.6, expression(u[1]^B))
text(7.5, 2.9, expression(u[2]^B))
text(7.5, -0.9, expression(u[3]^B))
text(7.5, -3.1, expression(u[4]^B))
text(7.5, -5.2, expression(u[5]^B))


#Point for seeing where the indifference curves intersect on the LHS


segments(4, ylims[1], 4, -6.75, col = "purple" , lwd = segmentlinewidth, lty = 2)
segments(4, -6.75, 4, -2.5, col = "purple" , lwd = segmentlinewidth, lty = 1)
segments(4, -2.5, 4, ylims[2], col = "purple" , lwd = segmentlinewidth, lty = 2)

segments(0, 0, xlims[2], 0, col = COL[2] , lwd = segmentlinewidth, lty = 2)

points(7, 0, pch = 16, col = "black", cex = 1.5)
text(6.9, 0.5, expression(e))

points(4, 0, pch = 16, col = "black", cex = 1.5)
text(3.9, 0.5, expression(i))


#Label point i. 
points(4, -2.25, pch = 16, col = "black", cex = 1.5)
text(3.9, -1.5, expression(paste(f)))

segments(0, -4.5, 4, -4.5, col = COL[2] , lwd = segmentlinewidth, lty = 2)
points(4, -4.5, pch = 16, col = "black", cex = 1.5)
text(4.1, -5, expression(paste(n)))
# 

points(4, -6.75, pch = 16, col = "black", cex = 1.5)
text(4.1, -7.5, expression(paste(g)))


brackets(x1 = -0.5, y1 = -4.5, x2 = -0.5, y2 = 0,  
         ticks = 0.5, curvature = 0.5, type = 1, 
         col = "black", lwd = 1, lty = 1, xpd = TRUE)
#text(6.6, -1, expression(paste("Quantity of the good, x")), xpd = TRUE)
#text(6.6, -1.4, expression(paste("A sells to B")), xpd = TRUE)


#Initial Allocations
# segments(8.48, 0, 8.48, 0.88, col = COL[2] , lwd = segmentlinewidth, lty = 2)
# segments(10, 0.88, 8.48, 0.88, col = COL[2] , lwd = segmentlinewidth, lty = 2)
# 
points(x = 1, y = 0, pch = 16, col = "black", cex = 1.5)
text(0.9, 0.5, expression(paste(e*minute)))




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
text(-1.5, -2.25, expression(paste("Quantity of money, y")), xpd = TRUE, srt = 90)
text(-1.3, -2.5, expression(paste("A pays B")), xpd = TRUE, srt = 90)


dev.off()

