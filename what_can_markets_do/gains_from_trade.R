require(shape)
library(extrafont)
library(pBrackets)
pdf(file = "what_can_markets_do/gains_from_trade.pdf", width = 5, height = 4)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(2, 1, 1, 1))

xlims <- c(0, 6)
ylims <- c(0, 2)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

ticksy <- seq(from = 0, to = 6, by = 1)
ylabels <- seq(from = 0, to = 10, by = 1)
ticksx <- c(xlims[1], 1, 2, 4, 5, xlims[2])
xlabels <- c(0, expression(S), expression(s), expression(b), expression(paste(B)), 1)
axis(1, at = ticksx, pos = 1, labels = xlabels)
axis(2, at = NA, pos = 0, labels = NA, las = 0)


brackets(x1 = 2, y1 = 1.05, x2 = 4, y2 = 1.05,  
         ticks = 0.5, curvature = 0.5, type = 1, 
         col = "black", lwd = 2, lty = 1, xpd = TRUE)
text(3, 1.3, expression(paste("Realized trade")), xpd = TRUE, srt = 0)

brackets(x1 = 5, y1 = 0.75, x2 = 1, y2 = 0.75,  
         ticks = 0.5, curvature = 0.5, type = 1, 
         col = "black", lwd = 2, lty = 1, xpd = TRUE)
text(3, 0.5, expression(paste("Gains from trade")), xpd = TRUE, srt = 0)
#text(3, .25, expression(paste("From B to A")), xpd = TRUE, srt = 270)



#Pareto-improving lens
#ypoly1 <- indiffA(xpoly1, utility = 10)
#ypoly2 <- WalrasP(xpoly1, intercept = 8.5)
#polygon(x = c(xpoly1, rev(xpoly1)), y = c(ypoly1, rev(ypoly2)), col = COL[4], density = NULL, border = NA)

#ypoly3 <- indiffA(xpoly1, utility  = 13)
#ypoly4 <- WalrasP(xpoly1, intercept = 11.5)
#polygon(x = c(xpoly1, rev(xpoly1)), y = c(ypoly4, rev(ypoly3)), col = COL[4], density = NULL, border = NA)



#text(0.5*ylims[2], -1.2, expression(paste("A's Good, x")), xpd = TRUE, cex = axislabelsize) 
#mtext("A's Good, x", side = 1, line = 2.5, cex = axislabelsize)
#text(-0.6, 0.5*ylims[2], expression(paste("A's Money, y")), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Add arrows:
#arrows(-0.6, 6.5, -0.6, 9, xpd = TRUE, length=0.1,angle=40,lwd=3)
#arrows(6, -1.2, 9, -1.2, xpd = TRUE, length=0.1,angle=40,lwd=3)


dev.off()

