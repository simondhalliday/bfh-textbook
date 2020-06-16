require(shape)
library(pBrackets)
pdf(file = "risk/eb_risk_qql_trade.pdf", width = 8, height = 8)

# Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)

par(mar =  c(7, 9, 6, 6))

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


xlims <- c(0, 1)
ylims <- c(0, 1000)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(uA(x = 1, y = ylims[2]/3), 
       uA(x = 1, y = ylims[2]/3) + 90)
b <- c(uB(x = 1, y = ylims[2]/3))

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
     xaxs = "i", 
     yaxs = "i")


ticksy2 <- c(ylims[1], 2*ylims[2]/3,ylims[2])
ylabels2 <- c(0, expression(paste(y[z]^W)), 1000)
ticksx2 <- c(0, 0.55, 1)
xlabels2 <- c(0, expression(paste(Delta[a]^W)), 1)

axis(side = 3, at = ticksx2, pos = 0, labels = xlabels2, cex.axis = labelsize)
axis(side = 4, at = ticksy2, pos = 0, labels = ylabels2, las = 1, cex.axis = labelsize)


#Set up axes at sides 3 and 4 (top and right)
#text(5, -1, expression(paste("B's Good, x")), xpd = TRUE, cex = axislabelsize) 
#mtext("B's Good, x", side = 3, line = 2.5, cex = axislabelsize)
text(-0.17, 0.55*ylims[2], expression(paste("W's expected income, ", hat(y)^W)), xpd = TRUE, cex = axislabelsize, srt = 270) 
text(0.5*xlims[2], -150, expression(paste("W's risk, ", Delta^W == 1 - Delta^N)), xpd = TRUE, cex = axislabelsize) 



#Add arrows:
# arrows(-0.8, 3, -0.8, 5, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)
# arrows(6, -1, 9, -1, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)


priceLine <- function(x, slope = 275, intercept = 2*ylims[2]/3) {
  intercept + slope*x
}

xx1 <- seq(xlims[1], xlims[2], length.out = npts)
#lines(xx1, priceLine(xx1), col = "gray", lwd = segmentlinewidth)

#Add arrows:
# arrows(-0.1, 740, -0.1, 950, xpd = TRUE, length=0.1,angle=40,lwd=3)
# arrows(0.72, -100, 0.9, -100, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(-0.15, 850, -0.15, 950, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(0.8, -145, 0.9, -145, xpd = TRUE, length=0.1,angle=40,lwd=3)



par(new = TRUE)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs = "i", 
     yaxs = "i")

ticksy <- c(ylims[1], 
            indiffA(x = 0.45, utility = uA(x = 1, y = ylims[2]/3) + 90), 
            ylims[2]/3,ylims[2])
ylabels <- c(0, expression(paste(hat(y)[z]^N - p[s]*(1-Delta[a]^N))), expression(paste(hat(y)[z]^N)), 1000)
ticksx <- c(0, 0.45, 1)
xlabels <- c(0, expression(paste(Delta[a]^N)), NA)


axis(side = 1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(side = 2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)


lines(xx1, priceIns(xx1), col = COL[2], 
      lwd = segmentlinewidth, lty = 1)

contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLA[4],
        lwd = graphlinewidth,
        levels = a, 
        xaxs = "i", 
        yaxs = "i", 
        add = TRUE) 

text(1.025, -50, expression(1), cex = labelsize, xpd = TRUE)

text(0.5*xlims[2], -170, expression(paste("N's risk, ", Delta^N)), xpd = TRUE, cex = axislabelsize) 
#mtext("A's Good, x", side = 1, line = 2.5, cex = axislabelsize)
text(-0.19, 0.55*ylims[2], expression(paste("N's expected income,", hat(y)^N)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Add arrows for N:
arrows(-0.17, 840, -0.17, 950, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(0.68, -175, 0.9, -175, xpd = TRUE, length=0.1,angle=40,lwd=3)


xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(2.5, xlims[2], length.out = npts)


#Label indiffs for N
text(0.85, 210, expression(u[1]^N), cex = annotatesize)
text(0.85, 390, expression(u[2]^N), cex = annotatesize)

#Label the indiffs for B
#text(0.15, 220, expression(u[1]^W))
#text(1, 6.8, expression(u[2]^W))
#text(2.6, 8.1, expression(v[3]^B))
#text(3.4, 6.9, expression(v[4]^B))

#Point for seeing where the indifference curves intersect on the LHS


points(1, ylims[2]/3, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(1 - 0.025, ylims[2]/3 + 20, expression(z), cex = annotatesize)

# points(0.4, indiffA(0.4), pch = 16, col = "black", cex = 1.5, xpd = TRUE)
# text(0.4, indiffA(0.4) - 20, expression(b))
# 
# points(0.7, indiffA(0.7), pch = 16, col = "black", cex = 1.5, xpd = TRUE)
# text(0.7, indiffA(0.7) - 20, expression(c))

#Segment lines
segments(0, 333, 1, 333, col = grays[20], lwd = segmentlinewidth, lty = 2)
segments(0.45, 0, 0.45, ylims[2], col = grays[20], lwd = segmentlinewidth, lty = 2)
segments(0, indiffA(x = 0.45, utility = uA(x = 1, y = ylims[2]/3)) + 90, 0.45, indiffA(x = 0.45, utility = uA(x = 1, y = ylims[2]/3)) + 90,
         col = grays[20], lwd = segmentlinewidth, lty = 2)


points(0.45, indiffA(x = 0.45, 
                     utility = uA(x = 1, y = ylims[2]/3) + 90), 
       pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(0.45 + 0.02, indiffA(x = 0.45, 
                   utility = uA(x = 1, y = ylims[2]/3) + 90) - 30, 
     expression(a), cex = annotatesize)




# points(5, 4.4, pch = 16, col = "black", cex = 1.5)
# text(5.2, 4.6, expression(paste(n)))
# 
# 
#Initial Allocations

#text(0.3, 105, expression(paste("Slope", phantom() == p[s])), cex = annotatesize)

text(0.7, 55, expression(paste("Slope", phantom() == p[s])), cex = annotatesize)
Arrows(0.7, 75, 0.7, 225, col = "black", lty = 1, lwd = 2, arr.type = "triangle")

#Braces for labels
brackets(x1 = 1, y1 = -10, x2 = 0.48, y2 = -10,
         ticks = 0.5, curvature = 0.5, type = 1,
         col = "black", lwd = 2, lty = 1, h = 20, xpd = TRUE)
text(0.75, -60, expression(paste("Quantity of insurance")), xpd = TRUE, cex = annotatesize)
text(0.75, -100, expression(paste("N buys from W")), xpd = TRUE, cex = annotatesize)

brackets(x1 = 0.44, y1 = -10, x2 = 0, y2 = -10,
         ticks = 0.5, curvature = 0.5, type = 1,
         col = "black", lwd = 2, lty = 1, h = 20, xpd = TRUE)
text(0.225, -60, expression(paste("N's remaining")), xpd = TRUE, cex = annotatesize)
text(0.225, -100, expression(paste("risk exposure")), xpd = TRUE, cex = annotatesize)

brackets(x1 = 0.02, y1 = ylims[2]/3 - 8,
         x2 = 0.02, y2 = indiffA(x = 0.45, 
                                 utility = uA(x = 1, y = ylims[2]/3) + 90) + 8, 
         ticks = 0.5, curvature = 0.5, type = 1,
         col = "black", lwd = 2, lty = 1, h = 0.02, xpd = TRUE)
text(0.25, 290, expression(paste("Cost of insurance")), xpd = TRUE, cex = annotatesize)
text(0.25, 230, expression(paste("for Nadya")), xpd = TRUE, cex = annotatesize)
# 

# brackets(x1 = 10.2, y1 = 4.4, x2 = 10.2, y2 = 0.9,  
#          ticks = 0.5, curvature = 0.5, type = 1, 
#          col = "black", lwd = 2, lty = 1, xpd = TRUE)
# text(11.2, 2.6, expression(paste("Quantity of money, y")), xpd = TRUE, srt = 270)
# text(10.9, 2.6, expression(paste("B pays A")), xpd = TRUE, srt = 270)





dev.off()

