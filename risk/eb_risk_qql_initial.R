require(shape)
#library(extrafont)
library(pBrackets)
pdf(file = "risk/eb_risk_qql_initial.pdf", width = 8, height = 8)

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

par(mar =  c(5, 6, 5, 6))

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

axis(side = 3, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(side = 4, at = ticksy, pos = 0, labels = ylabels, las = 0, cex.axis = labelsize)


#Set up axes at sides 3 and 4 (top and right)
#text(5, -1, expression(paste("B's Good, x")), xpd = TRUE, cex = axislabelsize) 
#mtext("B's Good, x", side = 3, line = 2.5, cex = axislabelsize)

text(-0.15, 0.5*ylims[2], expression(paste("W's expected income, ", hat(y)^W)), xpd = TRUE, cex = axislabelsize, srt = 270) 

#text(0.4*xlims[2], -100, expression(paste("W's risk, ", Delta^W == 1 - Delta^N)), xpd = TRUE, cex = axislabelsize) 
text(0.5*xlims[2], -125, expression(paste("W's risk, ", Delta^W == 1 - Delta^N)), xpd = TRUE, cex = axislabelsize) 



#Add arrows for W:
# arrows(-0.1, 740, -0.1, 950, xpd = TRUE, length=0.1,angle=40,lwd=3)
# arrows(0.7, -100, 0.9, -100, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(-0.13, 780, -0.13, 900, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(0.78, -120, 0.9, -120, xpd = TRUE, length=0.1,angle=40,lwd=3)


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
     xaxs = "i", 
     yaxs = "i")

ticksy <- seq(from = ylims[1], to = ylims[2], by = 100)
#ylabels <- seq(from = ylims[1], to = ylims[2], by = 100)
ylabels <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
ticksx <- seq(from = 0, to = xlims[2], by = 0.2)
xlabels <- seq(from = 0, to = xlims[2], by = 0.2)



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


contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLA[3],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE) 

text(0.5*xlims[2], -100, expression(paste("N's risk, ", Delta^N)), xpd = TRUE, cex = axislabelsize) 

#mtext("A's Good, x", side = 1, line = 2.5, cex = axislabelsize)

text(-0.15, 0.5*ylims[2], expression(paste("N's expected income,", hat(y)^N)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Add arrows for N:
# arrows(-0.1, 740, -0.1, 950, xpd = TRUE, length=0.1,angle=40,lwd=3)
# arrows(0.63, -100, 0.9, -100, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(-0.13, 780, -0.13, 900, xpd = TRUE, length=0.1,angle=40,lwd=3)
#arrows(0.78, -120, 0.9, -120, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(0.68, -100, 0.85, -100, xpd = TRUE, length=0.1,angle=40,lwd=3)

xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(2.5, xlims[2], length.out = npts)

contour(x, y, 
        outer(x, y, uB),
        drawlabels = FALSE,
        col = COLB[2],
        lwd = graphlinewidth,
        levels = b, 
        add = TRUE
) 


#Label indiffs for N
text(0.85, 210, expression(u[1]^N), cex = labelsize)
text(0.85, 400, expression(u[2]^N), cex = labelsize)

#Label the indiffs for B
text(0.7, 280, expression(u[1]^W), cex = labelsize)
text(0.7, 100, expression(u[2]^W), cex = labelsize)


points(1, ylims[2]/3, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(1 - 0.025, ylims[2]/3 + 20, expression(z), cex = labelsize)

points(0.4, indiffA(0.4), pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(0.4, indiffA(0.4) - 30, expression(b), cex = labelsize)

points(0.7, indiffA(0.7), pch = 16, col = "black", cex = 1.5, xpd = TRUE)
text(0.7, indiffA(0.7) - 30, expression(c), cex = labelsize)


#Label point f. 
points(4, -3.75, pch = 16, col = "black", cex = 1.5)
text(3.9, -3.5, expression(paste(f)), cex = labelsize)

text(0.5, 210, expression(paste("Pareto-improving")), cex = labelsize)
text(0.5, 170, expression(paste("lens")), cex = labelsize)
# 
# 
points(4, -8.25, pch = 16, col = "black", cex = 1.5)
text(4.1, -8.75, expression(paste(g)), cex = labelsize)

axis(side = 1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(side = 2, at = ticksy, pos = 0, labels = ylabels, las = 0, cex.axis = labelsize)





dev.off()

