require(shape)
pdf(file = "property/property_tioli_asymmetric.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 3

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")


uA <- function(xA, yA, alpha = 2/3){
  ((xA)^alpha)*(yA^(1 - alpha))
}


indiffcurveA1 <- function(x, U = 2, A = 1, a = 2/3) {
  ((((U)/A)*(1/x)^a)^(1/(1-a)))
}

indiffcurveA2 <- function(x, U = 4, A = 1, a = 2/3) {
  (((U/A)*(1/x)^a)^(1/(1-a)))
}

indiffcurveA3 <- function(x, U = 6.123724, A = 1, a = 2/3) {
  (((U/A)*(1/x)^a)^(1/(1-a)))
}

indiffcurveA4 <- function(x, U = 7.156591, A = 1, a = 2/3) {
  ((((U)/A)*(1/x)^a)^(1/(1-a)))
}

paretoEC <- function(x, slope1 = 15, slope2 = 3, int = 40) {
  (slope1*x)/(int - slope2*x)
}


uB <- function(xA, yA, alpha = 1/3){
  ((10-xA)^alpha)*((15-yA)^(1 - alpha))
}

indiffcurveBneg <- function(x, U = 5.09, A = 1, a = 1/3) {
  15 - (((U/A)*(1/(10 - x))^a)^(1/(1-a)))
}

indiffcurveBneg1 <- function(x, U = 3.09, A = 1, a = 1/3) {
  15 - (((U/A)*(1/(10 - x))^a)^(1/(1-a)))
}

indiffcurveBneg2 <- function(x, U = 5.09, A = 1, a = 1/3) {
  15 - (((U/A)*(1/(10 - x))^a)^(1/(1-a)))
}

indiffcurveBneg3 <- function(x, U = 8.244574, A = 1, a = 1/3) {
  15 - (((U/A)*(1/(10 - x))^a)^(1/(1-a)))
}




#Aisha happens to have found 8 apples and 2 oranges, 
#and Betty happens to have found 2 apples and 13 oranges. 
#Aisha's utility (8^0.5)*(2^0.5) = 4
#Betty's utility (2^0.5)*(13^0.5) = 5.09


par(mar =  c(6, 4, 4, 4))
xlims <- c(0, 10)
ylims <- c(0, 15)
xlims2 <- c(10, 0)
ylims2 <- c(15, 0)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = "",
     ylab = "", 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

mtext(expression(paste("A's coffee (bags), ", x^A)), side = 1, line = 2.5, cex = axislabelsize)
text(-1, 0.5*ylims[2], expression(paste("A's data (gigabytes), ", y^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
yy1 <- indiffcurveA2(xx1, U = uA(8,2), A = 1, a = 2/3)
yy2 <- indiffcurveA2(xx1)

#Polygon Attempt
#polygon(x = c(1.34, 6, 8, 10 - 6.73), y = c(12, 9, 2, 15 - 10.1), col="powderblue", density=NULL, border = NA)

#I need something like xx1 with npts for 
xpoly1 <- seq(from = 4.15, to = 8, length.out = 500)
ypoly1 <- indiffcurveA2(xpoly1, U = uA(8,2), A = 1, a = 2/3)
ypoly2 <- indiffcurveBneg(xpoly1, U = uB(8,2), A = 1, a = 1/3)
polygon(x = c(xpoly1, rev(xpoly1)), y = c(ypoly1, rev(ypoly2)), col=COL[4], density=NULL, border = NA)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(uA(8,2) - 1.5, uA(8,2), uA(8,2) + 1)
contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLA[3],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE)

npts <- 500 
npts2 <- 501


#Draw the lines for the graphs
#lines(xx1, indiffcurveA1(xx1), col = COLA[3], lwd = graphlinewidth)
#lines(xx1, indiffcurveA2(xx1), col = COLA[3], lwd = graphlinewidth)
#lines(xx1, indiffcurveA3(xx1), col = COLA[3], lwd = graphlinewidth)
#lines(xx1, indiffcurveA4(xx1), col = COLA[3], lwd = graphlinewidth)
lines(xx1, paretoEC(xx1), col = COL[2], lwd = graphlinewidth)

# lines(xx1, indiffcurveBneg1(xx1), col = COLB[2], lwd = graphlinewidth)
#lines(xx1, indiffcurveBneg2(xx1, U = uB(8,2), a = 1/3), col = COLB[2], lwd = graphlinewidth)
# lines(xx1, indiffcurveBneg3(xx1), col = COLB[2], lwd = graphlinewidth)

#Customize ticks and labels for the plot
ticksy <- seq(from = 0, to = 15, by = 1)
ylabels <- seq(from = 0, to = 15, by = 1)
ticksx <- seq(from = 0, to = 10, by = 1)
xlabels <- seq(from = 0, to = 10, by = 1)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0)

#Add arrows:
arrows(-0.9, 10.5, -0.9, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(6.2, -1.6, 9, -1.6, xpd = TRUE, length=0.1,angle=40,lwd=3)

#Annotation of the three graphs and the NE
text(9.6, 0.85, expression(u[1]^A))
text(9.6, 1.8, expression(u[2]^A))
text(9.6, 2.85, expression(u[3]^A))

#Perhaps useful point to label the unused intersection of the participation constraints
#points(1.34, 12, pch = 16, col = "black", cex = 1.5)



#Pareto efficient curve
#segments(4.16, 6.23, 6.73, 10.1, lty = 2, lwd = 2)
text(8.5, 12.5, expression("Pareto-efficient"))
text(8.5, 12, expression("curve"))
Arrows(8.5, 11.7, 8.5, 9.4, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#Label Pareto Improving Lens
text(5.5, 12.5, expression(paste("Pareto-improving")))
text(5.5, 12, expression(paste("lens")))
Arrows(5.5, 11.7, 5.5, 5.4, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#Set up second axes and labels

par(new = TRUE)

#Use the same x and ylims as previously, but with locations switched
xlims2 <- c(10, 0)
ylims2 <- c(15, 0)

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

#Set up axes at sides 3 and 4 (top and right)
axis(side = 3, at = ticksx, pos = 0, labels = xlabels)
axis(side = 4, at = ticksy, pos = 0, labels = ylabels, las = 0)
mtext("B's Apples, x", side = 3, cex = axislabelsize)
text(-0.8, 7, expression(paste("B's Oranges, y")), xpd = TRUE, cex = axislabelsize, srt = 270) 

#Add arrows:
arrows(-0.8, 10, -0.8, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(6.2, -1.8, 9, -1.8, xpd = TRUE, length=0.1,angle=40,lwd=3)


uB2 <- function(xB, yB, alpha = 1/3){
  ((xB)^alpha)*((yB)^(1-alpha))
}

b <- c(uB2(2,13) - 1.5, uB2(2,13), uB2(2,13) + 1.12)
contour(x, y,
        outer(x, y, uB2),
        drawlabels = FALSE,
        col = COLB[3],
        lwd = graphlinewidth,
        levels = b,
        xaxs="i",
        yaxs="i",
        add = TRUE,
        xpd = TRUE)

#Label B's indifference curves
text(9.1, 4.7, expression(u[1]^B))
text(9.1, 6.6, expression(u[2]^B))
#text(9.1, 4.6, expression(u[3]^B))
text(9.1, 8.1, expression(u[3]^B))

#Add a point for the initial endowment
points(2, 13, pch = 16, col = "black", cex = 1.5)
text(1.9, 12.7, expression(z))

#Label a point on the middle of the curve
points(10-6.66, 15-5, pch = 16, col = "black", cex = 1.5)
text(5, 7, expression(i))

#Add point for comparison to participation constraint
points(6.2, 1.55, pch = 16, col = "black", cex = 1.5)
text(6.1, 1.2, expression(d))


#Calculate TIOLI power allocation for B
#mrs(x,y) => pareto efficient curve is (3/2)x = y
#A's initital utility is 4 = u(8,2) = (8^0.5)*(2^0.5)
#Therefore substitute in the contract curve u_e^A = 4 = (x^A)^(0.5)(3/2*x^A)^0.5
# => x^A = 4/((3/2)^0.5) = 3.27 => x^B = 6.73
# => y^A = 3/2(x^A) = 4.9 => y^B = 10.1

#Add point g for B's TIOLI power
points(4.25, 11.2, pch = 16, col = "black", cex = 1.5)
text(4.25, 11.7, expression(g))

#Calculate TIOLI power allocation for A
#mrs(x,y) => pareto efficient curve is (3/2)x = y
#A's initital utility is 4 = u(8,2) = (8^0.5)*(2^0.5)
#Therefore substitute in the contract curve u_e^A = 4 = (x^A)^(0.5)(3/2*x^A)^0.5
# => x^B = 5.09/((3/2)^0.5) = 4.16 => x^A = 5.84
# => y^B = 3/2(x^B) = 6.23 => y^A = 8.77 
#=> u^A = (5.84^0.5)*(8.77^0.5) = 7.156591
#Add point f for A's TIOLI power
# points(3.35, 10, pch = 16, col = "black", cex = 1.5)
# text(3.35, 9.5, expression(f))

#Annotating B's endowment
#text(1.8, 12.5, expression(e))

#Annotating a point that is a Pareto improvement over e.
points(3, 12.4, pch = 16, col = "black", cex = 1.5)
text(3, 12, expression(h))
#(2.94^0.5)*(12.76^0.5)


dev.off()



Welfare <- function(xA, yA){
  (xA)^(2/3)*(yA)^(1/3) + (10 - xA)^(1/3)*(15 - yA)^(2/3)
}

Welfare(6.66, 5)


