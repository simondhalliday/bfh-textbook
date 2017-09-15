#require(ggplot2)
require(shape)
pdf(file = "bfh-textbook/property/property_psp1.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 3

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")


indiffcurveA1 <- function(x, U = 4, A = 1, a = 0.5) {
  ((((U-2)/A)*(1/x)^a)^(1/(1-a)))
}

indiffcurveA2 <- function(x, U = 4, A = 1, a = 0.5) {
  (((U/A)*(1/x)^a)^(1/(1-a)))
}

indiffcurveA3 <- function(x, U = 6.196918, A = 1, a = 0.5) {
  (((U/A)*(1/x)^a)^(1/(1-a)))
}

indiffcurveA4 <- function(x, U = 6.984164, A = 1, a = 0.5) {
  ((((U)/A)*(1/x)^a)^(1/(1-a)))
}

paretoEC <- function(x) {
  (3/2)*x
}

MonopolyPrice <- function(x) {
  21.82142 - 2.477677*x 
}

OfferCurveA <- function(x) {
  x/(x  - 4)
}

mrsplot <- function(x) {
  (13/2)*(x/(9-x)^2)
}

OfferCurveB <- function(x) {
  15 - (13/2)*(10 - x)/(9 - x)
}

#Deriving the offer curves: let py = 1
#wA = 8px + 2py = 8px + 2; Demand: x = w/2px => x = 4 + 1/px => px = 1/(x - 4)
#Offer curve is y = px*x => (1/(x-4))*x = x/(x-4) defined for x neq 4
#wA = 2px + 13py = 2px + 13; Demand: x = w/2px => x = 1 + 13/2px => px = 13/2(x - 1)
#B's Offer curve is y = px*x => (13/2)(1/(x-1))*x = (13/2)(x/(x-1)
#Therefore in terms of yA and xA:
#15 - y = (13/2)*(10 - x)/(10 - x - 1)
#Therefore y = 15 - (13/2)*(10-x)/(9 - x) defined for x neq 9
#The tangency of the indifference curve and the offer curve gives mrsplot
#Where mrsplot equals that offer curve, we get x = (3/17)*(51 - sqrt(221)) = 6.376576; 
#So y = 6.022321; therefore u = (6.376576^0.5)*(6.022321^0.5)

indiffcurveBneg1 <- function(x, U = 5.09, A = 1, a = 0.5) {
  15 - (((U/A)*(1/(10 - x))^a)^(1/(1-a)))
}

indiffcurveBneg2 <- function(x, U = 5.703502, A = 1, a = 0.5) {
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
     xlab = expression(paste("A's Apples, ", x)),
     ylab = expression(paste("A's Oranges, ", y)), 
     line = 2.5,
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")


npts <- 500 
npts2 <- 501

#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
yy1 <- indiffcurveA2(xx1, U = 4, A = 1, a = 0.5)
yy2 <- indiffcurveA2(xx1)

#Polygon Attempt
#polygon(x = c(1.34, 6, 8, 10 - 6.73), y = c(12, 9, 2, 15 - 10.1), col="powderblue", density=NULL, border = NA)

#I need something like xx1 with npts for 
#xpoly1 <- seq(from = 1.34, to = 8, length.out = 500)
#ypoly1 <- indiffcurveA2(xpoly1, U = 4, A = 1, a = 0.5)
#ypoly2 <- indiffcurveBneg(xpoly1, U = 5.09, A = 1, a = 0.5)
#polygon(x = c(xpoly1, rev(xpoly1)), y = c(ypoly1, rev(ypoly2)), col=COL[3], density=NULL, border = NA)
xx2 <- seq(4, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 9, length.out = npts)

#Draw the lines for the graphs
#lines(xx1, indiffcurveA1(xx1), col = COLA[3], lwd = graphlinewidth)
lines(xx1, indiffcurveA2(xx1), col = COLA[3], lwd = graphlinewidth)
lines(xx1, indiffcurveA3(xx1), col = COLA[3], lwd = graphlinewidth)
#lines(xx1, indiffcurveA4(xx1), col = COLA[3], lwd = graphlinewidth)
#lines(xx1, paretoEC(xx1), col = COL[2], lwd = graphlinewidth)
#lines(xx2, OfferCurveA(xx2), col = COL[2], lwd = graphlinewidth)

#lines(xx1, mrsplot(xx1), col = COL[1], lwd = graphlinewidth)
lines(xx1, indiffcurveBneg1(xx1), col = COLB[2], lwd = graphlinewidth)
lines(xx1, indiffcurveBneg2(xx1), col = COLB[2], lwd = graphlinewidth)

lines(xx3, OfferCurveB(xx3), col = COLB[4], lwd = graphlinewidth+1)
lines(xx1, MonopolyPrice(xx1), col = COL[8], lwd = graphlinewidth)

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
#text(9.6, 0.9, expression(u[1]^A))
text(9.6, 2.2, expression(u[1]^A))
text(9.6, 4.4, expression(u[2]^A))
#text(9.6, 5.9, expression(u[4]^A))

#Perhaps useful point to label the unused intersection of the participation constraints
#points(1.34, 12, pch = 16, col = "black", cex = 1.5)



#Pareto efficiency curve
segments(3.27, 4.9, 5.84, 8.77, lty = 1, lwd = graphlinewidth, col = COL[2])
text(4.7, 5, expression("Pareto Efficient Curve"))
Arrows(4.5, 5.3, 4.5, 6.2, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label B's offer curve
text(1, 3, expression("B's Offer Curve"))
Arrows(1, 3.3, 1, 7.2, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Monopoly Price Line
text(7, 13.5, expression(paste("Monopoly Price Line")))
text(7, 13, expression(paste("Slope", phantom()==-p[m])))
Arrows(6.3, 13, 3.8, 13, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

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
mtext("B's Apples, x", side=3, line = 2.5, cex = axislabelsize)
text(-0.8, 7, expression(paste("B's Oranges, y")), xpd = TRUE, cex = axislabelsize, srt = 270) 

#Add arrows:
arrows(-0.8, 10, -0.8, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(6.2, -1.8, 9, -1.8, xpd = TRUE, length=0.1,angle=40,lwd=3)


#Functions for B's indifference curves
indiffcurveB1 <- function(x, U = 5.09, A = 1, a = 0.5) {
  ((((U-2)/A)*(1/x)^a)^(1/(1-a)))
}

indiffcurveB2 <- function(x, U = 5.09, A = 1, a = 0.5) {
  (((U/A)*(1/x)^a)^(1/(1-a)))
}

indiffcurveB3 <- function(x, U = 5.703502, A = 1, a = 0.5) {
  ((((U)/A)*(1/x)^a)^(1/(1-a)))
}

indiffcurveB4 <- function(x, U = 8.244574, A = 1, a = 0.5) {
  ((((U)/A)*(1/x)^a)^(1/(1-a)))
}




#lines(xx1, indiffcurveB1(xx1), col = COLB[3], lwd = graphlinewidth)
#lines(xx1, indiffcurveB2(xx1), col = COLB[3], lwd = graphlinewidth)
#lines(xx1, indiffcurveB3(xx1), col = COLB[3], lwd = graphlinewidth)
#lines(xx1, indiffcurveB4(xx1), col = COLB[3], lwd = graphlinewidth)

#Label B's indifference curves
text(9.1, 2.4, expression(u[1]^B))
text(9.1, 4.1, expression(u[2]^B))
#text(9.1, 4.6, expression(u[3]^B))
#text(9.1, 8.2, expression(u[4]^B))

#Add a point for the initial endowment
points(2, 13, pch = 16, col = "black", cex = 1.5)
text(1.8, 12.5, expression(e))




#Label a point on the middle of the curve
#points(5, 7.5, pch = 16, col = "black", cex = 1.5)
#text(5, 7, expression(i))

#(5^0.5)*(7.5^0.5) = 6.123724

#Add point for comparison to participation constraint
#points(6.2, 1.55, pch = 16, col = "black", cex = 1.5)
#text(6.1, 1.2, expression(d))


#Calculate TIOLI power allocation for B
#mrs(x,y) => pareto efficient curve is (3/2)x = y
#A's initital utility is 4 = u(8,2) = (8^0.5)*(2^0.5)
#Therefore substitute in the contract curve u_e^A = 4 = (x^A)^(0.5)(3/2*x^A)^0.5
# => x^A = 4/((3/2)^0.5) = 3.27 => x^B = 6.73
# => y^A = 3/2(x^A) = 4.9 => y^B = 10.1

#Add point g for B's TIOLI power
points(6.73, 10.1, pch = 16, col = "black", cex = 1.5)
text(6.73, 10.6, expression(g))

#Calculate TIOLI power allocation for A
#mrs(x,y) => pareto efficient curve is (3/2)x = y
#A's initital utility is 4 = u(8,2) = (8^0.5)*(2^0.5)
#Therefore substitute in the contract curve u_e^A = 4 = (x^A)^(0.5)(3/2*x^A)^0.5
# => x^B = 5.09/((3/2)^0.5) = 4.16 => x^A = 5.84
# => y^B = 3/2(x^B) = 6.23 => y^A = 8.77 
#=> u^A = (5.84^0.5)*(8.77^0.5) = 7.156591
#Add point f for A's TIOLI power
points(4.16, 6.23, pch = 16, col = "black", cex = 1.5)
text(4.16, 5.8, expression(f))


#Annotating B's endowment
text(1.8, 12.5, expression(e))

#Annotating a point that is a Pareto improvement over e.
points(3.623424, 8.977679, pch = 16, col = "black", cex = 1.5)
text(3.5, 8.6, expression(m))
#(2.94^0.5)*(12.76^0.5)

#Label Pareto Improving Lens
#text(7, 5, expression(paste("Pareto-Improving Lens")))

dev.off()

