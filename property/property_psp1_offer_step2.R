#require(ggplot2)
require(shape)
library(pBrackets)
#pdf(file = "property_psp1_offerSTEP1.pdf", width = 9, height = 7)
#pdf(file = "property_psp1_offerSTEP2.pdf", width = 9, height = 7)
#pdf(file = "property_psp1_offerSTEP3.pdf", width = 9, height = 7)
#pdf(file = "property_psp1_offerSTEP4.pdf", width = 9, height = 7)
pdf(file = "property/property_psp1_offer_step2.pdf", width = 9, height = 7)

#Set parameters for graphics
pointsize <- 1.8
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

PriceLine <- function(x, intercept = 10, slope = 1) {
  intercept - slope*x 
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


par(mar =  c(4, 4, 4, 4))
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


npts <- 500 
npts2 <- 501

#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
yy1 <- indiffcurveA2(xx1, U = 4, A = 1, a = 0.5)
yy2 <- indiffcurveA2(xx1)

xx2 <- seq(4, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 9, length.out = npts)

xpolyF <- c(2.78, 10, 10, 8.8, 2.78)
ypolyF1 <- c(15, 15, 0, 0, 15)
polygon(xpolyF, ypolyF1, border = FALSE, col = COLB[1])


#Draw the lines for the graphs
#lines(xx1, mrsplot(xx1), col = COL[1], lwd = graphlinewidth)
lines(xx1, indiffcurveBneg1(xx1), col = COLB[3], lwd = graphlinewidth)
lines(xx1, indiffcurveBneg2(xx1), col = COLB[3], lwd = graphlinewidth)
#lines(xx1, indiffcurveBneg2(xx1, U = 6.4), col = COLB[2], lwd = graphlinewidth)
#lines(xx1, indiffcurveBneg2(xx1, U = 7.5), col = COLB[2], lwd = graphlinewidth)

brackets(x1 = 6.3, y1 = 8.1, x2 = 8, y2 = 8.1,  
         ticks = 0.5, curvature = 0.5, type = 1, h = 0.15,
         col = "black", lwd = 2, lty = 1, xpd = TRUE)
text(7.1, 9.8, expression(paste("Coffee")),  xpd = TRUE,  cex = labelsize)
text(7.1, 9, expression(paste("B gets")),  xpd = TRUE,  cex = labelsize)


brackets(x1 = 8.3, y1 = 6, x2 = 8.3, y2 = 2,  
         ticks = 0.5, curvature = 0.5, type = 1, h = 0.15,
         col = "black", lwd = 2, lty = 1, xpd = TRUE)
text(9.2, 4.4, expression(paste("Data B")),  xpd = TRUE,  cex = labelsize)
text(9.2, 3.6, expression(paste("gives up")),  xpd = TRUE,  cex = labelsize)


#lines(xx3, OfferCurveB(xx3), col = COLB[4], lwd = graphlinewidth+1)
lines(xx1, MonopolyPrice(xx1), col = COL[8], lwd = graphlinewidth)
#lines(xx1, PriceLine(xx1), col = COL[8], lwd = graphlinewidth)
#lines(xx1, PriceLine(xx1, intercept = 15, slope = 1.63), col = COL[8], lwd = graphlinewidth)
#lines(xx1, PriceLine(xx1, intercept = 50, slope = 6), col = COL[8], lwd = graphlinewidth)

#Customize ticks and labels for the plot
ticksy <- seq(from = 0, to = 15, by = 1)
ylabels <- seq(from = 0, to = 15, by = 1)
ticksx <- seq(from = 0, to = 10, by = 1)
xlabels <- seq(from = 0, to = 10, by = 1)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0)

#Axis labels
mtext(expression(paste("A's coffee (kilograms), ", x^A)), side=1, line = 3, cex = axislabelsize)
text(-0.8, 7, expression(paste("A's data (gigabytes), ", y^A)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Add arrows:
arrows(-0.75, 11, -0.75, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(7.5, -1.7, 9, -1.7, xpd = TRUE, length=0.1,angle=40,lwd=3)

#Annotation of the three graphs and the NE
text(8, 13, expression("B's feasible set"), cex = annotatesize)
# text(9.6, 2.2, expression(u[1]^A))
# text(9.6, 4.4, expression(u[2]^A))
#text(9.6, 5.9, expression(u[4]^A))

#Perhaps useful point to label the unused intersection of the participation constraints
#points(1.34, 12, pch = 16, col = "black", cex = 1.5)



#Pareto efficiency curve
# segments(3.27, 4.9, 5.84, 8.77, lty = 1, lwd = graphlinewidth, col = COL[2])
# text(4.7, 5, expression("Pareto Efficient Curve"))
# Arrows(4.5, 5.3, 4.5, 6.2, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
#Label B's offer curve
#text(1, 3, expression("B's Offer Curve"))
 #Arrows(1, 3.3, 1, 7.2, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#First Price Line
# text(6.8, 13.5, expression(paste("Price Line")))
# text(6.8, 13, expression(paste("Slope", phantom()==-p[1])))

#Second price line
text(4.8, 13.5, expression(paste("Budget constraint")), cex = annotatesize)
text(4.8, 12.75, expression(paste("slope", phantom()==-p[2])), cex = annotatesize)

#Third price line
# text(2, 13.5, expression(paste("Price Line")))
# text(2, 13, expression(paste("Slope", phantom()==-p[3])))


# Arrows(6.3, 13, 3.8, 13, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#  text(8, 13.5, expression(paste("Different price lines")))
# text(8, 13, expression(paste("going through the endowment")))
#  text(8, 12.5, expression(paste("and tangent to B's ICs")))
#  text(8, 12, expression(paste("form the offer curve")))
# text(7, 13, expression(paste("Slope", phantom()==-p[m])))
# Arrows(6.3, 13, 3.8, 13, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Add a point for the initial endowment
points(8, OfferCurveB(x = 8), pch = 16, col = "black", cex = 1.5)
text(8+0.2, OfferCurveB(x = 8) + 0.2, expression(z), cex = labelsize)


#Add a point for the initial endowment
points(6.35, OfferCurveB(x = 6.35), pch = 16, col = "black", cex = 1.5)
text(6.35 + 0.3, OfferCurveB(x = 6.35) + 0.3, expression(b[2]), cex = labelsize)

#Add a point for the initial endowment
# points(5, OfferCurveB(x = 5), pch = 16, col = "black", cex = 1.5)
#text(5 + 0.2, OfferCurveB(x = 5) + 0.3, expression(b[3]))

#Add a point for the initial endowment
#points(2.5, OfferCurveB(x = 2.5), pch = 16, col = "black", cex = 1.5)
# text(2.5 + 0.2, OfferCurveB(x = 2.5) + 0.3, expression(b[4]))


text(-0.3, -1.7, expression("Ayanda"), xpd = TRUE, cex = namesize, col = COLA[4])
text(10.4, 16.4, expression("Biko"), xpd = TRUE, cex = namesize, col = COLB[4])

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

text(5, -1.5, expression(paste("B's coffee (kilograms), ", x^B)), xpd = TRUE, cex = axislabelsize)
text(-0.8, 7, expression(paste("B's data (gigabytes), ", y^B)), xpd = TRUE, cex = axislabelsize, srt = 270) 

#Add arrows:
arrows(-0.75, 11, -0.75, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(7.4, -1.5, 9, -1.5, xpd = TRUE, length=0.1,angle=40,lwd=3)


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

indiffcurveB5 <- function(x, U = 9, A = 1, a = 0.5) {
  ((((U)/A)*(1/x)^a)^(1/(1-a)))
}



#lines(xx1, indiffcurveB1(xx1), col = COLB[3], lwd = graphlinewidth)
#lines(xx1, indiffcurveB2(xx1), col = COLB[3], lwd = graphlinewidth)
#lines(xx1, indiffcurveB3(xx1), col = COLB[3], lwd = graphlinewidth)
#lines(xx1, indiffcurveB4(xx1), col = COLB[3], lwd = graphlinewidth)

#Label B's indifference curves
text(9.1, 2.4, expression(u[1]^B), cex = labelsize)
text(9.1, 4.1, expression(u[2]^B), cex = labelsize)
#text(9.1, 5, expression(u[3]^B))
 #text(9.1, 6.7, expression(u[4]^B))








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
# points(6.73, 10.1, pch = 16, col = "black", cex = 1.5)
# text(6.73, 10.6, expression(g))

#Calculate TIOLI power allocation for A
#mrs(x,y) => pareto efficient curve is (3/2)x = y
#A's initital utility is 4 = u(8,2) = (8^0.5)*(2^0.5)
#Therefore substitute in the contract curve u_e^A = 4 = (x^A)^(0.5)(3/2*x^A)^0.5
# => x^B = 5.09/((3/2)^0.5) = 4.16 => x^A = 5.84
# => y^B = 3/2(x^B) = 6.23 => y^A = 8.77 
#=> u^A = (5.84^0.5)*(8.77^0.5) = 7.156591
#Add point f for A's TIOLI power
# points(4.16, 6.23, pch = 16, col = "black", cex = 1.5)
# text(4.16, 5.8, expression(f))


#Annotating B's endowment
# text(1.8, 12.5, expression(e))

#Annotating a point that is a Pareto improvement over e.
# points(3.623424, 8.977679, pch = 16, col = "black", cex = 1.5)
# text(3.5, 8.6, expression(m))
#(2.94^0.5)*(12.76^0.5)

#Label Pareto Improving Lens
#text(7, 5, expression(paste("Pareto-Improving Lens")))

dev.off()

