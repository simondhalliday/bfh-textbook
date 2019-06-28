require(shape)
#pdf(file = "property_tioli_asymmetric_logsSTEP1.pdf", width = 9, height = 7)
#pdf(file = "property_tioli_asymmetric_logsSTEP2.pdf", width = 9, height = 7)
#pdf(file = "property_tioli_asymmetric_logsSTEP3.pdf", width = 9, height = 7)
#pdf(file = "property_tioli_asymmetric_logsSTEP4.pdf", width = 9, height = 7)
pdf(file = "property/property_tioli_asymmetric_logs.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 3
namesize <- 1.3

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")


uAlog <- function(xA, yA, alpha = 2/3){
  alpha*log(xA) + (1-alpha)*log(yA)
}
  
indifflogA <- function(xA, U, alpha = 2/3){
  exp((U - alpha*log(xA))/(1 - alpha))
}

uBlog <- function(xA, yA, alpha = 1/3){
  alpha*log(10-xA) + (1-alpha)*log(15-yA)
}

indifflogB <- function(xA, U = uBlog(9,1), alpha = 1/3){
  15 - exp((U - alpha*log(10-xA))/(1 - alpha))
}

paretoEC <- function(x, slope1 = 15, slope2 = 3, int = 40) {
  (slope1*x)/(int - slope2*x)
}

#Aisha happens to have found 8 apples and 2 oranges, 
#and Betty happens to have found 2 apples and 13 oranges. 
#Aisha's utility (8^0.5)*(2^0.5) = 4
#Betty's utility (2^0.5)*(13^0.5) = 5.09


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

mtext(expression(paste("A's coffee (kilograms), ", x^A)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.8, 0.5*ylims[2], expression(paste("A's data (gigabytes), ", y^A)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
yy1 <- indiffcurveA2(xx1, U = uA(8,2), A = 1, a = 2/3)
yy2 <- indiffcurveA2(xx1)

#Polygon Attempt
#polygon(x = c(1.34, 6, 8, 10 - 6.73), y = c(12, 9, 2, 15 - 10.1), col="powderblue", density=NULL, border = NA)

#I need something like xx1 with npts for 
xpoly1 <- seq(from = 2.85, to = 9, length.out = 501)
ypoly1 <- indifflogA(xpoly1, U = uAlog(9,1), alpha = 2/3)
ypoly2 <- indifflogB(xpoly1, U = uBlog(9,1), alpha = 1/3)
polygon(x = c(xpoly1, rev(xpoly1)), y = c(ypoly1, rev(ypoly2)), col=COL[4], density=NULL, border = NA)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(uA(9,2) - 1.5, uA(8,2), uA(8,2) + 1)

a2 <- c(uAlog(9,1) - 0.4, uAlog(9,1), uAlog(9,1) + 0.485)
  
contour(x, y, 
        outer(x, y, uAlog),
        drawlabels = FALSE,
        col = COLA[3],
        lwd = graphlinewidth,
        levels = a2, 
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

#lines(xx1, indifflogB(xx1), col = COLB[2], lwd = graphlinewidth)
#lines(xx1, indiffcurveBneg2(xx1, U = uB(8,2), a = 1/3), col = COLB[2], lwd = graphlinewidth)
#lines(xx1, indiffcurveBneg3(xx1), col = COLB[2], lwd = graphlinewidth)

#Customize ticks and labels for the plot
ticksy <- seq(from = 0, to = 15, by = 1)
ylabels <- seq(from = 0, to = 15, by = 1)
ticksx <- seq(from = 0, to = 10, by = 1)
xlabels <- seq(from = 0, to = 10, by = 1)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0)

#Add arrows:
arrows(-0.75, 11.5, -0.75, 14, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)
arrows(6.9, -1.6, 9, -1.6, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)

#Annotation of the three graphs and the NE
text(1.5, 14, expression(u[1]^A))
text(2.9, 14, expression(u[2]^A==u[z]^A))
text(5.2, 14, expression(u[3]^A))

#Perhaps useful point to label the unused intersection of the participation constraints
#points(1.34, 12, pch = 16, col = "black", cex = 1.5)



#Pareto efficient curve
#segments(4.16, 6.23, 6.73, 10.1, lty = 2, lwd = 2)
text(8.8, 13.5, expression("Pareto-efficient"))
text(8.8, 13, expression("curve"))
Arrows(8.8, 12.7, 8.8, 10.4, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#Label Pareto Improving Lens
text(4, 11, expression(paste("Pareto-improving")))
text(4, 10.5, expression(paste("lens")))
Arrows(4, 10.2, 4, 8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)



text(-0.5, -1.4, expression("Ayanda"), xpd = TRUE, cex = namesize, col = COLA[4])
text(10.4, 16.4, expression("Bongani"), xpd = TRUE, cex = namesize, col = COLB[4])

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
arrows(6.9, -1.5, 9, -1.5, xpd = TRUE, length=0.1,angle=40,lwd=3)


uB2 <- function(xB, yB, alpha = 1/3){
  ((xB)^alpha)*((yB)^(1-alpha))
}

uBlog2 <- function(xB, yB, alpha = 1/3){
  alpha*log(xB) + (1-alpha)*log(yB)
}

#b <- c(uB2(1,14) - 1, uB2(1,14), uB2(1,14) + 1)
b <- c(uBlog2(1,14) - 0.4, uBlog2(1,14), uBlog2(1,14) + 0.42)
contour(x, y,
        outer(x, y, uBlog2),
        drawlabels = FALSE,
        col = COLB[3],
        lwd = graphlinewidth,
        levels = b,
        xaxs="i",
        yaxs="i",
        add = TRUE,
        xpd = TRUE)

#Label B's indifference curves
text(9.2, 3, expression(u[1]^B))
text(9.2, 5.2, expression(u[2]^B == u[z]^B))
#text(9.1, 4.6, expression(u[3]^B))
text(9.2, 9.2, expression(u[3]^B))

#Add a point for the initial endowment
points(1, 14, pch = 16, col = "black", cex = 1.5)
text(0.9, 13.7, expression(z))

#Label a point on the middle of the curve
points(10-6.66, 15-5, pch = 16, col = "black", cex = 1.5)
text(10-6.66, 15-5 + 0.4, expression(i))

#Add point for comparison to participation constraint
points(10-2.9, 15 - 9.75, pch = 16, col = "black", cex = 1.5)
text(10-2.9 + 0.1, 15 - 9.75 + 0.4, expression(d))


#Calculate TIOLI power allocation for B
#mrs(x,y) => pareto efficient curve is (3/2)x = y
#A's initital utility is 4 = u(8,2) = (8^0.5)*(2^0.5)
#Therefore substitute in the contract curve u_e^A = 4 = (x^A)^(0.5)(3/2*x^A)^0.5
# => x^A = 4/((3/2)^0.5) = 3.27 => x^B = 6.73
# => y^A = 3/2(x^A) = 4.9 => y^B = 10.1

#Add point g for B's TIOLI power
points(10-5.1, 15-3.1, pch = 16, col = "black", cex = 1.5)
text(10-5.1, 15-3.1 + 0.5, expression(g))

#Calculate TIOLI power allocation for A
#mrs(x,y) => pareto efficient curve is (3/2)x = y
#A's initital utility is 4 = u(8,2) = (8^0.5)*(2^0.5)
#Therefore substitute in the contract curve u_e^A = 4 = (x^A)^(0.5)(3/2*x^A)^0.5
# => x^B = 5.09/((3/2)^0.5) = 4.16 => x^A = 5.84
# => y^B = 3/2(x^B) = 6.23 => y^A = 8.77 
#=> u^A = (5.84^0.5)*(8.77^0.5) = 7.156591
#Add point f for A's TIOLI power
points(10-7.45, 15-6.3, pch = 16, col = "black", cex = 1.5)
text(10-7.45, 15-6.3 - 0.4, expression(f))

#Annotating B's endowment
#text(1.8, 12.5, expression(e))

#Annotating a point that is a Pareto improvement over e.
indifflogAneg <- function(xA, U, alpha = 2/3){
  15 - exp((U - alpha*log(10-xA))/(1 - alpha))
}

points(3, indifflogAneg(3, U = uAlog(9,1)), pch = 16, col = "black", cex = 1.5)
text(3 - 0.15, indifflogAneg(3, U = uAlog(9,1)) - (0.15*1.5), expression(h))

# uAlog(9,1)
# [1] 1.464816
#(2.94^0.5)*(12.76^0.5)


dev.off()



Welfare <- function(xA, yA){
  (xA)^(2/3)*(yA)^(1/3) + (10 - xA)^(1/3)*(15 - yA)^(2/3)
}

Welfare(6.66, 5)


