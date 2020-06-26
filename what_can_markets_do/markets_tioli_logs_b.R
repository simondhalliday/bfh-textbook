#' Graph Designer: Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library("shape")

pdf(file = "what_can_markets_do/markets_tioli_logs_b.pdf", width = 9, height = 7)

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


uAlog <- function(xA, yA, alpha = 1/2){
  alpha*log(xA) + (1-alpha)*log(yA)
}

indifflogA <- function(xA, U, alpha = 1/2){
  exp((U - alpha*log(xA))/(1 - alpha))
}

indiffcurveA2 <- function(x, U = 4, A = 1, a = 0.5) {
  (((U/A)*(1/x)^a)^(1/(1-a)))
}

uBlog <- function(xA, yA, alpha = 1/2){
  alpha*log(10-xA) + (1-alpha)*log(15-yA)
}

indifflogB <- function(xA, U = uBlog(9,1), alpha = 1/2){
  15 - exp((U - alpha*log(10-xA))/(1 - alpha))
}

paretoEC <- function(x, ybar = 15, xbar = 10) {
  (ybar/xbar)*x
}

OfferCurveA <- function(x) {
  # x/(x  - 4)
  x / (2*x - 9)
}

OfferCurveB <- function(x) {
  #15 - (14/2)*(10 - x)/(9 - x)
  (145 - 16*x) / (19 - 2*x)
}

#Use the TIOLI funciton to find what value of x you should have for point g
xatioli <- function(uza){
  exp(uza - (1/2)*log(3/2))
}

xbtioli <- function(uzb){
  exp(uzb - (1/2)*log(3/2))
}

par(mar =  c(4.5, 4.5, 4.5, 4.5))
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

mtext(expression(paste("A's coffee (kilograms), ", x^A)), side = 1, line = 3, cex = axislabelsize)
text(-0.9, 0.5*ylims[2], expression(paste("A's data (gigabytes), ", y^A)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(4.5, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 9.5, length.out = npts)
yy1 <- indiffcurveA2(xx1, U = uA(8,2), A = 1, a = 2/3)
yy2 <- indiffcurveA2(xx1)

#Polygon Attempt
#polygon(x = c(1.34, 6, 8, 10 - 6.73), y = c(12, 9, 2, 15 - 10.1), col="powderblue", density=NULL, border = NA)

#I need something like xx1 with npts for 
# xpoly1 <- seq(from = 0.7, to = 9, length.out = 501)
# ypoly1 <- indifflogA(xpoly1, U = uAlog(9,1), alpha = 1/2)
# ypoly2 <- indifflogB(xpoly1, U = uBlog(9,1), alpha = 1/2)
# polygon(x = c(xpoly1, rev(xpoly1)), y = c(ypoly1, rev(ypoly2)), col=COL[4], density=NULL, border = NA)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(uA(9,2) - 1.5, uA(8,2), uA(8,2) + 1)

a2 <- c(uAlog(9,1), uAlog(10 - xbtioli(uBlog(9,1)),15 - (3/2)*xbtioli(uBlog(9,1))))

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
lines(xx3, OfferCurveB(xx3), col = COLB[5], lwd = graphlinewidth)
lines(xx2, OfferCurveA(xx2), col = COLA[5], lwd = graphlinewidth)


#Customize ticks and labels for the plot
ticksy <- seq(from = 0, to = 15, by = 1)
ylabels <- seq(from = 0, to = 15, by = 1)
ticksx <- seq(from = 0, to = 10, by = 1)
xlabels <- seq(from = 0, to = 10, by = 1)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, gap.axis = -1, las = 0, cex.axis = labelsize)

#Add arrows: 
arrows(-0.9, 12, -0.9, 14, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)
arrows(7.5, -1.7, 9, -1.7, xpd = TRUE, length = 0.1, angle = 40, lwd = 3)

#Annotation of the three graphs and the NE
#text(0.5, 12, expression(u[1]^A), cex = annotatesize)
#text(1.3, 12, expression(u[2]^A==u[z]^A), cex = annotatesize)
text(1.1, 12, expression(u[z]^A), cex = annotatesize)
text(6.5, 12, expression(u[3]^A), cex = annotatesize)

#Perhaps useful point to label the unused intersection of the participation constraints
#points(1.34, 12, pch = 16, col = "black", cex = 1.5)

text(2.8, 7.0, expression("B's best-response"), cex = annotatesize)
text(2.8, 6.3, expression("function (ICC)"), cex = annotatesize)
#arrows(2, 6.5, 1.5, 6.5, xpd = TRUE, length=0.1,angle=40,lwd=3)

text(3.1, 12.0, expression("A's best-response"), cex = annotatesize)
text(3.1, 11.3, expression("function (ICC)"), cex = annotatesize)
# arrows(7.7, 6.5, 8.2, 6.5, xpd = TRUE, length=0.1,angle=40,lwd=3)



#Pareto efficient curve
#segments(4.16, 6.23, 6.73, 10.1, lty = 2, lwd = 2)
text(8.1, 14.2, expression("Pareto-efficient"), cex = annotatesize)
text(8.1, 13.5, expression("curve"), cex = annotatesize)
#Arrows(8.8, 12.7, 8.8, 10.4, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

# Label actors in corner
text(-0.3, -1.4, expression("A"), xpd = TRUE, cex = namesize, col = COLA[4])
text(10.4, 16.4, expression("B"), xpd = TRUE, cex = namesize, col = COLB[4])

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
     xaxs = "i", 
     yaxs = "i")

#Set up axes at sides 3 and 4 (top and right)
axis(side = 3, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(side = 4, at = ticksy, pos = 0, labels = ylabels, gap.axis = -1, las = 0, cex.axis = labelsize)

text(0.5*xlims[2], -2, expression(paste("B's coffee (kilograms), ", x^B)), xpd = TRUE, cex = axislabelsize)
text(-0.9, 7, expression(paste("B's data (gigabytes), ", y^B)), xpd = TRUE, cex = axislabelsize, srt = 270) 

#Add arrows:
arrows(-0.9, 12, -0.9, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(7.5, -2, 9, -2, xpd = TRUE, length=0.1,angle=40,lwd=3)


uBlog2 <- function(xB, yB, alpha = 1/2){
  alpha*log(xB) + (1-alpha)*log(yB)
}

#b <- c(uB2(1,14) - 1, uB2(1,14), uB2(1,14) + 1)




b <- c(uBlog2(1,14), uBlog2(3,13.4), uBlog2(10 - xatioli(uAlog(9,1)),15 - (3/2)*xatioli(uAlog(9,1))))
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
#text(0.75, 12.5, expression(u[1]^B), cex = annotatesize)
#text(1.7, 12.5, expression(u[2]^B == u[z]^B), cex = annotatesize)
#text(1.4, 12.5, expression(u[z]^B), cex = annotatesize)
text(1.5 - 0.75, 12.5, expression(u[z]^B), cex = annotatesize)
text(3.6 - 0.75, 12.5, expression(u[3]^B), cex = annotatesize)
text(7.3, 12.5, expression(u[4]^B), cex = annotatesize)

#Add a point for the initial endowment
points(1, 14, pch = 16, col = "black", cex = 1.5)
text(0.8, 13.7, expression(z), cex = annotatesize)

#Label a point on the middle of the curve
# points(5, 7.5, pch = 16, col = "black", cex = 1.5)
# text(5, 7.5 - 0.5, expression(i), cex = annotatesize)

#Add point for comparison to participation constraint
# points(10-.7, 15 - 13.5, pch = 16, col = "black", cex = 1.5)
# text(10-.7 - 0.2, 15 - 13.5 + 0.4, 
#      expression(d), cex = annotatesize)


#Calculate TIOLI power allocation for B
#mrs(x,y) => pareto efficient curve is (3/2)x = y
#A's initital utility is 4 = u(8,2) = (8^0.5)*(2^0.5)
#Therefore substitute in the contract curve u_e^A = 4 = (x^A)^(0.5)(3/2*x^A)^0.5
# => x^A = 4/((3/2)^0.5) = 3.27 => x^B = 6.73
# => y^A = 3/2(x^A) = 4.9 => y^B = 10.1



#Add point g for B's TIOLI power
points(10-xatioli(uAlog(9,1)), 15-(3/2)*xatioli(uAlog(9,1)), pch = 16, col = "black", cex = 1.5)
text(10-2.44949, 15-(3/2)*2.44949 - 0.6, 
     expression(t^B), cex = annotatesize)

#Calculate TIOLI power allocation for A
#mrs(x,y) => pareto efficient curve is (3/2)x = y
#A's initital utility is 4 = u(8,2) = (8^0.5)*(2^0.5)
#Therefore substitute in the contract curve u_e^A = 4 = (x^A)^(0.5)(3/2*x^A)^0.5
# => x^B = 5.09/((3/2)^0.5) = 4.16 => x^A = 5.84
# => y^B = 3/2(x^B) = 6.23 => y^A = 8.77 
#=> u^A = (5.84^0.5)*(8.77^0.5) = 7.156591
#Add point f for A's TIOLI power


#xbtioli(uBlog2(1,14))
points(xbtioli(uBlog2(1,14)), 3/2*xbtioli(uBlog2(1,14)), pch = 16, col = "black", cex = 1.5)
text(xbtioli(uBlog2(1,14)), 3/2*xbtioli(uBlog2(1,14)) + 0.7, 
     expression(t^A), cex = annotatesize)

#Annotating B's endowment
#text(1.8, 12.5, expression(e))

#Annotating a point that is a Pareto improvement over e.
indifflogAneg <- function(xA, U, alpha = 1/2){
  15 - exp((U - alpha*log(10-xA))/(1 - alpha))
}

# points(3, indifflogAneg(3, U = uAlog(9,1)), pch = 16, col = "black", cex = 1.5)
# text(3 - 0.2, indifflogAneg(3, U = uAlog(9,1)) - (0.2*1.5), 
#      expression(h), cex = annotatesize)

# uAlog(9,1)
# [1] 1.464816
#(2.94^0.5)*(12.76^0.5)


dev.off()