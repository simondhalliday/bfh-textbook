require(shape)
#pdf(file = "property_hg_he_powertestSTEP2.pdf", width = 9, height = 7)
#pdf(file = "property_hg_he_powertestSTEP3.pdf", width = 9, height = 7)

pdf(file = "property/property_hg_he_powertest.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")
grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")

par(mar =  c(4, 4.5, 4, 4.5))

uA <- function(x, y, alpha = 0.5, lambda = 0.4) {
  (x^(alpha)*y^(1 - alpha))^(1-lambda)*((10 - x)^(alpha)*(15 - y)^(1 - alpha))^lambda
}

uB <- function(x, y) {
  (10 - x)^(0.5)*(15 - y)^(0.5)
}

xlims <- c(0, 10)
ylims <- c(0, 15)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 



#For B we need a utility where xA = 5.88, ya = 8.82 => corresponds to 1.618 above

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

ticksy <- seq(from = 0, to = 15, by = 1)
ylabels <- seq(from = 0, to = 15, by = 1)
ticksx <- seq(from = 0, to = 10, by = 1)
xlabels <- seq(from = 0, to = 10, by = 1)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#a <- c(uA(9,1) - 1, uA(9,1), uA(9,1) + 2, uA(9,1), uA(9,1) + 3, uA(9,1) + 4, uA(9,1) + 5, uA(9,1) + 6, uA(9,1) + 6.6,  uA(9,1) + 7)
a <- c(uA(9,1), uA(9,1) + 1.5, uA(9,1) + 2.5,  uA(9,1) + 2.9)

contour(x, y, 
        outer(x, y, uA),
        drawlabels = FALSE,
        col = CBCols[1],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE) 

#mtext(expression(paste("A's coffee (kilograms), ", x^A)), side=1, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], -1.6, expression(paste("A's coffee (kilograms), ", x^A)), xpd = TRUE, cex = axislabelsize) 
text(-0.95, 7, expression(paste("A's data (gigabytes), ", y^A)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Add arrows:
Arrows(-0.95, 11.8, -0.95, 14, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5, xpd = TRUE)
Arrows(7.4, -1.6, 9, -1.6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5, xpd = TRUE)

# arrows(-0.75, 11.5, -0.75, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
# arrows(7.5, -1.5, 9, -1.5, xpd = TRUE, length=0.1,angle=40,lwd=3)

segments(0, 0, 6, 9, lty = 1, col = CBCols[4] , lwd = graphlinewidth)
text(3.9, 3.8, expression("Pareto-efficient"), cex = labelsize)
text(4, 3.2, expression("Curve"), cex = labelsize)
Arrows(3.8, 4, 3.8, 5.2, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the iso-welfare functions for the HG, Aisha
text(9.7, 9, expression(u[1]^A), cex = annotatesize)
text(9.25, 9, expression(u[2]^A), cex = annotatesize)
text(8.15, 9, expression(u[3]^A), cex = annotatesize)
text(6.75, 9, expression(u[4]^A), cex = annotatesize)

#Label the indifference curves for the HE, Betty
text(1.3, 12.8, expression(u[1]^B), cex = annotatesize)
text(1.3, 11.6, expression(u[2]^B), cex = annotatesize)
text(1.3, 10.5, expression(u[3]^B), cex = annotatesize)
text(1.3, 8.2, expression(u[4]^B), cex = annotatesize)
text(1.3, 0.9, expression(u[5]^B), cex = annotatesize)

#A's bliss point - 3.1073 is the value of u
# points(5.88, 8.82, pch = 16, col = "black", cex = 1.5)
text(6, 13.5, expression(paste("A's highest utility")), cex = labelsize)
Arrows(6, 13, 6, 9.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label point i. 
points(5, 7.5, pch = 16, col = "black", cex = 1.5)
text(5, 7, expression(paste(i)), cex = annotatesize)
# 

text(-0.35, -1.4, expression("Ayanda"), xpd = TRUE, cex = namesize, col = CBCols[1])
text(10.5, 16.4, expression("Biko"), xpd = TRUE, cex = namesize, col = CBCols[2])

#Add new plot
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

uB <- function(x, y, alpha = 0.5, lambda = 0) {
  (x^(alpha)*y^(1 - alpha))^(1-lambda)*((10 - x)^(alpha)*(15 - y)^(1 - alpha))^lambda
}

b <- c(uB(1,14), uB(1,14) + 1.16, uB(1,14) + 2.08, uB(1,14) + 3.58, uB(1,14) + 7.03)

contour(x, y, 
        outer(x, y, uB),
        drawlabels = FALSE,
        col = CBCols[2],
        lwd = graphlinewidth,
        levels = b, 
        add = TRUE
) 


# #Label two points for comparison
points(1, 14, pch = 16, col = "black", cex = 1.5)
text(1.2, 13.8, expression(paste(z)), cex = annotatesize)

# #Label two points for comparison
points(0.4*10, 0.4*15, pch = 16, col = "black", cex = 1.5)
text(0.4*10-0.2, 0.4*15-0.2,expression(paste(v)), cex = annotatesize)



points(8.8, (3/2)*8.8, pch = 16, col = "black", cex = 1.5)
text(8.8, (3/2)*8.8 - 0.5, expression(paste(j)), cex = annotatesize)

points(0.4, (3/2)*0.4, pch = 16, col = "black", cex = 1.5)
text(0.4 + 0.2, (3/2)*0.4 + 0.2, expression(paste(k)), cex = annotatesize)

#Set up axes at sides 3 and 4 (top and right)
axis(side = 3, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(side = 4, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)
text(5, -1.6, expression(paste("B's coffee (kilograms), ", x^B)), xpd = TRUE, cex = axislabelsize)
text(-0.95, 7, expression(paste("B's data (gigabytes), ", y^B)), xpd = TRUE, cex = axislabelsize, srt = 270) 

#Add arrows:
Arrows(-0.95, 11.8, -0.95, 14, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5, xpd = TRUE)
Arrows(7.4, -1.6, 9, -1.6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5, xpd = TRUE)

# arrows(-0.8, 11.5, -0.8, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
# arrows(7.5, -1.5, 9, -1.5, xpd = TRUE, length=0.1,angle=40,lwd=3)


dev.off()
