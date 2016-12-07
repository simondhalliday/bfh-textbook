require(shape)
pdf(file = "bfh-textbook/coordination_failures/coord_responses.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 3

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 4, 4, 4))

uA <- function(ea, eb, alpha = 16, beta = 1/64) {
alpha*(1 - beta*eb)*ea - 0.5*(ea)^2
}

uB <- function(ea, eb, alpha = 16, beta = 1/64) {
  alpha*(1 - beta*ea)*eb - 0.5*(eb)^2
}

brfA <- function(eb, alpha = 16, beta = 1/64) {
  alpha*(1 - beta*eb)
}

brfB <- function(eb, alpha = 16, beta = 1/64) {
  (alpha - eb)/(alpha * beta)
}

brfB2 <- function(ea, alpha = 16, beta = 1/64) {
  alpha(1 - beta*ea)
}

xlims <- c(0, 18)
ylims <- c(0, 18)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
#a <- c(30, 35, 56.88887)
#b <- c(30, 35, 56.88887)
a <- c(75, 81.92, 95)
b <- c(75, 81.92, 95)
#a <- c(120, 150, 300)
#b <- c(120, 150, 300)



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

ticksy <- seq(from = 0, to = ylims[2], by = 2)
ylabels <- seq(from = 0, to = ylims[2], by = 2)
ticksx <- seq(from = 0, to = xlims[2], by = 2)
xlabels <- seq(from = 0, to = xlims[2], by = 2)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
lines(xx1, brfA(xx1, alpha = 16, beta = 1/64), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfB(xx1, alpha = 16, beta = 1/64), col = COLB[4], lwd = graphlinewidth)


#persp(x, y, outer(x, y, u), ticktype="detailed") 
contour(y, x, 
        outer(x, y, uA),
        #labels = c("v1", "v2", "v3"),
        drawlabels = FALSE,
        col = COLA[3],
        #xlab = expression(paste("A's Apples, ", x)),
        #ylab = expression(paste("A's Oranges, ", y)),
        #cex.lab = axislabelsize,
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE) 

mtext("A's effort, ea", side=1, line = 2.5, cex = axislabelsize)
text(-0.8, 7, expression(paste("B's effort, eb")), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Add arrows:
arrows(-0.8, 10, -0.8, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(6.2, -1.7, 9, -1.7, xpd = TRUE, length=0.1,angle=40,lwd=3)


contour(x, y, 
        outer(x, y, uB),
        #labels = c("v1", "v2", "v3"),
        drawlabels = FALSE,
        col = COLB[2],
        #xlab = expression(paste("")),
        #ylab = expression(paste("")),
        lwd = graphlinewidth,
        levels = b, 
        add = TRUE
        #xaxs="i", 
        #yaxs="i"
) 


#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
#text(5.5, 2.4, expression("Pareto Efficient Curve"))
#Arrows(5.3, 2.7, 5.3, 7.6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the iso-welfare functions for the HG, Aisha
#text(9.35, 3.5, expression(v[1]^A))
#text(7.8, 6.4, expression(v[2]^A))
#text(7.4, 7.1, expression(v[3]^A))
#text(6.6, 8.3, expression(v[4]^A))

#Label the indifference curves for the HG, Betty
#text(0.65, 11.8, expression(v[1]^B))
#text(2.2, 8.9, expression(v[2]^B))
#text(2.6, 8.1, expression(v[3]^B))
#text(3.4, 6.9, expression(v[4]^B))

#Label point i. 
# points(5, 7.5, pch = 16, col = "black", cex = 1.5)
# text(5, 7.9, expression(paste("i")))


#points(5.84, 8.77, pch = 16, col = "black", cex = 1.5)

#A's bliss point - 3.1073 is the value of u
# points(x = 5.88, y = 8.82, pch = 16, col = "black", cex = 1.5)
# text(5.88, 10.5, expression(paste("A's highest v, ", v[max]^A)))
# Arrows(5.88, 10.2, 5.88, 9.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#B's bliss point: 
# points(x = 4.11765, y = 6.17647, pch = 16, col = "black", cex = 1.5)
# text(x = 4.11765, y = 4.5, expression(paste("B's highest v, ", v[max]^B)))
# Arrows(4.11765, 4.7, 4.11765, 5.65, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# 


par(new = TRUE)

#Use the same x and ylims as previously, but with locations switched
xlims2 <- c(18, 0)
ylims2 <- c(18, 0)

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
# axis(side = 3, at = ticksx, pos = 0, labels = xlabels)
# axis(side = 4, at = ticksy, pos = 0, labels = ylabels, las = 0)
# mtext("B's Apples, x", side=3, line = 2.5, cex = axislabelsize)
# text(-0.8, 7, expression(paste("B's Oranges, y")), xpd = TRUE, cex = axislabelsize, srt = 270) 
# 
# #Add arrows:
# arrows(-0.8, 10, -0.8, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
# arrows(6.2, -1.8, 9, -1.8, xpd = TRUE, length=0.1,angle=40,lwd=3)
# 

#Customize ticks and labels for the plot
#ticksy <- seq(from = 0, to = 15, by = 1)
#ylabels <- seq(from = 0, to = 15, by = 1)
#ticksx <- seq(from = 0, to = 10, by = 1)
#xlabels <- seq(from = 0, to = 10, by = 1)
#axis(1, at = ticksx, pos = 0, labels = xlabels)
#axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0)

dev.off()
