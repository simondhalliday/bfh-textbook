#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
pdf(file = "bfh-textbook/constrained_optimization/ff_hg.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1

indiffcurveHG <- function(x, v = 10, lambda = 1) {
  (v) - lambda * x
}


upf <- function(ub, y = 80){
  sqrt(y - (ub)^2)
}

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
par(mar =  c(6, 5, 4, 4))
xlims <- c(0, 15)
ylims <- c(0, 15)

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
xx2 <- seq(0, 8.944271, length.out = npts2)

#Draw the lines for the graphs
lines(xx1, indiffcurveHG(xx1, v = 10 - 4, lambda = 0.5), col = COLB[2], lwd = graphlinewidth)
lines(xx1, indiffcurveHG(xx1, v = 10, lambda = 0.5), col = COLB[2], lwd = graphlinewidth)
lines(xx1, indiffcurveHG(xx1, v = 10 + 4, lambda = 0.5), col = COLB[2], lwd = graphlinewidth)
lines(xx2, upf(xx2, y = 80), col = COLA[3], lwd = graphlinewidth)

#Customize ticks and labels for the plot
# ticksy <- c(ylims[1], 10 - 4, 10, 10 + 4, ylims[2])
# ylabels <- c(NA, expression(paste(v[1]^A == u[1]^A)), expression(paste(v[2]^A == u[2]^A)), expression(paste(v[3]^A == u[3]^A)), NA)
# ticksx <- c(xlims[1], 20 - 8, 20, 20 + 8, xlims[2])
# xlabels <- c(NA, expression(paste(v[1]^A == u[1]^B)), expression(paste(v[2]^A == u[2]^B)), expression(paste(v[3]^A == u[3]^B)), NA)


# 
ticksy <- seq(from = ylims[1], to = ylims[2], by = 2)
ylabels <- seq(from = ylims[1], to = ylims[2], by = 2)
ticksx <- c(seq(from = xlims[1], to = xlims[2], by = 2), 15)
xlabels <- c(seq(from = xlims[1], to = xlims[2], by = 2), NA)

#Set up axes based on ticks and labels
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

mtext(expression(paste("B's Utility, ", u^B)), side=1, line = 2.5, cex = axislabelsize)
text(-1.7, 0.5*(ylims[2]), expression(paste("A's Utility, ", u^A)), xpd = TRUE, cex = axislabelsize, srt = 90) 



#Add arrows:
#arrows(-0.9, 10.5, -0.9, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
#arrows(6.2, -1.6, 9, -1.6, xpd = TRUE, length=0.1,angle=40,lwd=3)

#Annotation of the three graphs
text(9.8, .6, expression(v[1]^A))
text(9.8, 4.6, expression(v[2]^A))
text(9.8, 8.6, expression(v[3]^A))


#Lines to label 2 points
# segments(0, 8, 2, 8, lty = 2, col = "darkgray", lwd = 2)
# segments(2, 0, 2, 10, lty = 2, col = "darkgray", lwd = 2)
# segments(0, 4, 6, 4, lty = 2, col = "darkgray", lwd = 2)
# segments(6, 0, 6, 10, lty = 2, col = "darkgray", lwd = 2)

#Annotate point of tangency
#c and d
# points(2, 8, pch = 16, col = "black", cex = 1.5)
text(4.2, 8.2, expression(i))
segments(0, 8, 4, 8, lty = 2, col = "darkgray", lwd = 2)
segments(4, 0, 4, 8, lty = 2, col = "darkgray", lwd = 2)
points(4, 8, pch = 16, col = "black", cex = 1.5)
# segments(0, 4, 6, 4, lty = 2, col = "darkgray", lwd = 2)
# segments(6, 0, 6, 10, lty = 2, col = "darkgray", lwd = 2)
# text(6.4, 4.4, expression(d))
# 
# #f and g
# points(2, 10, pch = 16, col = "black", cex = 1.5)
# text(2.4, 10.4, expression(f))
# points(6, 10, pch = 16, col = "black", cex = 1.5)
# text(6.4, 10.4, expression(g))

#Mention lambda
text(12, 12.5, expression("A's iso-value curves"), cex = axislabelsize)
text(12, 11.5, expression(paste("for ", lambda == 0.5)), cex = axislabelsize)

#Label feasible frontier/upf
text(5.6, 5.3, expression("Feasible Frontier"), cex = labelsize)
text(5.6, 4.7, expression("(utility possibilities"), cex = labelsize)
text(5.6, 4.1, expression("frontier, upf)"), cex = labelsize)
Arrows(5.6, 5.6, 5.6, 6.6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label mrs = mrt
text(4, 10, expression(paste(mrs(u^A,u^B) == mrt(u^A, u^B))), cex = labelsize)
Arrows(4, 9.8, 4, 8.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


##Scratch spot: 
#-du^A/du^B = - (- u^B/sqrt(80 - (u^B)^2)) = u^B/sqrt(80 - (u^B)^2)
#dy/dx of y = sqrt(80 - (x)^2)
#This must equal the mrs = 0.5
# 0.5 = u^B/sqrt(80 - (u^B)^2)
#which implies u^B = 4, meaning y^B = 16, meaning y^A = 64 and u^A = 8

dev.off()

