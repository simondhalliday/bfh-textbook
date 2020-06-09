source("risk/education1.R")
# the second plot
pdf(file = "risk/education2.pdf", width = 9, height = 7)


indiff <- function(delta, u = 2, alpha = 0.2, beta = 2){
        u + alpha * delta^beta
}

# Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 8, 1, 1))

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = "",
     ylab = "",
     xaxt = "n",
     yaxt = "n",
     bty = "n",
     xaxs = "i",
     yaxs = "i"
)

# ticksx <- seq(from = 0, to = xlims[2],by = 2)
# xlabels <- seq(from = 0, to = xlims[2],by = 2)
ticksy <- c(0, y0 - 4, y0, y0 + 2.7, y0 + 4.7, yT, 17, ylims[2])
ylabels <- c(NA, expression(paste(hat(y)[0])), expression(paste(hat(y)[1] == z)), expression(paste(hat(y)[2])), expression(paste(hat(y)[3])), expression(paste(hat(y)[d] == hat(y)[f])), expression(paste(hat(y)[e])), NA)
# ticksy <- seq(from = 0, to = ylims[2], by = 3)
# ylabels <- seq(from = 0, to = ylims[2], by = 3)
ticksx <- c(0, deltaT, 7, ylims[2])
xlabels <- c(NA, expression(paste(Delta[f])), expression(paste(Delta[d] == Delta[e])), NA)

mtext(expression(paste("Risk, ", Delta)), side = 1, line = 2.5, cex = axislabelsize)
text(-1.5, 0.5*ylims[2] - 0.1, expression(paste("Expected income, ", hat(y))), xpd = TRUE, cex = axislabelsize, srt = 90) 


axis(1, at = ticksx,  pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy,  pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)
# the point (y0,delta0) and the indifference curve through it
lines(xx1, indiff(xx1, u = U(y0 - 4, delta0), alpha =  0.225), col = COLA[4], lwd = graphlinewidth)
lines(xx1, indiff(xx1, u = U(y0, delta0)), col = COLA[4], lwd = graphlinewidth)
lines(xx1, indiff(xx1, u = U(y0 + 2.7, delta0), alpha = 0.175), col = COLA[4], lwd = graphlinewidth)
lines(xx1, indiff(xx1, u = U(y0 + 4.7, delta0), alpha = 0.15), col = COLA[4], lwd = graphlinewidth)
#points(delta0, y0, pch = 16, col = COLB[4], cex = 1.5)

# the point (yR, deltaR), (yRprime, deltaRprime), Add segments
segments(deltaR, 0, deltaR, yRprime, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, yR, deltaR, yR, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, yRprime, deltaR, yRprime, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(deltaT, 0, deltaT, yT, lty = 2, col = grays[20], lwd = segmentlinewidth)

#Diagonal from the origin
segments(0, 0, deltaR, yRprime, lty = 2, col = COLB[4], lwd = segmentlinewidth)

#Label points
points(deltaR, yR, pch = 16, col = "black", cex = 1.5)
points(deltaRprime, yRprime, pch = 16, col = "black", cex = 1.5)
# the point (yT, deltaT)
points(deltaT, yT, pch = 16, col = "black", cex = 1.5)

#points(0, y0, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
#text(0.1, y0 + 0.2, expression(paste(z)), cex = labelsize)

#Label 5 points on line

text(1.6, 1.2 - 0.1, expression(paste(u[0])), cex = labelsize)
text(1.6, 5.2 - 0.1, expression(paste(u[1])), cex = labelsize)
text(1.6, 7.8 - 0.1, expression(paste(u[2])), cex = labelsize)
text(1.6, 9.8 - 0.1, expression(paste(u[3])), cex = labelsize)

text(deltaR + 0.15, yR - 0.3, expression(paste(d)), cex = labelsize)
text(deltaR-0.15, yRprime + 0.3, expression(paste(e)), cex = labelsize)


#Arrow for cost
brackets(deltaR + 0.2, yRprime - 0.3, deltaR + 0.2, yR + 0.3,  ticks = 0.5, curvature = 0.5, type = 1, 
         col = "black", lwd = 2, lty = 1, h = 0.3, xpd = TRUE)
text(deltaR + 1.25, (yR + yRprime)/2, expression(paste("Tax")), cex = labelsize)

# #Arrow for tax
# brackets(deltaR + 0.3, yRprime - 0.3, deltaR + 0.3, yR + 0.3,  ticks = 0.5, curvature = 0.5, type = 1, 
#          col = "black", lwd = 2, lty = 1, h = 0.3, xpd = TRUE)
# #Arrows(deltaR + 0.3, yR + 0.3, deltaR + 0.3, yRprime - 0.3, code = 3, col = "black", lty = 1, arr.type = "triangle")
# text(deltaR + 1, (yR + yRprime)/2, expression(paste("Tax")), cex = labelsize)
text(deltaT + 0.1, yT - 0.3, expression(f), cex = labelsize)


#Arrow for reduced risk
Arrows(deltaT + 0.2, 0.3, deltaR - 0.1, 0.3, code = 1, col = "black", lty = 1, arr.type = "triangle")
#text(deltaT + 1, 1.6, expression(paste("Taxation")), cex = labelsize)
text(deltaT + 1, 1.5, expression(paste("Tax reduces")), cex = labelsize)
text(deltaT + 1, 0.8, expression(paste("risk exposure")), cex = labelsize)


dev.off()
