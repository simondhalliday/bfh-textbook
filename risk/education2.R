source("risk/education.R")
# the second plot
pdf(file = "risk/education2.pdf", width = 10, height = 10)
#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 6, 1, 1))
plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("Risk, ",Delta)),
     ylab = "",
     xaxt = "n",
     yaxt = "n",
     cex.lab = axislabelsize + 0.5,
     bty = "n",
     xaxs="i",
     yaxs="i"
)

# ticksx <- seq(from = 0, to = xlims[2],by = 2)
# xlabels <- seq(from = 0, to = xlims[2],by = 2)
ticksy <- c(0, y0 - 4, y0, y0 + 2.7, y0 + 4.7, yT, 17, ylims[2])
ylabels <- c(NA, expression(paste(y[0])), expression(paste(y[1] == z)), expression(paste(y[2])), expression(paste(y[3])), expression(paste(y[d] == y[f])), expression(paste(y[e])), NA)
# ticksy <- seq(from = 0, to = ylims[2], by = 3)
# ylabels <- seq(from = 0, to = ylims[2], by = 3)
ticksx <- c(0, deltaT, 7, ylims[2])
xlabels <- c(NA, expression(paste(Delta[f])), expression(paste(Delta[d] == Delta[e])), NA)

text(-1, 0.5*ylims[2] - 0.1, expression(paste("Expected Income, ", y)), xpd = TRUE, cex = axislabelsize + 0.5, srt = 90) 


axis(1,at = ticksx,  pos = 0, labels = xlabels, cex.axis = axislabelsize)
axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1, cex.axis = axislabelsize)
# the point (y0,delta0) and the indifference curve through it
lines(xx1, indiff(xx1, u = U(y0 - 4, delta0), alpha =  0.225), col = COLA[5], lwd = graphlinewidth)
lines(xx1, indiff(xx1, u = U(y0, delta0)), col = COLA[5], lwd = graphlinewidth)
lines(xx1, indiff(xx1, u = U(y0 + 2.7, delta0), alpha = 0.175), col = COLA[5], lwd = graphlinewidth)
lines(xx1, indiff(xx1, u = U(y0 + 4.7, delta0), alpha = 0.15), col = COLA[5], lwd = graphlinewidth)
#points(delta0, y0, pch = 16, col = COLB[4], cex = 1.5)

# the point (yR, deltaR), (yRprime, deltaRprime), Add segments
segments(deltaR, 0, deltaR, yRprime, lty = 2, col = "darkgrey" , lwd = segmentlinewidth)
segments(0, yR, deltaR, yR, lty = 2, col = "darkgrey" , lwd = segmentlinewidth)
segments(0, yRprime, deltaR, yRprime, lty = 2, col = "darkgrey" , lwd = segmentlinewidth)
segments(deltaT, 0, deltaT, yT, lty = 2, col = "darkgrey", lwd = segmentlinewidth)

#Diagonal from the origin
segments(0, 0, deltaR, yRprime, lty = 2, col = COLB[2], lwd = segmentlinewidth)

#Label points
points(deltaR, yR, pch = 16, col = "black", cex = 1.5)
points(deltaRprime, yRprime, pch = 16, col = "black", cex = 1.5)
# the point (yT, deltaT)
points(deltaT, yT, pch = 16, col = "black", cex = 1.5)

#points(0, y0, pch = 16, col = "black", cex = 1.5, xpd = TRUE)
#text(0.1, y0 + 0.2, expression(paste(z)), cex = labelsize)

#Label 5 points on line

text(1.6, 1.2, expression(paste(u[0])), cex = labelsize)
text(1.6, 5.2, expression(paste(u[1])), cex = labelsize)
text(1.6, 7.8, expression(paste(u[2])), cex = labelsize)
text(1.6, 9.8, expression(paste(u[3])), cex = labelsize)

text(deltaR + 0.15, yR - 0.3, expression(paste(d)), cex = labelsize)
text(deltaR-0.15, yRprime + 0.3, expression(paste(e)), cex = labelsize)

#Arrow for tax
Arrows(deltaR + 0.3, yR + 0.3, deltaR + 0.3, yRprime - 0.3, code = 3, col = "black", lty = 1, arr.type = "triangle")
text(deltaR + 0.55, (yR + yRprime)/2 + 0.5, expression(paste("Tax")), cex = labelsize)
text(deltaT + 0.1, yT - 0.3, expression(f), cex = labelsize)


#Arrow for reduced risk
Arrows(deltaT + 0.2, 0.3, deltaR - 0.1, 0.3, code = 1, col = "black", lty = 1, arr.type = "triangle")
#text(deltaT + 1, 1.6, expression(paste("Taxation")), cex = labelsize)
text(deltaT + 1, 1.1, expression(paste("Tax reduces")), cex = labelsize)
text(deltaT + 1, 0.6, expression(paste("risk exposure")), cex = labelsize)


dev.off()
