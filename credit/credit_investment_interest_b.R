#' Graph Designer: Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "credit/credit_investment_interest_b.pdf", width = 8, height = 6)

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
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 6, 0.1, 4))

brfFn <- function(delta, q = 16, k = 1) {
  .5 + (delta / (2 * q)) * (1 - k)
}

PCFn <- function(delta, q = 1) {
  delta/q
}

isoreturnFn <- function(delta, b = 0.5, rho = 0.05) {
  1 - ((1 + rho)/(1 - b))*(1/delta)
}

yFn <- function(d1, f1, q = 1){
  q*f1*(1 - f1) - d1*(1 - f1)
}

ylow <- function(delta, q = 1, ybar = 0.0625){
  (-sqrt(delta^2 - 2*delta*q + q^2 - 4*q*ybar) + delta + q)/(2*q)
}

yhigh <- function(delta, q = 1, ybar = 0.0625){
  (sqrt(delta^2 - 2*delta*q + q^2 - 4*q*ybar) + delta + q)/(2*q)
}

xlims <- c(0, 12)
ylims <- c(0, 1)

npts <- 501 
d1 <- seq(xlims[1], xlims[2], length.out = npts)
f1 <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(0.0625)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs = "i", 
     yaxs = "i"
)

deltalvls <- c(5.7, 8.5, 11.2)

ticksy <- c(ylims[1], ylims[2] - 0.1)
ylabels <- c(NA, 1)
ticksx <- c(xlims[1], 2.1, 2.8, deltalvls, xlims[2])
xlabels <- c(NA, expression(paste(frac(1 + rho[1], 1 - b))), expression(paste(frac(1 + rho[2], 1 - b))), expression(paste(delta[i])), expression(paste(delta[h])), expression(paste(delta[g])), NA)


npts <- 503 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

axis(1, at = ticksx, pos = 0, labels = NA, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

equitylevels <- c(0.04, 0.275, 0.666)
#Draw the graphs
lines(xx1, brfFn(xx1, k = equitylevels[1]), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfFn(xx1, k = equitylevels[2]), col = COLA[4], lwd = graphlinewidth)
lines(xx1, brfFn(xx1, k = equitylevels[3]), col = COLA[4], lwd = graphlinewidth)
lines(xx1, isoreturnFn(xx1, rho = 0.05), col = COLB[4], lwd = graphlinewidth)
lines(xx1, isoreturnFn(xx1, rho = 0.4), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, isoreturnFn(xx1, pi = 0.5), col = COLB[4], lwd = graphlinewidth)


#Axis labels
#mtext(expression(paste("Interest factor, ", delta)), side = 1, line = 3.3, cex = axislabelsize)
text(0.5*(xlims[2]), -0.16, expression(paste("Interest factor, ", delta)), xpd = TRUE, cex = axislabelsize) 
text(-1, 0.5*(ylims[2]), expression(paste("Probability of failure (risk), ", f)), xpd = TRUE, cex = axislabelsize, srt = 90) 

text(x = c(xlims[1], 1.65, 3.18, deltalvls, xlims[2]), 
     par("usr")[3] - 0.01, 
     labels = xlabels, 
     srt = 0, pos = 1, xpd = TRUE, 
     cex = labelsize)

#segments(0.39, -1, 0.39, brfFn(0.39, k = 0.75), lty = 2, col = grays[20] , lwd = segmentlinewidth)
#points(0.39, brfFn(0.39, k = 0.75), pch = 16, col = "black", cex = 1.5)
#text(0.39 + 0.025, brfFn(0.39, k = 0.75) - 0.03, expression(paste(b)), cex = labelsize)

segments(deltalvls[2], 0, deltalvls[2], brfFn(8.5, k = equitylevels[1]), lty = 2, col = grays[20] , lwd = segmentlinewidth)
points(8.5, brfFn(8.5, k = equitylevels[1]), pch = 16, col = "black", cex = 1.5)
text(8.5, brfFn(8.5, k = equitylevels[1]) + 0.04, expression(paste(h)), cex = labelsize)

segments(deltalvls[3], 0, deltalvls[3], brfFn(deltalvls[3], k = equitylevels[2]), lty = 2, col = grays[20] , lwd = segmentlinewidth)
points(11.2, brfFn(11.2, k = equitylevels[2]), pch = 16, col = "black", cex = 1.5)
text(11.2 + 0.2, brfFn(11.2, k = equitylevels[2]) - 0.03, expression(paste(g)), cex = labelsize)


segments(deltalvls[1], 0, deltalvls[1], brfFn(deltalvls[1], k = equitylevels[2]), lty = 2, col = grays[20] , lwd = segmentlinewidth)
points(5.7, brfFn(5.7, k = equitylevels[2]), pch = 16, col = "black", cex = 1.5)
text(5.7 + 0.2, brfFn(5.7, k = equitylevels[2]) - 0.03, expression(paste(i)), cex = labelsize)


# text(1, brfFn(1, k = 0.5) + 0.04, expression(paste(n)), cex = labelsize)

#points(0.59, brfFn(0.59, k = 0.75), pch = 16, col = "black", cex = 1.5)
#text(0.59 + 0.025, brfFn(0.59, k = 0.75) - 0.03, expression(paste(a)), cex = labelsize)

#segments(.7, -1, 0.7, brfFn(delta = 0.7, k = 0.28), lty = 2, col = grays[20] , lwd = segmentlinewidth)
#points(0.7, brfFn(0.7, k = 0.28), pch = 16, col = "black", cex = 1.5)

# ZPC labels
text(2.2, 0.2, expression(paste(hat(pi)[rho[1]]) ), cex = labelsize, xpd = TRUE)
text(4, 0.2, expression(paste(hat(pi)[rho[2]] )), cex = labelsize, xpd = TRUE)

#BRF labels
text(12.8, 0.85, expression(paste(BRF[k[1]]) ), cex = labelsize, xpd = TRUE)
text(12.8, 0.76, expression(paste(BRF[k[2]] )), cex = labelsize, xpd = TRUE)
text(12.8, 0.62, expression(paste(BRF[k[3]] )), cex = labelsize, xpd = TRUE)


# text(1.1, 1.08, expression(paste("BRF of previously excluded")), cex = labelsize, xpd = TRUE)
# text(1.1, 1.02, expression(paste("now marginal borrower")), cex = labelsize, xpd = TRUE)
# Arrows(1.1, 0.98, 1.1, 0.92, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# 
# text(1.1, 0.5, expression(paste("BRF of")), cex = labelsize)
# text(1.1, 0.44, expression(paste("wealthy borrower")), cex = labelsize)
# Arrows(1.1, 0.53, 1.1, 0.6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# 
# 
# text(0.6, 0.91, expression(paste("BRF of previously")), cex = labelsize)
# text(0.6, 0.85, expression(paste("marginal borrower")), cex = labelsize)
# Arrows(0.6, 0.82, 0.6, 0.68, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#text(0.49, 0.15, expression(paste("Policy")), cex = labelsize)
#text(0.49, 0.09, expression(paste("Change")), cex = labelsize)
#Arrows(7.2, 0.62, 5.8, 0.62, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


dev.off()