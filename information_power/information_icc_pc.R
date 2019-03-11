require(ggplot2)
require(shape)
pdf(file = "information_power/information_icc_pc.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5

isov <- function(p, delta = 5) {
  (p - delta)/p
}

brfFn <- function(p, delta = 5) {
  1 - (2 * delta) /p
}

solowCondition <- function(p, delta = 5){
  (p*(1/(8*delta)))
}

#COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")
par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 40)
ylims <- c(0, 1)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")),
     xaxt = "n", yaxt = "n", cex.lab = axislabelsize, bty = "n")

mtext(expression(paste("Price, ", p)), side=1, line = 2.5, cex = axislabelsize)
text(-6.4, 0.5*ylims[2], expression(paste("Quality, ", q)), xpd = TRUE, cex = axislabelsize, srt = 90) 


npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(10, xlims[2], length.out = npts)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
xx0 <- seq(5, xlims[2], length.out = npts)


#Draw the lines for the graphs
lines(xx0, isov(xx0, delta = 5), col = COLA[3], lwd = 3)
lines(xx1, brfFn(xx1), col = COL[2], lwd = 3)
lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 3)

#Customize ticks and labels for the plot
ticksy <- c(0, 0.5, 1)
ylabels <- c(0, expression(paste(q^N == q^C, phantom() == frac(1,2))), 1)
ticksx <- c(0, 10, 20, 40)
xlabels <- c(0, expression(paste(p^C == 2*underline("u"))), expression(p^N == paste(4*underline("u"))), 40)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Annotation of the three graphs and the NE
text(31, 0.98, expression(paste("Incomplete Contract")))
text(31, 0.92, expression(paste("Iso-profit: ", q," = ", frac(p, 8*underline("u")))))
text(25, 0.34, expression(paste("Best Response")))
text(25, 0.29, expression(paste("Function = q = ", 1 - frac(2*underline("u"), p))))
Arrows(25, 0.38, 25, 0.57, col = "black", lty = 1, lwd = 2, arr.type = "triangle")


text(16, 0.52, expression(paste("Nash Equilibrium")))
text(20.6, 0.48, expression(paste("n")))


text(13, 0.98, expression(paste("Complete Contract")))
text(13, 0.92, expression(paste("Iso-profit: ", q," = ", frac(p, 4*underline("u")))))
text(35, 0.55, expression(paste("Participation")))
text(35, 0.5, expression(paste("Constraint = ", q == 1 - frac(underline("u"), p))))
Arrows(35, 0.58, 35, 0.82, col = "black", lty = 1, lwd = 2, arr.type = "triangle")


#Lines for the coordinates of the Nash equilbrium
segments(20, 0, 20, 0.5, lty = 2, col = "darkgray")
segments(0, 0.5, 20, 0.5, lty = 2, col = "darkgray")
segments(10, 0, 10, 0.5, lty = 2, col = "darkgray")

#Iso-profit slope annotation
# text(25, 0.81, expression(paste("Slope = ", frac(1, 8*delta))))
# Arrows(25, 0.79, 25, 0.66, col = "black", lty = 1, lwd = 2, arr.type = "triangle")

#BRF Slope annotation
# text(25, 0.41, expression(paste("Slope = ", frac(2*delta, p^2))))
# Arrows(25, 0.43, 25, 0.56, col = "black", lty = 1, lwd = 2, arr.type = "triangle")

#Add a point for the NE
points(20, 0.5, pch = 16, col = "black", cex = 1.5)

#Add a ray and a point for d.  
segments(0, 0, 20, 1, lty = 1, lwd = 3, col = COL[3])
points(10, 0.5, pch = 16, col = "black", cex = 1.2)
text(9.6, 0.53, expression(paste("c")))

#Add a ray and a point for a. 
# segments(0, 0, 20, 0.25, lty = 2, lwd = 3, col = "darkgray")
# points(11.8, 0.15, pch = 16, col = "black", cex = 1.2)
# text(11.3, 0.17, expression(paste("c")))

dev.off()

