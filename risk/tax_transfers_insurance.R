#Graph Designer: Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
library(pBrackets)
pdf(file = "risk/tax_transfers_insurance.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2
a <- c(2, 4, 6)


COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(4, 6, .5, .5))
xlims <- c(0, 15)
ylims <- c(0, 18)

indiff <- function(g, intercept = 4, slope = 0.09){
  intercept + slope*g^2 + 0.15*g
}

insur <- function(g, intercept = 3, slope = 0.36){
  intercept  + slope*g
}

#Plot command 
plot(0, 0, xlim = xlims, ylim = ylims, 
     type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     axes = FALSE,
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i"
)

#Customize ticks and labels for the plot

ticksy <- c(0, 3, 4, 7, 9, 13, ylims[2])
ylabels <- c(NA, expression(paste(underline(y)(1 - phi))), expression(y[b]), expression(y[c]), expression(y[d]), expression(y[e]), NA)
ticksx <- c(0, 8.74, xlims[2])
xlabels <- c(NA, expression(Delta[d]), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = axislabelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = axislabelsize)
mtext(expression(paste("Risk, ", Delta)), side=1, line = 2.5, cex = axislabelsize)
text(xlims[1] - 1.5, ylims[2] - 0.5*(ylims[2] - ylims[1]), expression(paste("Expected income, ", hat(y))), xpd = TRUE, cex = axislabelsize, srt = 90) 


npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

segments(8.74, 0, 8.74, 18, col = "grey", lwd = segmentlinewidth, lty = 2)

lines(xx1, indiff(xx1, intercept = 4, slope = 0.09), col = COLA[4], lwd = graphlinewidth, lty = 1)
lines(xx1, indiff(xx1, intercept = 7, slope = 0.08), col = COLA[4], lwd = graphlinewidth, lty = 1)
lines(xx1, indiff(xx1, intercept = 9, slope = 0.075), col = COLA[4], lwd = graphlinewidth, lty = 1)
lines(xx1, indiff(xx1, intercept = 13, slope = 0.04), col = COLA[4], lwd = graphlinewidth, lty = 1)

lines(xx1, insur(xx1, slope = 0.75), col = COLB[4], lwd = graphlinewidth, lty = 1)
lines(xx1, insur(xx1, slope = 1.28), col = COLB[4], lwd = graphlinewidth, lty = 1)
lines(xx1, insur(xx1, slope = 1.49), col = COLB[4], lwd = graphlinewidth, lty = 1)
lines(xx1, insur(xx1, slope = 0), col = COLB[4], lwd = graphlinewidth, lty = 1)

# Points

points(8.74, 3, pch = 16, col = "black", cex = 1.5) # a
points(8.74, 9.53, pch = 16, col = "black", cex = 1.5) # b
points(8.74, 14.187, pch = 16, col = "black", cex = 1.5) # c
points(8.74, 16.01, pch = 16, col = "black", cex = 1.5) # d
points(8.74, 17.367, pch = 16, col = "black", cex = 1.5) # e
points(3.333, 5.5, pch = 16, col = "black", cex = 1.5) # b'
points(7, 11.97, pch = 16, col = "black", cex = 1.5) # c'

# Points Labels

text(9, 2.5, expression(a), cex = labelsize)
text(9, 9, expression(b), cex = labelsize)
text(9, 13.6, expression(c), cex = labelsize)
text(9, 15.5, expression(d), cex = labelsize)
text(9, 17, expression(e), cex = labelsize)
text(3.5, 4.9, expression(paste(b,"'")), cex = labelsize)
text(7.25, 11.5, expression(paste(c,"'")), cex = labelsize)

# Label value functions

text(.5, 4.75, expression(u[1]), cex = labelsize)
text(.5, 7.75, expression(u[2]), cex = labelsize)
text(.5, 9.75, expression(u[3]), cex = labelsize)
text(.5, 13.75, expression(u[4]), cex = labelsize)


dev.off()
