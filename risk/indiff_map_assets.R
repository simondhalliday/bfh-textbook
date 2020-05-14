#Graph Designer: Simon Halliday & Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "risk/indiff_map_assets.pdf", width = 8, height = 8)

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
grays <- gray.colors(25, start = 1, end = 0)

a <- c(2, 4, 6)
par(mar =  c(4, 4, .5, .5))
xlims <- c(0, 13)
ylims <- c(0, 18)

indiff <- function(g, intercept = 4, slope = 0.09){
  intercept + slope*g^2 + 0.15*g
}

insur <- function(g, intercept = 3, slope = 0.36){
  intercept  + slope*g
}

#Indifference curves of a risk-averse homo economicus (2nd graph out of the two for 4.7)
indiffA <- function(x, ua = 2, slope = 1/12) {
  ua + slope*(x)^2
}


npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

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

#x and y limits with plain axes without ticks/numbers to match previous graph

#ticksx <- seq(from = 0, to = xlims[2]+1, by = 4)
#xlabels <- seq(from = 0, to = xlims[2], by = 1)
ticksx <- c(xlims[1], xlims[2])
xlabels <- c(NA, NA)
#ticksy <- seq(from = 0, to = ylims[2]+1, by = 4)
#ylabels <- seq(from = 0, to = ylims[2], by = 1)
ticksy <- c(ylims[1], ylims[2])
ylabels <- c(NA, NA)

axis(1,at = ticksx,  pos = 0, labels = xlabels)
axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)


lines(xx1, indiff(xx1, intercept = 2, slope = 0.09), col = COLA[4], lwd = graphlinewidth, lty = 1)
lines(xx1, indiff(xx1, intercept = 8, slope = 0.075), col = COLA[4], lwd = graphlinewidth, lty = 1)
lines(xx1, indiff(xx1, intercept = 13, slope = 0.04), col = COLA[4], lwd = graphlinewidth, lty = 1)

#Axis labels and draw linear utility function
mtext(expression(paste("Risk, ", Delta)), side=1, line = 2.5, cex = axislabelsize)
text(xlims[1] - 1, ylims[2] - 0.5*(ylims[2] - ylims[1]), expression(paste("Expected income, ", hat(y))), xpd = TRUE, cex = axislabelsize, srt = 90) 

#label the three indifference curves

text(20.5, indiffA(20)+3.355, expression(paste(u[1])), xpd = TRUE, cex = labelsize)
text(16, indiffA(18)+9.8, expression(paste(u[2])),  xpd = TRUE, cex = labelsize)
text(10.5, indiffA(16)+15.5, expression(paste(u[3])),  xpd = TRUE, cex = labelsize)

# Asset Labels
text(2, 12.25, expression(paste("Substantial and")), xpd = TRUE, cex = labelsize)
text(2, 11.5, expression(paste("general assets")), xpd = TRUE, cex = labelsize)

text(2, 6.25, expression(paste("Few and")), xpd = TRUE, cex = labelsize)
text(2, 5.5, expression(paste("general assets")), xpd = TRUE, cex = labelsize)

text(8, 12.25, expression(paste("Substantial")), xpd = TRUE, cex = labelsize)
text(8, 11.5, expression(paste("and specific")), xpd = TRUE, cex = labelsize)
text(8, 10.75, expression(paste("assets")), xpd = TRUE, cex = labelsize)

text(8, 6.25, expression(paste("Few and")), xpd = TRUE, cex = labelsize)
text(8, 5.5, expression(paste("specific assets")), xpd = TRUE, cex = labelsize)

# Label value functions

text(.5, 2.55, expression(u[1]), cex = labelsize)
text(.5, 8.55, expression(u[2]), cex = labelsize)
text(.5, 13.55, expression(u[3]), cex = labelsize)

dev.off()