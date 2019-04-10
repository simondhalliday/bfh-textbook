#Graph Designer: Simon Halliday & Scott Cohn
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "risk/indiff_map_scale.pdf", width = 10, height = 8)


#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 1, 1))

#Indifference curves of a risk-averse homo economicus (2nd graph out of the two for 4.7)

indiffA <- function(x, ua = 2, slope = 1/12) {
  ua + slope*(x)^2
}

#Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 32.5)
xlims <- c(0, 15)

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
     xaxs="i", 
     yaxs="i"
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

#Axis labels and draw linear utility function
mtext(expression(paste("Risk, ", Delta)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.8, 0.5*ylims[2], expression(paste("Average Wealth, ",omega)), xpd = TRUE, cex = axislabelsize, srt = 90) 

# Asset Labels

text(3, 24, expression(paste("Substantial and")), xpd = TRUE, cex = labelsize)
text(3, 22.5, expression(paste("General Assets")), xpd = TRUE, cex = labelsize)
points(3, 26, pch = 16, col = "black", cex = 1.5,xpd = TRUE)
text(3.5, 26.5, expression(paste("A")), xpd = TRUE, cex = labelsize)


text(3, 10, expression(paste("Few and")), xpd = TRUE, cex = labelsize)
text(3, 8.5, expression(paste("General Assets")), xpd = TRUE, cex = labelsize)
points(3, 12, pch = 16, col = "black", cex = 1.5,xpd = TRUE)
text(3.5, 12.5, expression(paste("D")), xpd = TRUE, cex = labelsize)

text(13, 24, expression(paste("Substantial and")), xpd = TRUE, cex = labelsize)
text(13, 22.5, expression(paste("Specific Assets")), xpd = TRUE, cex = labelsize)
points(13, 26, pch = 16, col = "black", cex = 1.5,xpd = TRUE)
text(13.5, 26.5, expression(paste("B")), xpd = TRUE, cex = labelsize)

text(13, 10, expression(paste("Few and")), xpd = TRUE, cex = labelsize)
text(13, 8.5, expression(paste("Specific Assets")), xpd = TRUE, cex = labelsize)
points(13, 12, pch = 16, col = "black", cex = 1.5,xpd = TRUE)
text(13.5, 12.5, expression(paste("C")), xpd = TRUE, cex = labelsize)

dev.off()
