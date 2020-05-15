#Graph Designer(s): Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "risk/risk_neutral.pdf", width = 10, height = 8)

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

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 14, 1, 1))

#Concave utility of wealth function

ConcaveU <- function(x){
  (1600 - (x - 40)^2)^(1/2)
}

#Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 40)
xlims <- c(0, 40)

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
     xaxs = "i" 
     #yaxs="i"
)

pointbx <- 10
pointcx <- 25

ticksy <- c(0, 5, 20, 35, ylims[2])
ylabels <- c(NA, expression(paste(v(y - Delta, y))), NA, expression(paste(v(y + Delta, y))), NA)
ticksx <- c(0, 5, 20, 35, xlims[2])
xlabels <- c(NA, expression(paste(y - Delta, y)), expression(paste(y == y[ce])), expression(paste(y + Delta, y)), NA)

axis(1,at = ticksx,  pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Axis labels and draw linear utility function
mtext(expression(paste("Wealth, y")), side = 1, line = 2.5, cex = axislabelsize)
text(-13, 0.5*ylims[2], expression(paste("The value of wealth, v(y)")), xpd = TRUE, cex = axislabelsize, srt = 90) 

text(-5, 22.5, expression(paste(v(ce) == v(L), phantom() == phantom())), cex = labelsize, xpd = TRUE)
text(-4, 20, expression(paste(Pv(y - Delta, y))), cex = labelsize, xpd = TRUE)
text(-6, 17.5, expression(paste((1 - P)(v)(y + Delta, y))), cex = labelsize, xpd = TRUE)

segments(0, 5, 5, 5, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(5, 0, 5, 5, lty = 2, col = grays[20] , lwd = segmentlinewidth)

segments(0, 20, 20, 20, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(20, 0, 20, 20, lty = 2, col = grays[20] , lwd = segmentlinewidth)

segments(0, 35, 35, 35, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(35, 0, 35, 35, lty = 2, col = grays[20] , lwd = segmentlinewidth)

segments(0, 0, xlims[2], ylims[2], lty = 1, col = COLA[4] , lwd = graphlinewidth)

text(5, 6.5, expression(paste("a")), cex = labelsize)
points(5, 5, pch = 16, col = "black", cex = 1.5)

text(20, 21.5, expression(paste("b")), cex = labelsize)
points(20, 20, pch = 16, col = "black", cex = 1.5)

text(35, 36.5, expression(paste("c")), cex = labelsize)
points(35, 35, pch = 16, col = "black", cex = 1.5)


dev.off()