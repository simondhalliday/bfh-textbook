#Graph Designer(s): Simon Halliday & Riley Boeth '17 & Scott Cohn
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "risk/risk_neutral_u_positive.pdf", width = 10, height = 8)

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
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 12, 1, 1))

#Linear utility of wealth equation

LinearU <- function(x,y) {
  x + 7
}


#Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 43)
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
     xaxs="i"#, 
     #yaxs="i"
)

#x and y limits with plain axes without ticks/numbers to match previous graph

ticksx <- c(0, 2, NA, 19, 36, xlims[2])
xlabels <- c(NA, NA, NA, NA, NA)
ticksy <- c(0, 9, 26, ylims[2])
ylabels <- c(NA, NA, NA, NA)

axis(1,at = ticksx,  pos = 0, labels = FALSE)
axis(2,at = ticksy,  pos = 0, labels = FALSE, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Axis labels and draw linear utility function
mtext(expression(paste("Income, y")), side = 1, line = 2.5, cex = axislabelsize)
text(-12, 0.5*ylims[2], expression(paste("Utility, u(y)")), xpd = TRUE, cex = axislabelsize, srt = 90) 

lines(xx1, LinearU(xx1, y), col = COLA[5], lwd = graphlinewidth)

#Label 3 points on line

text(2, 10.7, expression(paste("b")), cex = labelsize)
segments(2, 0, 2, 9, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, 9, 2, 9, lty = 2, col = grays[20], lwd = segmentlinewidth)
points(2, 9, pch = 16, col = "black", cex = 1.5)

# text(9.7, 17.9, expression(paste("c")), cex = labelsize)
# segments(9.7, 0, 9.7, 16.7, lty = 2, col = grays[20], lwd = segmentlinewidth)
# segments(0, 16.7, 9.7, 16.7, lty = 2, col = grays[20], lwd = segmentlinewidth)
# points(9.7, 16.7, pch = 16, col = "black", cex = 1.5)

text(19, 27.5, expression(paste("a")), cex = labelsize)
segments(19, 0, 19, 26, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, 26, 19, 26, lty = 2, col = grays[20], lwd = segmentlinewidth)
points(19, 26, pch = 16, col = "black", cex = 1.5)

text(36, 44.5, expression(paste("g")), cex = labelsize, xpd = TRUE)
segments(36, 0, 36, 43, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, 43, 36, 43, lty = 2, col = grays[20], lwd = segmentlinewidth)
points(36, 43, pch = 16, col = "black", cex = 1.5)
#Label y-sub,x-sub,etc. on axes

text(36, -1.5, expression(paste(y^G == y[0] +  delta[1])), xpd = TRUE, cex = labelsize)
text(19, -1.5, expression(paste(y[0])),  xpd = TRUE,  cex = labelsize)
text(2.7, -1.6, expression(paste(y^B == y[0] - delta[2])),  xpd = TRUE,  cex = labelsize)


text(-3.5, 43, expression(paste(u(y + delta[1]))), xpd = TRUE, cex = labelsize)

text(-4.2, 28.5, expression(paste(u(y[c]) == v(L), phantom() == phantom())),  xpd = TRUE, cex = labelsize)
text(-5, 26, expression(paste(p%.%u(y[0] + delta[1]) + phantom())),  xpd = TRUE, cex = labelsize)
text(-5.5, 23.5, expression(paste((1 - p)%.%u(y[0] - delta[2]))),  xpd = TRUE, cex = labelsize)

text(-3.5, 9, expression(paste(u(y[0] - delta[2]))),  xpd = TRUE,  cex = labelsize)



dev.off()