#Graph Designer: Simon Halliday, Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "what_can_markets_do/curfew_indifference_b.pdf", width = 10, height = 8)

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
par(mar =  c(6, 5, 2, 3))

#proportion of wealth functions

MUb <- function(x, beta = 1/2, Tb = 9){
  2*beta*(Tb - x)
}

uA <- function(x, y, alpha = 1/2, Ta = 3){
  y - alpha*(Ta - x)^2
}

uB <- function(x, y, alpha = 1/2, Tb = 9){
  -y + alpha*(Tb - x)^2
}

uB2 <- function(x, y, alpha = 1/2, Tb = 9){
  y + alpha*(Tb - x)^2
}

MUa <- function(x, alpha = 1/2, Ta = 3){
  -2*alpha*(Ta - x)
}


#Add limits on axes and levels of utility for each function 
ylims <- c(0, 10)
xlims <- c(2, 10)
b <- c(-5, -2.5, 0)

b2 <- c(-5, -2.5, 0, 5, 10)
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
contour(x, y, 
        outer(x, y, uB),
        drawlabels = FALSE,
        col = COLB[3],
        lwd = graphlinewidth,
        levels = b, 
        xaxs = "i", 
        yaxs = "i", 
        add = TRUE)

# contour(x, y, 
#         outer(x, y, uB2),
#         drawlabels = FALSE,
#         col = COLA[3],
#         lwd = graphlinewidth,
#         levels = b2, 
#         xaxs = "i", 
#         yaxs = "i", 
#         add = TRUE)

#x and y limits with plain axes without ticks/numbers to match previous graph; y axes on both sides

ticksx <- c(2, 3, 6, 9, 10)
xlabels <- c(NA, expression(paste(T^A == 9*p*m)), expression(paste(12*a*m)),  expression(paste(T^B  == 3*a*m)), NA)
#ticksy <- c(0, MUa(x = 7.5), MUa(x = 6), MUb(x = 7.5), ylims[2])
#ylabels <- c(NA, expression(paste(-u[T]^A*(T*minute))), expression(paste(-u[T]^A == u[T]^B)), expression(paste(u[T]^B*(T*minute))), NA)
ticksy <- c(ylims[1], ylims[2])
ylabels <- c(NA,  NA)

axis(1, at = ticksx,  pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy,  pos = 2, labels = ylabels, las = 1, cex.axis = labelsize)
#axis(4, at = ticksy2, pos = 9, labels = FALSE, las = 1, cex.axis = labelsize)


#Disutility polygon
# xrent <- c(3, 4.5, 9, 3)
# yrent <- c(MUa(3), MUa(4.5, alpha = 3/4), MUb(9), MUa(3))
# polygon(xrent, yrent, col = COL[4], density = NULL, border = NA)


npts <- 503 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)
xx5 <- seq(xlims[1], xlims[2], length.out = npts)

#Axis labels and draw linear utility functions
text(6, -1.2, expression(paste("Curfew, T")), xpd = TRUE, cex = axislabelsize, srt = 0) 
#text(, side = 1, line = 2, cex = axislabelsize)
text(xlims[1] - 0.5, 0.5*ylims[2], expression(paste("Payment received from A, ", y^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 

# lines(xx1, MUb(xx1), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, MUa(xx1), col = COLA[5], lwd = graphlinewidth)
#lines(xx5, MUa(xx5, alpha = 3/4), col = COLA[5], lwd = segmentlinewidth, lty = 1)
#lines(xx1, MUb(xx1, beta = 1/4), col = COLB[4], lwd = segmentlinewidth, lty = 1)

#Label points on line

# text(6, MUa(6) + 0.2, expression(paste(i)), cex = labelsize)
# segments(6, 0, 6, MUa(6), lty = 2, col = grays[20], lwd = segmentlinewidth)
# segments(0, MUa(6), 6, MUa(6), lty = 2, col = grays[20], lwd = segmentlinewidth)
# points(6, MUa(6), pch = 16, col = "black", cex = 1.5)

# text(7.5 + 0.1, MUa(7.5) - 0.1, expression(paste(g)), cex = labelsize)
# segments(7.5, MUb(7.5), 0, MUb(7.5), lty = 2, col = grays[20], lwd = segmentlinewidth)
# segments(7.5, MUa(7.5), 0, MUa(7.5), lty = 2, col = grays[20], lwd = segmentlinewidth)
# segments(7.5, 0, 7.5, MUa(7.5), lty = 2, col = grays[20], lwd = segmentlinewidth)
# points(7.5, MUb(7.5), pch = 16, col = "black", cex = 1.5)

# text(7.5 + 0.1, MUb(7.5) + 0.1, expression(paste(h)), cex = labelsize)
# points(7.5, MUa(7.5), pch = 16, col = "black", cex = 1.5)

#Label relevant points on axes

#text(20, -.9, expression(paste("x*")), xpd = TRUE, cex = labelsize)
#text(33, -.9, expression(paste("a"[x])), xpd = TRUE, cex = labelsize)

#text(-2, 33, expression(paste(-u[x]^B*(x*minute))),  xpd = TRUE, cex = labelsize)
#text(-2, 7, expression(paste(u[x]^A*(x*minute))),  xpd = TRUE, cex = labelsize)

# text(4, 4, expression(paste("B's marginal disutility")), xpd = TRUE, cex = labelsize)
# text(4, 3.5, expression(paste(u[T]^B == 2*beta*(T^B - T))), xpd = TRUE, cex = labelsize)
# #text(4, 5.5, expression(paste(beta == frac(1,2))), xpd = TRUE, cex = labelsize)
# text(3.8, 3, expression(paste(beta == frac(1,4))), xpd = TRUE, cex = labelsize)


# text(7.75, 5.5, expression(paste("-A's marginal disutility ")),  xpd = TRUE, cex = labelsize)
# text(7.75, 5.2, expression(paste(-u[T]^A == -2*alpha*(T^A - T))),  xpd = TRUE, cex = labelsize)
# #text(7.75, 5.5, expression(paste(alpha == frac(1,2))), xpd = TRUE, cex = labelsize)
# text(7.75, 4.7, expression(paste(alpha == frac(3,4))), xpd = TRUE, cex = labelsize)

#Better for B arrow and label
text(9.3, 8, expression(paste("Better for B")), 
     xpd = TRUE, cex = axislabelsize )
Arrows(8.5, 7, 8.5, 9,
       col = "black", lty = 1, lwd = 2, 
       arr.type = "triangle",
       arr.lwd = 0.5, code = 2)
#arrows(6, 6, 3, 8, xpd = TRUE, length=0.1,angle=40,lwd=3)

text(4.5, 9, expression(paste( u[1]^B )), xpd = TRUE, cex = labelsize)
text(5.2, 9, expression(paste( u[2]^B )), xpd = TRUE, cex = labelsize)
text(5.9, 9, expression(paste( u[3]^B )), xpd = TRUE, cex = labelsize)

#Axis arrow
Arrows(6.75, -1.2, 8, -1.2,
       col = "black", lty = 1, lwd = 2, 
       arr.type = "triangle", xpd = TRUE,
       arr.lwd = 0.5, code = 2)

text(7.25, -0.9, expression(paste("Later")), 
     xpd = TRUE, cex = axislabelsize )

dev.off()