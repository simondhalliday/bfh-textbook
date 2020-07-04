#Graph Designer: Simon Halliday & Scott Cohn
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(pBrackets)
pdf(file = "risk/taxes_transfers_big.pdf", width = 10, height = 8)


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
par(mar =  c(5, 7, 1, 1))

taxline <- function(x, tau = 0.3, b = 0){
  tau*x + b
}

#Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 50000)
xlims <- c(0, 160000)

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




#Expected incomes in order of low, phi*ybar, high
expinc <- c(50000,76000,120000)
tau <- c(0.3)
exptax <- tau*expinc

ticksy <- c(ylims[1], exptax, ylims[2])
ylabels <- c(NA, exptax, NA)

ticksx <- c(xlims[1], expinc, xlims[2])
xlabels <- c(NA, expinc, NA)
#xlabels <- c(NA, expression(paste(y[L])), expression(paste(underline(y)(1 -  phi))), expression(paste(y[H])), NA)


axis(1,at = ticksx,  pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)
xx5 <- seq(xlims[1], xlims[2], length.out = npts)

#Axis labels and 
#mtext(expression(paste("Risk, ", Delta)), side = 1, line = 2.5, cex = axislabelsize)
text(-22000, 0.5*ylims[2], expression(paste("Taxes paid, ", hat(T), ", and transfers received, $")), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(0.5*xlims[2], -5500 , expression(paste("Expected income pre-tax, ", hat(y) )), xpd = TRUE, cex = axislabelsize) 

#draw linear tax function
lines(xx5, taxline(x = xx5), col = COLA[4], lwd = graphlinewidth)

#mtext(expression(paste("Income, ", y)), side = 1, line = 3, cex = axislabelsize)

# Segments
#phi&ybar
segments(expinc[2], 0, expinc[2], exptax[2], lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(0, exptax[2], xlims[2], exptax[2], lty = 1, col = COLB[4], lwd = graphlinewidth)

#low expected income
segments(0, exptax[1], expinc[1], exptax[1], lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(expinc[1], 0, expinc[1], exptax[1], lty = 2, col = grays[20], lwd = segmentlinewidth)

#high expected income
segments(0, exptax[3], expinc[3], exptax[3], lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(expinc[3], 0, expinc[3], exptax[3], lty = 2, col = grays[20], lwd = segmentlinewidth)


# Points
points(expinc[2], exptax[2], pch = 16, col = "black", cex = 1.5,xpd = TRUE)
points(expinc[1], exptax[1], pch = 16, col = "black", cex = 1.5,xpd = TRUE)
points(expinc[3], exptax[3], pch = 16, col = "black", cex = 1.5,xpd = TRUE)

# Brackets
brackets(x1 = expinc[1] - 500, y1 = exptax[1] + 500, x2 = expinc[1]- 500, y2 = exptax[2] - 500,  
         ticks = 0.5, curvature = 0.5, type = 1, 
         col = "black", lwd = 2, lty = 1, xpd = TRUE)

brackets(x1 = expinc[3] + 500, y1 = exptax[3] - 500, x2 = expinc[3] + 500, y2 = exptax[2] + 500,  
         ticks = 0.5, curvature = 0.5, type = 1, 
         col = "black", lwd = 2, lty = 1, xpd = TRUE)

# Labels
text(32000, exptax[2] - 2500, expression(paste("Taxes paid")), xpd = TRUE, cex = labelsize)
text(32000, exptax[2] - 4000, expression(paste("lower than")), xpd = TRUE, cex = labelsize)
text(32000, exptax[2] - 5500, expression(paste("transfers")), xpd = TRUE, cex = labelsize)

text(140000, exptax[3] - 5000, expression(paste("Taxes paid")), xpd = TRUE, cex = labelsize)
text(140000, exptax[3] - 6500, expression(paste("higher than")), xpd = TRUE, cex = labelsize)
text(140000, exptax[3] - 8000, expression(paste("transfers")), xpd = TRUE, cex = labelsize)

text(expinc[1], exptax[1] + 1500, expression(paste("a")), xpd = TRUE, cex = labelsize)
text(expinc[2], exptax[2] + 1500, expression(paste("b")), xpd = TRUE, cex = labelsize)
text(expinc[3], exptax[3] + 1500, expression(paste("c")), xpd = TRUE, cex = labelsize)

text(140000, ylims[2] - 1500, expression(paste("Taxes")), xpd = TRUE, cex = labelsize)
text(140000, ylims[2] - 3500, expression(paste("paid")), xpd = TRUE, cex = labelsize)
text(140000, ylims[2] - 5500, expression(paste((tau*y))), xpd = TRUE, cex = labelsize)

text(140000, exptax[2] - 1000, expression(paste("Transfers")), xpd = TRUE, cex = labelsize)
text(140000, exptax[2] - 2800, expression(paste("received")), xpd = TRUE, cex = labelsize)
text(140000, exptax[2] - 5000, expression(paste("by all citizens")), xpd = TRUE, cex = labelsize)
text(140000, exptax[2] - 7000, expression(paste(tau*underline(y), (1 - phi))), xpd = TRUE, cex = labelsize)



dev.off()