#Graph Designer: Simon Halliday
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics
library(plotrix)
require(shape)
library(pBrackets)
pdf(file = "competitionmarkets/housing_bubble.pdf", width = 7, height = 7)

# Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#ff7f00", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")

a <- c(2, 4, 6)
par(mar =  c(4, 4, 2, 4))
xlims <- c(0, 1.3)
ylims <- c(0, 1.3)

s_shaped <- function(x, 
                     intercept = 0.1, 
                     coeff = 9, 
                     constant = 160){
  intercept + exp(coeff*x)/(exp(x*coeff) + constant)
}

degree45 <- function(x, slope = 1){
  slope*x
}


#Plot command 
plot(0, 0, xlim = xlims, ylim = ylims, 
     type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     axes = FALSE,
     bty = "n", 
     xaxs = "i", 
     yaxs = "i"
)


#Customize ticks and labels for the plot
ticksy <- c(0,  riskreturn(g = 5.6), insurance(g = 1.8), insurance(g = 10.4), ylims[2])
ylabels <- c(NA, expression(paste(hat(y)[a])), expression(paste(hat(y)[d])), expression(paste(hat(y)[c])), NA)
ticksx <- c(0, 1.8, 5.6, 10.5, xlims[2])
xlabels <- c(NA, expression(paste(Delta[d])), expression(paste(Delta[a])), expression(paste(Delta[c])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = axislabelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = axislabelsize)
axis.break(1, breakpos = 0.05, breakcol="black", style="slash")
axis.break(2, breakpos = 0.05, breakcol="black", style="slash")

#mtext(expression(paste("Degree of risk, ", Delta)), side=1, line = 2.5, cex = axislabelsize)
text(xlims[2] - 0.5*(xlims[2] - xlims[1]), 
     ylims[1] - 0.12, 
     expression(paste("Price now, ", p)), xpd = TRUE, cex = axislabelsize) 
text(xlims[1] - 0.1, 
     ylims[2] - 0.5*(ylims[2] - ylims[1]), 
     expression(paste("Expected price later, ", hat(p))), xpd = TRUE, cex = axislabelsize, srt = 90) 

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(xlims[1], 10.5, length.out = npts)
xx3 <- seq(10.5, xlims[2], length.out = npts)
lines(xx1, s_shaped(xx1), col = CBCols[1], lwd = graphlinewidth, lty = 1)
lines(xx1, s_shaped(xx1, intercept = 0.27), col = CBCols[1], lwd = graphlinewidth, lty = 2)
lines(xx1, degree45(xx1), col = CBCols[2], lwd = graphlinewidth, lty = 1)

#lines(xx3, insurance(xx3), col = COL[3], lwd = segmentlinewidth, lty = 2)
#lines(xx1, insurance(xx1, intercept = 7.4), col = COL[3], lwd = segmentlinewidth, lty = 2)
#lines(xx1, indiffA(xx1, intercept = 5.6), col = COLB[4], lwd = graphlinewidth, lty = 1)
#lines(xx1, indiffA(xx1, intercept = 7.7, slope = 0.115), col = COLB[4], lwd = graphlinewidth, lty = 1)
#lines(xx1, indiffA(xx1, intercept = 7.75, slope = 0.115), col = COLB[4], lwd = graphlinewidth, lty = 1)
#lines(xx1, indiffA(xx1, intercept = 10.35, slope = 0.1), col = COLB[4], lwd = graphlinewidth, lty = 1)


today <- c(0.12, 0.54, 1.09, 1.27)


points(today, today, pch = 16, col = "black", cex = 1.5)
text(today[1] + 0.03, today[1] - 0.03, expression(a), cex = labelsize)
text(today[2] + 0.03, today[2] - 0.03, expression(b), cex = labelsize)
text(today[3] + 0.03, today[3] - 0.03, expression(c), cex = labelsize)
text(today[4] + 0.03, today[4] - 0.03, expression(d), cex = labelsize, xpd = TRUE)

#Label curves
text(1.4, 1.33, expression(paste(hat(p) == p)), cex = labelsize, xpd = TRUE)
text(1.4, 1.05, expression(paste(hat(p)(p))), cex = labelsize, xpd = TRUE)

dev.off()

