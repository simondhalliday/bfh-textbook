#Graph Designer: Simon Halliday
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
library(pBrackets)
pdf(file = "competitionmarkets/housing_bubble_base.pdf", width = 7, height = 7)

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
par(mar =  c(4, 5, 2, 4))
xlims <- c(0, 1.3)
ylims <- c(0, 1.3)

s_shaped <- function(x, 
                     intercept = 0.05, 
                     coeff = 10, 
                     constant = 150){
  intercept + exp(coeff*x)/(exp(x*coeff) + constant)
}

degree45 <- function(x, slope = 1){
  slope*x
}

intline <- function(x, slope = 0.4, int = 0.4){
  int + slope*x
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

today <- c(0.27, 0.508, 0.67, 0.828, 1.07)
#Customize ticks and labels for the plot
ticksy <- c(0,  intline(today[1]),  intline(today[3]), intline(today[5]), ylims[2])
ylabels <- c(NA, expression(paste(hat(p)[l])), expression(paste(hat(p)[e])), expression(paste(hat(p)[h])), NA)
ticksx <- c(0, today[1], today[3], today[5], xlims[2])
xlabels <- c(NA, expression(paste(p[l])), expression(paste(p[e])), expression(paste(p[h])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = axislabelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = axislabelsize)
#mtext(expression(paste("Degree of risk, ", Delta)), side=1, line = 2.5, cex = axislabelsize)
text(xlims[2] - 0.5*(xlims[2] - xlims[1]), 
     ylims[1] - 0.12, 
     expression(paste("Price now, ", p)), xpd = TRUE, cex = axislabelsize) 
text(xlims[1] - 0.18, 
     ylims[2] - 0.5*(ylims[2] - ylims[1]), 
     expression(paste("Expected price later, ", hat(p))), xpd = TRUE, cex = axislabelsize, srt = 90) 

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(xlims[1], 10.5, length.out = npts)
xx3 <- seq(10.5, xlims[2], length.out = npts)
# lines(xx1, s_shaped(xx1), col = CBCols[1], lwd = graphlinewidth, lty = 1)
# lines(xx1, s_shaped(xx1, intercept = 0.24), col = CBCols[1], lwd = graphlinewidth, lty = 2)
lines(xx1, degree45(xx1), col = CBCols[2], lwd = graphlinewidth, lty = 1)
lines(xx1, intline(xx1), col = CBCols[1], lwd = graphlinewidth, lty = 1)

#lines(xx3, insurance(xx3), col = COL[3], lwd = segmentlinewidth, lty = 2)
#lines(xx1, insurance(xx1, intercept = 7.4), col = COL[3], lwd = segmentlinewidth, lty = 2)
#lines(xx1, indiffA(xx1, intercept = 5.6), col = COLB[4], lwd = graphlinewidth, lty = 1)
#lines(xx1, indiffA(xx1, intercept = 7.7, slope = 0.115), col = COLB[4], lwd = graphlinewidth, lty = 1)
#lines(xx1, indiffA(xx1, intercept = 7.75, slope = 0.115), col = COLB[4], lwd = graphlinewidth, lty = 1)
#lines(xx1, indiffA(xx1, intercept = 10.35, slope = 0.1), col = COLB[4], lwd = graphlinewidth, lty = 1)



#Segments to l
segments(today[1], 0, 
         today[1], intline(today[1]), 
         lty = 2, col = grays[20], lwd = segmentlinewidth)

segments(0, intline(today[1]), 
         today[1], intline(today[1]), 
         lty = 2, col = grays[20], lwd = segmentlinewidth)

#segments to e
segments(today[3], 0, 
         today[3], intline(today[3]), 
         lty = 2, col = grays[20], lwd = segmentlinewidth)

segments(0, intline(today[3]), 
         today[3], intline(today[3]), 
         lty = 2, col = grays[20], lwd = segmentlinewidth)

#segments to h
segments(today[5], 0, 
         today[5], intline(today[5]), 
         lty = 2, col = grays[20], lwd = segmentlinewidth)

segments(0, intline(today[5]), 
         today[5], intline(today[5]), 
         lty = 2, col = grays[20], lwd = segmentlinewidth)


points(today[3], today[3], pch = 16, col = "black", cex = 1.5)
points(today[1], intline(today[1]), pch = 16, col = "black", cex = 1.5)
points(today[5], intline(today[5]), pch = 16, col = "black", cex = 1.5)

#Arrows up to equilibrium
# Arrows(today[1], today[1] + 0.02, 
#        today[1], intline(today[1]) - 0.04,
#        col = "black", lty = 1, lwd = 2,
#        arr.type = "triangle", xpd = TRUE,
#        arr.lwd = 0.5, code = 2)

Arrows(today[1] + 0.02, today[2], 
       today[2] - 0.04, today[2],
       col = "black", lty = 1, lwd = 2,
       arr.type = "triangle", xpd = TRUE,
       arr.lwd = 0.5, code = 2)

Arrows(today[2], today[2]  + 0.02, 
       today[2], intline(today[2])  - 0.03,
       col = "black", lty = 1, lwd = 2,
       arr.type = "triangle", xpd = TRUE,
       arr.lwd = 0.5, code = 2)


#Arrows down to equilibrium
# Arrows(today[5], today[5] - 0.02, 
#        today[5], intline(today[5]) + 0.04,
#        col = "black", lty = 1, lwd = 2,
#        arr.type = "triangle", xpd = TRUE,
#        arr.lwd = 0.5, code = 2)

Arrows(today[5] - 0.02, today[4], 
       today[4] + 0.04, today[4],
       col = "black", lty = 1, lwd = 2,
       arr.type = "triangle", xpd = TRUE,
       arr.lwd = 0.5, code = 2)

Arrows(today[4], today[4] - 0.02,
       today[4], intline(today[4]) + 0.03,
       col = "black", lty = 1, lwd = 2,
       arr.type = "triangle", xpd = TRUE,
       arr.lwd = 0.5, code = 2)


# text(today[1] + 0.03, today[1] - 0.03, expression(a), cex = labelsize)
# text(today[2] + 0.03, today[2] - 0.03, expression(b), cex = labelsize)
text(today[1] - 0.03, intline(today[1]) + 0.03, expression(l), cex = labelsize)
text(today[3] + 0.03, today[3] - 0.03, expression(e), cex = labelsize)
text(today[5] + 0.03, intline(today[5]) - 0.03, expression(h), cex = labelsize)

#Label curves
text(1.4, 1.33, expression(paste(hat(p) == p)), cex = labelsize, xpd = TRUE)

text(1.4, 0.93, expression(paste(hat(p)(p))), cex = labelsize, xpd = TRUE)





dev.off()

