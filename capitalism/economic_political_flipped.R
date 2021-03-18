#Graph Designer: Simon Halliday & Riley Boeth '17
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(plotrix)
library(pBrackets)
pdf(file = "capitalism/economic_political_flipped.pdf", width = 8, height = 8)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.7
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(0, 0, 0, 2))

#Indifference curves of a risk-averse homo economicus (2nd graph out of the two for 4.7)

#Add limits on axes and levels of utility for each indifference curve
ylims <- c(10, 35)
xlims <- c(0, 30)

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


text(5, 34, expression(bold("Capital")), xpd = TRUE, cex = labelsize)
text(5, 33, expression(bold("market")), xpd = TRUE, cex = labelsize)

text(5, 31, expression(paste("Lenders")), xpd = TRUE, cex = labelsize)
text(5, 30, expression(paste("(B)")), xpd = TRUE, cex = labelsize)

text(5, 25, expression(paste("Borrowers")), xpd = TRUE, cex = labelsize)
text(5, 24, expression(paste("(A)")), xpd = TRUE, cex = labelsize)

text(5, 22, expression(paste("Credit rationed")), xpd = TRUE, cex = labelsize)
text(5, 21, expression(paste("(C)")), xpd = TRUE, cex = labelsize)

segments(1, 23, 9, 23, lty = 1, col = "black" , lwd = 2)


text(15, 34, expression(bold("Manager")), xpd = TRUE, cex = labelsize)
text(15, 33,  expression(bold("market")), xpd = TRUE, cex = labelsize)

text(15, 26, expression(paste("Owners")), xpd = TRUE, cex = labelsize)
text(15, 25, expression(paste("(B)")), xpd = TRUE, cex = labelsize)
 
text(15, 20, expression(paste("Managers")), xpd = TRUE, cex = labelsize)
text(15, 19, expression(paste("(A)")), xpd = TRUE, cex = labelsize)
 
text(15, 17, expression(paste("Job rationed")), xpd = TRUE, cex = labelsize)
text(15, 16, expression(paste("(C)")), xpd = TRUE, cex = labelsize)

segments(11, 18, 19, 18, lty = 1, col = "black" , lwd = 2)



text(25, 34, expression(bold("Labor")), xpd = TRUE, cex = labelsize)
text(25, 33,expression(bold("market")), xpd = TRUE, cex = labelsize)

text(25, 21, expression(paste("Employers")), xpd = TRUE, cex = labelsize)
text(25, 20, expression(paste("(B)")), xpd = TRUE, cex = labelsize)
 
text(25, 15, expression(paste("Employed")), xpd = TRUE, cex = labelsize)
text(25, 14, expression(paste("(A)")), xpd = TRUE, cex = labelsize)
 
text(25, 12, expression(paste("Unemployed")), xpd = TRUE, cex = labelsize)
text(25, 11, expression(paste("(C)")), xpd = TRUE, cex = labelsize)

segments(21, 13, 29, 13, lty = 1, col = "black" , lwd = 2)


#Brackets between levels
#Capital to Manager
brackets(x1 = 10, y1 = 30.5,
         x2 = 10, y2 = 24.5,
         ticks = 0.75, curvature = 0.4, type = 1, h = 0.4,
         col = COLB[5], lwd = 2, lty = 1, xpd = TRUE)
#Manager to Labor
brackets(x1 = 20, y1 = 25.5,
         x2 = 20, y2 = 19.5,
         ticks = 0.75, curvature = 0.4, type = 1, h = 0.4,
         col = COLB[5], lwd = 2, lty = 1, xpd = TRUE)

#Arrow within levels
#Lender to Borrower
Arrows(5, 29, 5, 26,  col = COLA[5], lty = 1, lwd = 3, code = 2, arr.type = "triangle", arr.lwd = 0.5)
#Manager to Labor
Arrows(15, 24, 15, 21,  col = COLA[5], lty = 1, lwd = 3, code = 2, arr.type = "triangle", arr.lwd = 0.5)
#Employer to Employee
Arrows(25, 19, 25, 16,  col = COLA[5], lty = 1, lwd = 3, code = 2, arr.type = "triangle", arr.lwd = 0.5)

dev.off()


