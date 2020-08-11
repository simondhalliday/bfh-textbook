#Graph Designer: Simon Halliday & Riley Boeth '17
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(plotrix)
pdf(file = "capitalism/economic_political_flipped.pdf", width = 8, height = 10)

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
ylims <- c(3, 41)
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


text(5, 40, expression(paste("Capital")), xpd = TRUE, cex = labelsize)
text(5, 39, expression(paste("Market")), xpd = TRUE, cex = labelsize)

text(5, 35, expression(paste("Lenders")), xpd = TRUE, cex = labelsize)
text(5, 34, expression(paste("(B)")), xpd = TRUE, cex = labelsize)

text(5, 25, expression(paste("Borrowers")), xpd = TRUE, cex = labelsize)
text(5, 24, expression(paste("(A)")), xpd = TRUE, cex = labelsize)

text(5, 15, expression(paste("Credit Rationed")), xpd = TRUE, cex = labelsize)
text(5, 14, expression(paste("(C)")), xpd = TRUE, cex = labelsize)

segments(1, 16, 9, 16, lty = 1, col = "black" , lwd = 2)


text(15, 40, expression(paste("Manager")), xpd = TRUE, cex = labelsize)
text(15, 39,  expression(paste("Market")), xpd = TRUE, cex = labelsize)

text(15, 30, expression(paste("Owners")), xpd = TRUE, cex = labelsize)
text(15, 29, expression(paste("(B)")), xpd = TRUE, cex = labelsize)
 
text(15, 20, expression(paste("Managers")), xpd = TRUE, cex = labelsize)
text(15, 19, expression(paste("(A)")), xpd = TRUE, cex = labelsize)
 
text(15, 10, expression(paste("Job Rationed")), xpd = TRUE, cex = labelsize)
text(15, 9, expression(paste("(C)")), xpd = TRUE, cex = labelsize)

segments(11, 11, 19, 11, lty = 1, col = "black" , lwd = 2)



text(25, 40, expression(paste("Labor")), xpd = TRUE, cex = labelsize)
text(25, 39,expression(paste("Market")), xpd = TRUE, cex = labelsize)

text(25, 25, expression(paste("Employers")), xpd = TRUE, cex = labelsize)
text(25, 24, expression(paste("(B)")), xpd = TRUE, cex = labelsize)
 
text(25, 15, expression(paste("Employed")), xpd = TRUE, cex = labelsize)
text(25, 14, expression(paste("(A)")), xpd = TRUE, cex = labelsize)
 
text(25, 5, expression(paste("Unemployed")), xpd = TRUE, cex = labelsize)
text(25, 4, expression(paste("(C)")), xpd = TRUE, cex = labelsize)

segments(21, 6, 29, 6, lty = 1, col = "black" , lwd = 2)


#Arrow between levels
#Capital to Manager
#Arrows(15, 22.5, 15, 16.5,  col = COLB[5], lty = 1, lwd = 3, code = 2, arr.type = "triangle", arr.lwd = 0.5)
#Manager to Labor
#Arrows(20, 12.5, 20, 6.5,  col = COLB[5], lty = 1, lwd = 3, code = 2, arr.type = "triangle", arr.lwd = 0.5)

#Arrow within levels
#Lender to Borrower
Arrows(5, 33, 5, 26,  col = COLA[5], lty = 1, lwd = 3, code = 2, arr.type = "triangle", arr.lwd = 0.5)
#Manager to Labor
Arrows(15, 28, 15, 21,  col = COLA[5], lty = 1, lwd = 3, code = 2, arr.type = "triangle", arr.lwd = 0.5)
#Employer to Employee
Arrows(25, 23, 25, 16,  col = COLA[5], lty = 1, lwd = 3, code = 2, arr.type = "triangle", arr.lwd = 0.5)

dev.off()


