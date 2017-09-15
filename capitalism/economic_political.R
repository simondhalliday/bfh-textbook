#Graph Designer: Simon Halliday & Riley Boeth '17
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(plotrix)
pdf(file = "capitalism/economic_political.pdf", width = 10, height = 8)

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
ylims <- c(0, 30)
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
     xaxs="i", 
     yaxs="i"
)


#axis(1,at = ticksx,  pos = 0, labels = xlabels)
#axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1)


#draw.circle(35, 5, 2, nv = 1000, border = COLA[5], col = COLA[1], lty = 1, lwd = 3)
# text(30, 6, expression(paste(C)), xpd = TRUE, cex = labelsize)
# text(30, 4, expression(paste(y^C == 2)), xpd = TRUE, cex = labelsize)


#Arrow between 2 and 4
#Arrows(9, 5, 32, 5,  col = COLB[5], lty = 1, lwd = 3, code = 3, arr.type = "triangle", arr.lwd = 0.5)

#draw.circle(5, 5, 3, nv = 1000, border = COLA[5], col = COLA[1], lty = 1, lwd = 3)
# text(5, 6, expression(paste(B)), xpd = TRUE, cex = labelsize)
# text(5, 4, expression(paste(y^B == 4)), xpd = TRUE, cex = labelsize)

#Arrow between 4 and 10
#Arrows(6, 9, 16, 25,  col = COLB[5], lty = 1, lwd = 3, code = 3, arr.type = "triangle", arr.lwd = 0.5)


text(5, 25.5, expression(paste("Capital")), xpd = TRUE, cex = labelsize)
text(5, 24.5, expression(paste("Market")), xpd = TRUE, cex = labelsize)

text(15, 25.5, expression(paste("Lenders")), xpd = TRUE, cex = labelsize)
text(15, 24.5, expression(paste("(B)")), xpd = TRUE, cex = labelsize)

text(25, 25.5, expression(paste("Borrowers")), xpd = TRUE, cex = labelsize)
text(25, 24.5, expression(paste("(A)")), xpd = TRUE, cex = labelsize)

text(35, 25.5, expression(paste("Credit Rationed")), xpd = TRUE, cex = labelsize)
text(35, 24.5, expression(paste("(C)")), xpd = TRUE, cex = labelsize)

segments(10, 23, 30, 23, lty = 1, col = "black" , lwd = 2)

segments(30, 24, 30, 26, lty = 1, col = "black" , lwd = 2)


text(5, 15.5, expression(paste("Manager")), xpd = TRUE, cex = labelsize)
text(5, 14.5, expression(paste("Market")), xpd = TRUE, cex = labelsize)

text(15, 15.5, expression(paste("Owners")), xpd = TRUE, cex = labelsize)
text(15, 14.5, expression(paste("(B)")), xpd = TRUE, cex = labelsize)

text(25, 15.5, expression(paste("Managers")), xpd = TRUE, cex = labelsize)
text(25, 14.5, expression(paste("(A)")), xpd = TRUE, cex = labelsize)

text(35, 15.5, expression(paste("Job Rationed")), xpd = TRUE, cex = labelsize)
text(35, 14.5, expression(paste("(C)")), xpd = TRUE, cex = labelsize)

segments(10, 13, 30, 13, lty = 1, col = "black" , lwd = 2)

segments(30, 14, 30, 16, lty = 1, col = "black" , lwd = 2)


text(5, 5.5, expression(paste("Labor")), xpd = TRUE, cex = labelsize)
text(5, 4.5, expression(paste("Market")), xpd = TRUE, cex = labelsize)

text(20, 5.5, expression(paste("Employers")), xpd = TRUE, cex = labelsize)
text(20, 4.5, expression(paste("(B)")), xpd = TRUE, cex = labelsize)

text(30, 5.5, expression(paste("Employed")), xpd = TRUE, cex = labelsize)
text(30, 4.5, expression(paste("(A)")), xpd = TRUE, cex = labelsize)

text(38, 5.5, expression(paste("Unemployed")), xpd = TRUE, cex = labelsize)
text(38, 4.5, expression(paste("(C)")), xpd = TRUE, cex = labelsize)

#segments(10, 13, 30, 13, lty = 1, col = "black" , lwd = 2)

segments(34, 4, 34, 6, lty = 1, col = "black" , lwd = 2)


#draw.circle(20, 30, 5, nv = 1000, border = COLA[5], col = COLA[1], lty = 1, lwd = 3)
#text(20, 30, expression(paste(A)), xpd = TRUE, cex = labelsize)


# text(20, 28, expression(paste(y^A == 10)), xpd = TRUE, cex = labelsize)

#Arrow between levels
#Capital to Manager
Arrows(15, 22.5, 15, 16.5,  col = COLB[5], lty = 1, lwd = 3, code = 2, arr.type = "triangle", arr.lwd = 0.5)
#Manager to Labor
Arrows(20, 12.5, 20, 6.5,  col = COLB[5], lty = 1, lwd = 3, code = 2, arr.type = "triangle", arr.lwd = 0.5)

#Arrow within levels
#Lender to Borrower
Arrows(17.5, 25.5, 21.5, 25.5,  col = COLA[5], lty = 1, lwd = 3, code = 2, arr.type = "triangle", arr.lwd = 0.5)
#Manager to Labor
Arrows(17.5, 15.5, 21.5, 15.5,  col = COLA[5], lty = 1, lwd = 3, code = 2, arr.type = "triangle", arr.lwd = 0.5)
#Employer to Employee
Arrows(23, 5.5, 27, 5.5,  col = COLA[5], lty = 1, lwd = 3, code = 2, arr.type = "triangle", arr.lwd = 0.5)

#Bottom of frame
# segments(14, 10, 26, 10, lty = 1, col = "black" , lwd = 1)
# #Top of frame
# segments(14, 18, 26, 18, lty = 1, col = "black" , lwd = 1)
# #Left of frame
# segments(14, 10, 14, 18, lty = 1, col = "black" , lwd = 1)
# #Right of frame
# segments(26, 10, 26, 18, lty = 1, col = "black" , lwd = 1)


dev.off()


