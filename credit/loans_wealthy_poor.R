#' Graph Designer: Scott Cohn
#' Authors: Bowles & Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(pBrackets)
pdf(file = "credit/loans_wealthy_poor.pdf", width = 9, height = 7)


# setup -------------------------------------------------------------------

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.2
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
par(mar =  c(6, 3, 1, 3))

xlims <- c(0, 10)
ylims <- c(0, 10)

npts <- 501 

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs = "i", 
     yaxs = "i")

ticksy <- c(0, 2, 8, ylims[2])
ylabels <- c(NA, expression(paste(q^{kN})),  expression(paste(q^0, phantom()^N)), NA)
ticksy2 <- c(0, ylims[2])
ylabels2 <- c(NA, NA)
ticksx <- c(0, 2, 5, xlims[2])
xlabels <- c(0, expression(paste(phi^{0}, phantom()^N)), expression(phi^e), 1)


# lines -------------------------------------------------------------------

# gray segments
segments(5, 0, 5, 5, lty = 2, lwd = segmentlinewidth, col = grays[20])
segments(2, 0, 2, 8, lty = 2, lwd = segmentlinewidth, col = grays[20])
segments(0, 2, 2, 2, lty = 2, lwd = segmentlinewidth, col = grays[20])
segments(0, 8, 2, 8, lty = 2, lwd = segmentlinewidth, col = grays[20])

# blue (top L to bottom R)
segments(0, 9, 0.75, 9, lty = 1, lwd = graphlinewidth, col = COLB[4])
segments(0.75, 9, 0.75, 8, lty = 1, lwd = graphlinewidth, col = COLB[4])
segments(0.75, 8, 2, 8, lty = 1, lwd = graphlinewidth, col = COLB[4])
segments(2, 8, 2, 7, lty = 1, lwd = graphlinewidth, col = COLB[4])
segments(2, 7, 3, 7, lty = 1, lwd = graphlinewidth, col = COLB[4])
segments(3, 7, 3, 6, lty = 1, lwd = graphlinewidth, col = COLB[4])
segments(3, 6, 5, 6, lty = 1, lwd = graphlinewidth, col = COLB[4])
segments(5, 6, 5, 4.5, lty = 1, lwd = graphlinewidth, col = COLB[4])
segments(5, 4.5, 6, 4.5, lty = 1, lwd = graphlinewidth, col = COLB[4])
segments(6, 4.5, 6, 3.5, lty = 1, lwd = graphlinewidth, col = COLB[4])
segments(6, 3.5, 7.5, 3.5, lty = 1, lwd = graphlinewidth, col = COLB[4])
segments(7.5, 3.5, 7.5, 2.5, lty = 1, lwd = graphlinewidth, col = COLB[4])
segments(7.5, 2.5, 8.25, 2.5, lty = 1, lwd = graphlinewidth, col = COLB[4])
segments(8.25, 2.5, 8.25, 1.5, lty = 1, lwd = graphlinewidth, col = COLB[4])
segments(8.25, 1.5, 10, 1.5, lty = 1, lwd = graphlinewidth, col = COLB[4])

# green (bottom L to top R)
segments(0, 1, 2, 1, lty = 1, lwd = graphlinewidth, col = COLA[4])
segments(2, 1, 2, 2, lty = 1, lwd = graphlinewidth, col = COLA[4])
segments(2, 2, 3, 2, lty = 1, lwd = graphlinewidth, col = COLA[4])
segments(3, 2, 3, 3, lty = 1, lwd = graphlinewidth, col = COLA[4])
segments(3, 3, 4, 3, lty = 1, lwd = graphlinewidth, col = COLA[4])
segments(3, 3, 4, 3, lty = 1, lwd = graphlinewidth, col = COLA[4])
segments(4, 3, 4, 4, lty = 1, lwd = graphlinewidth, col = COLA[4])
segments(4, 4, 5, 4, lty = 1, lwd = graphlinewidth, col = COLA[4])
segments(5, 4, 5, 5.5, lty = 1, lwd = graphlinewidth, col = COLA[4])
segments(5, 5.5, 6, 5.5, lty = 1, lwd = graphlinewidth, col = COLA[4])
segments(6, 5.5, 6, 6.5, lty = 1, lwd = graphlinewidth, col = COLA[4])
segments(6, 6.5, 7.5, 6.5, lty = 1, lwd = graphlinewidth, col = COLA[4])
segments(7.5, 6.5, 7.5, 7, lty = 1, lwd = graphlinewidth, col = COLA[4])
segments(7.5, 7, 8.5, 7, lty = 1, lwd = graphlinewidth, col = COLA[4])
segments(8.5, 7, 8.5, 8, lty = 1, lwd = graphlinewidth, col = COLA[4])
segments(8.5, 8, 9.25, 8, lty = 1, lwd = graphlinewidth, col = COLA[4])
segments(9.25, 8, 9.25, 9, lty = 1, lwd = graphlinewidth, col = COLA[4])
segments(9.25, 9, 10, 9, lty = 1, lwd = graphlinewidth, col = COLA[4])

# axis
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)
axis(4, at = ticksy2, pos = 10, labels = ylabels2, las = 1, cex.axis = labelsize)

# points + labels ---------------------------------------------------------

points(5, 5, pch = 16, size = 1.5) # e
text(4.75, 5, expression(e), cex = labelsize)
points(2, 2, pch = 16, size = 1.5) # a
text(1.75, 2.25, expression(a), cex = labelsize)
points(2, 8, pch = 16, size = 1.5) # b
text(1.75, 7.75, expression(b), cex = labelsize)

# annotations -------------------------------------------------------------

text(1, 9.5, expression("Poor borrower's"), cex = labelsize)
text(1, 9.15, expression("best projects"), cex = labelsize)

text(8.8, 9.5, expression("Wealthy borrower's"), cex = labelsize)
text(8.8, 9.15, expression("best projects"), cex = labelsize)

text(8.75, 6.65, expression("Quality of wealthy"), cex = labelsize)
text(8.75, 6.3, expression(paste("borrower's projects, ", q^k)), cex = labelsize)

text(8.75, 3.35, expression("Quality of poor"), cex = labelsize)
text(8.75, 3, expression(paste("borrower's projects, ", q^0)), cex = labelsize)

# brackets + labels -------------------------------------------------------

# TODO Add brackets

brackets(1.9, -0.75, 0, -0.75, lty = 1, h = 0.3, lwd = segmentlinewidth, col = 1, xpd = TRUE)
text(1, -1.5, expression("Loans to poor"), cex = labelsize, xpd = TRUE)
text(1, -1.85, expression("borrowers"), cex = labelsize, xpd = TRUE)

brackets(10, -0.75, 2.1, -0.75, lty = 1, h = 0.3, lwd = segmentlinewidth, col = 1, xpd = TRUE)
text(6, -1.5, expression("Loans to wealthy"), cex = labelsize, xpd = TRUE)
text(6, -1.85, expression("borrowers"), cex = labelsize, xpd = TRUE)

dev.off()