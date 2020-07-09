#Graph Designer: Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
library(pBrackets)
pdf(file = "risk/tax_transfers_initial_a.pdf", width = 9, height = 7)

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

a <- c(2, 4, 6)
par(mar =  c(4, 6, 0.5, 0.5))
xlims <- c(0, 12)
ylims <- c(0, 13)

indiff <- function(g, intercept = 4, slope = 0.09){
  intercept + slope*g^2 + 0.15*g
}

insur <- function(g, intercept = 3, slope = 0.36){
  intercept  + slope*g
}

#Plot command 
plot(0, 0, xlim = xlims, ylim = ylims, 
     type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     axes = FALSE,
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs = "i", 
     yaxs = "i"
)

#Customize ticks and labels for the plot

ticksy <- c(0, 9.55, ylims[2])
ylabels <- c(NA,  expression(hat(y)[f]), NA)
ticksx <- c(0, 8.74, xlims[2])
xlabels <- c(NA, expression(Delta[f]), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)
mtext(expression(paste("Risk, ", Delta)), side = 1, line = 2.5, cex = axislabelsize)
text(xlims[1] - 1.5, ylims[2] - 0.5*(ylims[2] - ylims[1]), expression(paste("Expected income, ", hat(y))), xpd = TRUE, cex = axislabelsize, srt = 90) 


npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

segments(8.74, 0, 8.74, 9.53, col = grays[20], lwd = segmentlinewidth, lty = 2)

#Segment to point f 
segments(0, 9.53, 8.74, 9.53, col = grays[20], lwd = segmentlinewidth, lty = 2)

#Segment to point f' 
#segments(0, 5.5, 8.74, 5.5, col = grays[20], lwd = segmentlinewidth, lty = 2)
#segments(3.333, 0, 3.333, 5.5, col = grays[20], lwd = segmentlinewidth, lty = 2)

lines(xx1, indiff(xx1, intercept = 0.6, slope = 0.1), col = COLA[4], lwd = graphlinewidth, lty = 1)
lines(xx1, indiff(xx1, intercept = 4, slope = 0.09), col = COLA[4], lwd = graphlinewidth, lty = 1)
lines(xx1, indiff(xx1, intercept = 7, slope = 0.08), col = COLA[4], lwd = graphlinewidth, lty = 1)
#lines(xx1, indiff(xx1, intercept = 9, slope = 0.075), col = COLA[4], lwd = graphlinewidth, lty = 1)
#lines(xx1, indiff(xx1, intercept = 9.2, slope = 0.08), col = COLA[4], lwd = graphlinewidth, lty = 1)
lines(xx1, indiff(xx1, intercept = 13, slope = 0.04), col = COLA[4], lwd = graphlinewidth, lty = 1)

#Insurance Lines
#Though points f' and f
#lines(xx1, insur(xx1, slope = 0.75), col = COLB[4], lwd = graphlinewidth, lty = 1)
#lines(xx1, insur(xx1, slope = 1.28), col = COLB[4], lwd = graphlinewidth, lty = 1)
#lines(xx1, insur(xx1, slope = 1.49), col = COLB[4], lwd = graphlinewidth, lty = 1)
#lines(xx1, insur(xx1, slope = 1.638), col = COLB[4], lwd = graphlinewidth, lty = 1)
#lines(xx1, insur(xx1, slope = 1.559), col = COLB[4], lwd = graphlinewidth, lty = 1)
#Flat line
#lines(xx1, insur(xx1, slope = 0), col = COLB[4], lwd = graphlinewidth, lty = 1)

# Points

#points(8.74, 3, pch = 16, col = "black", cex = 1.5) # e
points(8.74, 9.53, pch = 16, col = "black", cex = 1.5) # f
#points(8.74, 14.187, pch = 16, col = "black", cex = 1.5) # g
points(8.74, 16.01, pch = 16, col = "black", cex = 1.5) # h
#points(8.74, 16.626, pch = 16, col = "black", cex = 1.5) # h
points(8.74, 17.367, pch = 16, col = "black", cex = 1.5) # i
#points(3.333, 5.5, pch = 16, col = "black", cex = 1.5) # f'
#points(7, 11.97, pch = 16, col = "black", cex = 1.5) # g'
#points(8.74, 12.2, pch = 16, col = "black", cex = 1.5) # j

# Points Labels

#text(9, 2.5, expression(e), cex = labelsize)
text(8.9, 9.2, expression(f), cex = labelsize)
#text(9, 13.65, expression(g), cex = labelsize)
text(9, 15.5, expression(h), cex = labelsize)
# text(9, 16.3, expression(d), cex = labelsize)
text(9, 17, expression(i), cex = labelsize)
#text(9, 12.2 - 0.2, expression(j), cex = labelsize)
# text(9, 17.37, expression(e), cex = labelsize)
#text(3.5, 5.1, expression(paste(f,"'")), cex = labelsize)
#text(7.25, 11.5, expression(paste(g,"'")), cex = labelsize)

# Label value functions

text(.5, 1.1, expression(u[0]), cex = labelsize)
text(.5, 4.55, expression(u[1]), cex = labelsize)
text(.5, 7.55, expression(u[2]), cex = labelsize)
# text(.5, 9.55, expression(u[3]), cex = labelsize) # if using u[3] w int = 9.2, 9.55 --> 9.75
# text(.5, 13.55, expression(u[4]), cex = labelsize)

#Label average wealth curve and indifference curves generally

# text(10.5, 8, expression(paste("Reduced")), xpd = TRUE, cex = labelsize)
# text(10.5, 7.45, expression(paste("expected")), xpd = TRUE, cex = labelsize)
# text(10.5, 7, expression(paste("income")), xpd = TRUE, cex = labelsize)

# Bracket
# brackets(x1 = 9, y1 = indiff(8.74, intercept = 0.6, slope = 0.1), 
#          x2 = 9, y2 = indiff(3.33, intercept = 4, slope = 0.09),  ticks = 0.5, curvature = 0.5, type = 1, 
#          col = "black", lwd = 2, lty = 1, h = 0.5, xpd = TRUE)

# Reduced risk (delta)
# brackets(x1 = 8.74 - 0.1, y1 = indiff(3.33, intercept = 4, slope = 0.09) - 0.8, 
#          x2 = 3.33 + 0.1, y2 = indiff(3.33, intercept = 4, slope = 0.09) - 0.8,  ticks = 0.5, curvature = 0.5, type = 1, 
#          col = "black", lwd = 2, lty = 1, h = 0.5, xpd = TRUE)
# 
# text(3.33 + (8.74-3.33)/2, 3.9, expression(paste("Reduced")), xpd = TRUE, cex = labelsize)
# text(3.33 + (8.74-3.33)/2, 3.4, expression(paste("risk")), xpd = TRUE, cex = labelsize)
#text(5.41, 7, expression(paste("income")), xpd = TRUE, cex = labelsize)


# Tax and transfer line
# text(11.5, 12.8, expression(paste("Tax and")), xpd = TRUE, cex = labelsize)
# text(11.5, 12.3, expression(paste("transfer line")), xpd = TRUE, cex = labelsize)


dev.off()