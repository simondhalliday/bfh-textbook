#' Graph Designer: Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library("shape")

pdf(file = "risk/risks_low_very.pdf", width = 9, height = 7)

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

a <- c(2, 4, 6)
par(mar =  c(5, 5, 1, 1))
xlims <- c(0, 10)
ylims <- c(0, 20)


u <- function(y, delta, a = 1.5, b = 0.1, c = 2){
  y^a - b * delta^c
}

indiff <- function(u, delta, a = 1.5, b = 0.1, c = 2){
  (u + b * delta^c )^(1/a)
}

slopeA <- function(g, slope1 = 0.25, slope2 = 0.08){
  slope1 + 2*slope2*g
}
 
indiffA2 <- function(g, intercept = 7.8, slope1 = 0.25, slope2 = 0.055){
  intercept  + slope1*g + slope2*g^2
}

slopeA2 <- function(g, slope1 = 0.25, slope2 = 0.055){
  slope1 + 2*slope2*g
}


slopeA3 <- function(g, slope1 = 0.25, slope2 = 0.01){
  slope1 + 2*slope2*g
}

tangentline <- function(b, m, x){
  m*x + b
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

ticksx <- c(0, 3, 9, xlims[2])
xlabels <- c(NA, expression(paste(Delta[Low])), expression(paste(Delta[High])), NA)
ticksy <- c(0, 1, 7.368, 13.57, ylims[2])
ylabels <- c(NA, expression(paste(c[1])), expression(paste(c[2])), expression(paste(c[3])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#Axis labels and draw linear utility function
mtext(expression(paste("Difference in income (good versus bad outcome), ", Delta, ", risk")), side = 1, line = 2.5, cex = axislabelsize)
text(-1, 0.5*ylims[2], expression(paste("Expected income, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 


npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(2, 10, length.out = npts)


# Dashed
segments(3, 0, 3, ylims[2], lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(9, 0, 9, ylims[2], lty = 2, col = grays[20], lwd = segmentlinewidth)


lines(xx2, tangentline(b = -2.64, m = 1.565, x = xx2), col = grays[22], lwd = segmentlinewidth, lty = 2)
lines(xx2, tangentline(b = 4.289, m = 1.183, x = xx2), col = grays[22], lwd = segmentlinewidth, lty = 2)
lines(xx2, tangentline(b = 11.01, m = 0.97, x = xx2), col = grays[22], lwd = segmentlinewidth, lty = 2)

# Indiff curves
for (u in c(1, 20, 50) ) {
  lines(xx1, indiff(u, xx1, c = 2.7), col = COLB[4], lwd = graphlinewidth, lty = 1)  
}

# Points + Labels
points(3, indiff(1, 3, c = 2.7), pch = 16, col = "black", cex = 1.5)
text(3 + 0.2, indiff(1, 3, c = 2.7) - 0.5, expression(e), cex = labelsize) # e
points(9, indiff(1, 9, c = 2.7), pch = 16, col = "black", cex = 1.5)
text(9 + 0.2, indiff(1, 9, c = 2.7) - 0.5, expression("e'"), cex = labelsize) # e'

points(3, indiff(20, 3, c = 2.7), pch = 16, col = "black", cex = 1.5)
text(3 + 0.2, indiff(20, 3, c = 2.7) - 0.5, expression(f), cex = labelsize) # f
points(9, indiff(20, 9, c = 2.7), pch = 16, col = "black", cex = 1.5)
text(9 + 0.2, indiff(20, 9, c = 2.7) - 0.5, expression("f'"), cex = labelsize) # f'

points(3, indiff(50, 3, c = 2.7), pch = 16, col = "black", cex = 1.5)
text(3 + 0.2, indiff(50, 3, c = 2.7) - 0.5, expression(g), cex = labelsize) # g
points(9, indiff(50, 9, c = 2.7), pch = 16, col = "black", cex = 1.5)
text(9 + 0.2, indiff(50, 9, c = 2.7) - 0.5, expression("g'"), cex = labelsize) # g'

#Label value functions
text(0.25, 1.65, expression(u[1]), cex = labelsize)
text(6, 3.8, expression("very risk"), cex = labelsize)
text(6, 3, expression("averse"), cex = labelsize)

text(0.25, 8, expression(u[2]), cex = labelsize)
text(6, 9.2, expression("risk"), cex = labelsize)
text(6, 8.4, expression("averse"), cex = labelsize)


text(0.25, 14.25, expression(u[3]), cex = labelsize)
text(6, 14.5, expression("almost risk"), cex = labelsize)
text(6, 13.7, expression("neutral"), cex = labelsize)


dev.off()

