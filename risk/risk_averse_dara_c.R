#' Graph Designer: Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library("shape")

pdf(file = "risk/risk_averse_dara_c.pdf", width = 9, height = 7)

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
ylims <- c(0, 40)


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

ticksx <- c(0, 0.5*xlims[2], xlims[2])
xlabels <- c(NA, expression(paste(Delta)), NA)
ticksy <- c(0, 6.5, 16, 29, ylims[2])
ylabels <- c(NA, expression(paste(hat(y)^{L})), expression(paste(hat(y)^{M})), expression(paste(hat(y)^{H})), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#Axis labels and draw linear utility function
mtext(expression(paste("Difference in wealth (good versus bad outcome), ", Delta, ", risk")), side = 1, line = 2.5, cex = axislabelsize)
text(-1, 0.5*ylims[2], expression(paste("Expected wealth, ", hat(y))), xpd = TRUE, cex = axislabelsize, srt = 90) 

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(2, 10, length.out = npts)



for (u in c(19, 60, 170) ) {
  lines(xx1, indiff(u, xx1, c = 2.7), col = COLB[4], lwd = graphlinewidth, lty = 1)  
}





dev.off() 
