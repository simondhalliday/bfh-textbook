require(ggplot2)
require(shape)
require(plotrix)
pdf(file = "employment/continuous_effort.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 3

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

WageFn <- function(H, delta = 5) {
  delta /(1 - H)
}

IndiffFn <- function(e, alpha = 2, u = 10) {
  u + alpha*(1/(1 - e) - e)
}

brfFn <- function(e, alpha = 2, h = 0.05, b = 1) {
  (-b -e^2*b + 2*e*b + h*alpha^2 - e^2*h*alpha + 2*e*h*alpha - h*alpha - alpha^2 + e^2*alpha - 2*e*alpha + alpha)/((e - 1)^2*(h - 1))
}

brfFn2 <- function(w, alpha = 0.125, b = 4, h = 0.1){
  1 - (alpha/(w - b/(1-h) + alpha))^(0.5)
}

isovlow <- function(w, alpha = 0.125, u = 6){
  (-sqrt(-3*alpha^2 - 2*alpha*(u - w) + (u - w)^2) + alpha + u - w)/(2*alpha)
}

isovhigh <- function(w, alpha = 0.125, u = 6){
  (sqrt(-3*alpha^2 - 2*alpha*(u - w) + (u - w)^2) + alpha + u - w)/(2*alpha)
}

solowCondition <- function(w, delta = 5){
  (w*(1/(8*delta)))
}


#COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")
par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 10)
ylims <- c(0, 1.2)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("Wage, ", w)),
     ylab = expression(paste("Effort, ", e)),
     xaxt = "n", 
     yaxt = "n",
     cex.lab = axislabelsize, 
     line = 2.5,
     bty = "n", 
     xaxs="i", 
     yaxs="i")

npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
#xx1 <- seq(xlims[1], 0.99, length.out = npts)
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
xx4 <- seq(xlims[1], 25, length.out = npts2)

#Draw the lines for the graphs
#lines(xx1, IndiffFn(xx1, alpha = 2), col = COLA[2], lwd = 4)
lines(xx1, brfFn2(xx1), col = COLA[2], lwd = 4)
lines(xx1, isovlow(xx1), col = COLB[2], lwd = 4)
lines(xx1, isovhigh(xx1), col = COLB[4], lwd = 4)
lines(xx2, solowCondition(xx2, delta = 1), col = COL[3], lwd = 4)
#lines(xx2, solowInfeas(xx2, delta = 5), col = COL[1], lwd = 4, lty = 2)

#Customize ticks and labels for the plot
ticksy <- c(0, 2.5, 5, 20,  40)
ylabels <- c(0, expression(paste(b)), expression(paste(b+a)), expression(paste(w[0])), NA)
ticksx <- c(0, 0.75, 1, xlims[2])
xlabels <- c(0, expression(paste(H,"*")), 1.0, NA)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Annotation of the  graphs
text(0.72, 35, expression(paste("Wage Curve ", w,"*",(H))))

#Line for the absolute maximum quality
#segments(1, 0, 1, 42, lty = 2, lwd = 3, col = "darkgray")
segments(0.75, 0, 0.75, 20, lty = 2, lwd = 2, col = "darkgray")

#Arrow to Slope of BRF
#Arrows(0.5, 13, 0.58, 13, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#text(0.35, 13, expression(paste("Slope = " - u[q], " = ", frac(delta, (1 - q)^2))))


Arrows(0.8, 15, 0.8, 19, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
Arrows(0.8, 15, 0.8, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(0.92, 12.5, expression(paste("Employment Rent")))

#Text to indicate delta = 5
#text(0.2, 38, expression(paste("Wage Function")))
#text(0.2, 36, expression(paste("set to ", delta, " = 5")))


#Zero profit condition 
segments(0, 20, 0.75, 20, lty = 1, lwd = 2, col = "darkgray")
segments(0.75, 20, 1.2, 20, lty = 2, lwd = 2, col = "darkgray")

#Unemployment benefits & a
segments(0, 5, 1.2, 5, lty = 2, lwd = 2, col = "darkgray")
segments(0, 2.5, 1.2, 2.5, lty = 2, lwd = 2, col = "darkgray")

#Zero profit condition
text(1.02, 21, expression(paste("Zero profit condition, ", w == w[0])))
text(0.97, 6, expression(paste(b + a)))
text(0.97, 3.5, expression(paste(b, " (unemployment benefits)")))
#text(1.08, 36, expression(paste("level of")))
#text(1.08, 34, expression(paste("employment, ", bar(H))))



dev.off()

