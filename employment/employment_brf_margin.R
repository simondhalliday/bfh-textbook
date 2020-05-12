require(shape)
pdf(file = "employment/employment_brf_margin.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

#The equation is below when v = 0. See Wolfram Alpha output. 
isov <- function(w, delta = 5) {
}

isovhigh1 <- function(w, delta = 5, v = 5){
  (sqrt(w^2 - 4 * delta * v) - w + 2*v)/(2 * v )
}

isovlow1 <- function(w, delta = 5, v = 5){
  (-sqrt(w^2 - 4 * delta * v) - w + 2*v)/(2 * v )
}

isovhigh2 <- function(w, delta = 5, v = 15){
  (sqrt(w^2 - 4 * delta * v) - w + 2*v)/(2 * v )
}

isovlow2 <- function(w, delta = 5, v = 15){
  (-sqrt(w^2 - 4 * delta * v) - w + 2*v)/(2 * v )
}

isovhigh3 <- function(w, delta = 5, v = 20){
  (sqrt(w^2 - 4 * delta * v) - w + 2*v)/(2 * v )
}

isovlow3 <- function(w, delta = 5, v = 20){
  (-sqrt(w^2 - 4 * delta * v) - w + 2*v)/(2 * v )
}

brfFn <- function(w, delta = 5) {
  1 - (2*delta) / w
}

#This is evaluated for p = 12; q = 12/(8*sqrt(12^2 - 4*delta1*v1)) - 1/8 for the slope
#At p = 12; q = (-sqrt(12^2 - 4*v1*delta1) - 12 + 2*v1)/(2*v1) = 0.799
tangencyLine <- function(w){
  (w*(0.05))
}

solowCondition <- function(w, delta = 5){
  (w*(1/(8*delta)))
}


COL <- c("#f7fcf5", "#e5f5e0", "#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#005a32")
# COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0)

par(mar =  c(5, 5, 1, 1))
xlims <- c(0, 60)
ylims <- c(0, 1)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("Wage, ", w)),
     ylab = expression(paste("Effort, ", e)),
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     line = 2.5,
     bty = "n", 
     xaxs = "i", 
     yaxs = "i")


npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx0 <- seq(5, xlims[2], length.out = npts)
xx1 <- seq(10, xlims[2], length.out = npts)
xx2 <- seq(5, 15, length.out = npts)
xx3 <- seq(10, xlims[2], length.out = npts2)
xx4 <- seq(xlims[1], 10, length.out = npts2)
#For below, I solved sqrt(p^2 - 200) - p + 200/p = 0, getting p = 10*sqrt(2) = 14.14214
xx5 <- seq( 18.43909, xlims[2], length.out = npts2)
xx6 <- seq( 18.43909, 25, length.out = npts2)
xx7 <- seq(20, xlims[2], length.out = npts2)
xx8 <- seq(xlims[1], 25, length.out = npts2)
xx9 <- seq(xlims[1], xlims[2], length.out = npts2)

#Draw the lines for the graphs
lines(xx1, brfFn(xx1), col = COLA[5], lwd = graphlinewidth)


#Customize ticks and labels for the plot
ticksy <- c(0, 1)
ylabels <- c(0, 1)
ticksx <- c(0, 10,xlims[2])
xlabels <- c(0, expression(paste(w == 2, underline(u))), NA)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

# text(24.8, 0.05, expression(paste(v[N])))
text(45, 0.66, expression(paste("Employee's ICC, or")), cex = labelsize)
text(45, 0.605, expression(paste("best-response")), cex = labelsize)
text(45, 0.55, expression(paste("function, ", e(w))), cex = labelsize)

dev.off()