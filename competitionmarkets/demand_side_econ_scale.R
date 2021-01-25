require(shape)
pdf(file = "competitionmarkets/demand_side_econ_scale.pdf", width = 9, height = 7)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

underlined <- function(x, y, label, ...){ 
  text(x, y, label, ...) 
  sw <- strwidth(label) 
  sh <- strheight(label) 
  lines(x + c(-sw/2, sw/2), rep(y - 1.5*sh/2, 2)) 
} 

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
Grays <- gray.colors(25, start = 1, end = 0)

par(mar =  c(5, 5, 1, 3))

Demand <- function(x, rmax = 20, xmax = 12, n = 10) {
  rmax - (rmax/(n*xmax))*x
}

mrsA <- function(x, rmax = 20, xmax = 10) {
  rmax - (rmax/xmax)*x
}

uA <- function(x, y, rmax = 20, xmax = 10) {
  y + rmax*x - (1/2)(rmax/xmax)*x^2
}


#Supply <- function(x, c1 = 0, c2 = 0.05){
#  c1 + 2*c2*x
#}

# new supply curve
Supply <- function(x, c1 = 3, c2 = 0.0465){
  c1 + 2*c2*x
}

xlims <- c(0, 130)
ylims <- c(0, 22)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(46.08, 55, 64)
b <- c(46.08, 55, 64)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
# ticksx <- seq(from = 0, to = xlims[2], by = 2)
# xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(0, 3, ylims[2])
#ylabels <- c(NA,  expression(paste(mc(X^{DH}))), expression(paste(p,"*")), expression(paste(p^H)), expression(paste(bar(p) )), NA)
ylabels <- c(NA,  expression(paste(underline(p))), NA)
ticksx <- c(0, xlims[2])
#xlabels <- c(NA, expression(paste(X^{DH})), expression(paste(X,"*")), expression(paste(X^{SH})), expression(paste(bar(p)/beta )), NA)
xlabels <- c(NA, NA)

axis(1, at = ticksx, pos = 0, labels = FALSE, cex.axis = labelsize)
text(x = c(0, 52, 75, 103, 127, xlims[2]), par("usr")[3] - 0.5, labels = xlabels, srt = 0, pos = 1, xpd = TRUE, cex = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(75, xlims[2], length.out = npts)

lines(xx1, Supply(xx1), col = COLB[4], lwd = graphlinewidth)
lines(xx2, Supply(xx2), col = COLB[4], lty = 2, lwd = segmentlinewidth)


#Label axes
mtext(expression(paste("Market quantity of output, ", X)), side=1, line = 2, cex = axislabelsize)
text(-12, 0.5*ylims[2], expression(paste("Price per unit of x, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 
# 
# points(50, 10, pch = 16, col = "black", cex = 1.5)
# text(52, 10.5, expression(M))

#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
text(105, 16, expression("Demand curve"), cex = labelsize)
text(105, 14.5, expression(paste(p(X) == underline(p) + beta*X)), cex = labelsize)


dev.off()
