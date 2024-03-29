library(shape)
pdf(file = "indmarketdemand/price_increase_sugar.pdf", width = 9, height = 7)

#Set parameters for graphics
pointsize <- 1.8
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

par(mar =  c(4, 9, 1, 1))

indiffA <- function(x, alpha = 0.5, uA = 5) {
  (uA / x^alpha)^(1/(1 - alpha))
}

cef <- function(x, e = 1.4, k = 44.7982){
  k*(x^(-1/e))
}

#The inverse demand function to creat the relative points
cefinv <- function(p, e = 1.4, k = 44.7982){
  (p/k)^(-e)
}

xlims <- c(0, 250)
ylims <- c(0, 2.5)

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
     xaxs = "i", 
     yaxs = "i")


## Paremeters to enable editing points 
## I've assumed a tax of 20% to make this more visible 
## I'll comment on that in the caption
tax <- 1.2 #20 percent tax
prices <- c(1.25, 1.25*tax) 
xprices <- c(cefinv(prices[1]), cefinv(prices[2]))

ticksy <- c(0, prices[1], prices[2], ylims[2])
ylabels <- c(NA, expression(paste(p[0] == phantom(),"$1.25")), expression(paste(p[1] == p[0] + Delta*p^tau)), NA)
ticksx <- c(0, xprices[2], xprices[1], xlims[2])
xlabels <- c(NA, expression(paste(X[1]) == 108), expression(paste(X[0]) == 150), NA)

text(-23, prices[2] - 0.09, expression(paste(phantom() == phantom(),"$1.50")), xpd = TRUE, cex = labelsize - 0.2)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize - 0.2)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize - 0.2)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(48, xprices[2], length.out = npts)
xx3 <- seq(xprices[2], xprices[1], length.out = npts)

#Label axes
mtext(expression(paste("Liters of sugary drinks, ", X)), side = 1, line = 2.5, cex = axislabelsize)
text(-56, 0.5*ylims[2], expression(paste("Price per liter, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 

# Segments
segments(xprices[1], 0, xprices[1], prices[1], lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0,  prices[1], xprices[1],  prices[1], lty = 2, col = "gray" , lwd = segmentlinewidth)

segments(0, prices[2], xprices[2], prices[2], lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(xprices[2], 0, xprices[2], prices[2], lty = 2, col = "gray" , lwd = segmentlinewidth)

# Demand Curve
lines(xx1, cef(xx1), col = COLA[5], lwd = graphlinewidth)
text(xlims[2] - 0.1*xlims[2], 0.78, expression(Demand), cex = annotatesize)

# Points
points(xprices[1], prices[1], pch = 16, col = "black", cex = 1.5)
text(xprices[1] + 5, prices[1] + 0.05, expression(a), cex = labelsize)

points(xprices[2], prices[2], pch = 16, col = "black", cex = 1.5)
text(xprices[2] + 5, prices[2] + 0.05, expression(b), cex = labelsize)

# Label Regions
# text(125, 1.35, expression(A), cex = labelsize)
# text(50, 1, expression(B), cex = labelsize)
# text(50, 1.35, expression(C), cex = labelsize)
# text(50, 1.75, expression(D), cex = labelsize)




dev.off()