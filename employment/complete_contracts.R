require(ggplot2)
require(shape)
require(plotrix)
pdf(file = "employment/complete_contracts_figure.pdf", width = 9, height = 7)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

Fn <- function(t, ubar = 0.4, b = 1.8) {
  (ubar)/(t) + b
}

#COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
COL <- c("#238b45","#7fc97f", "#beaed4", "#fdc086", "#ffff99")
par(mar =  c(4, 5, 3, 7))
xlims <- c(0, 1)
ylims <- c(2,5)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste(""), cex = axislabelsize),
     ylab = expression(paste(""), cex = axislabelsize),
     xaxt = "n", yaxt = "n", cex.lab = axislabelsize, bty = "n")

npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(0, 1, length.out = npts)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
xx4 <- seq(xlims[1], 25, length.out = npts2)

#Draw the lines for the graphs
lines(xx1, Fn(xx1), col = COL[1], lwd = graphlinewidth)
#lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 4)
#lines(xx2, solowInfeas(xx2, delta = 5), col = COL[1], lwd = 4, lty = 2)

#Customize ticks and labels for the plot
ticksy <- c(0,2.2,4.5,5)
ylabels <- c(0, NA, NA, NA)
ticksx <- c(0,0.15,1)
xlabels <- c(0, NA, 1)
axis(1, at = ticksx, pos = 2, labels = xlabels, cex.axis = labelsize)

axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

axis.break(axis=2,breakpos=2.1,pos=0,bgcol="white",breakcol="black",
           style="slash",brw=0.02)

#Segments
segments(0, 2.2, 1.1, 2.2, lty = 2, lwd = 1.5, col = "darkgray", xpd = TRUE)
segments(0.15, 2, 0.15, 4.5, lty = 2, lwd = 1.5, col = "darkgray")
segments(0,4.5,1.1,4.5,lty = 2, lwd = 1.5, col = "darkgray", xpd = TRUE)
segments(1, 2.2, 1,4.5, lty = 2, lwd = 1.5, col = "darkgray")

#Axis tick labels
#y
text(-0.1, 4.5, expression(paste(gamma[p]==1)), cex = labelsize, xpd = TRUE)
#text(-0.05, 0.8, expression(paste(p^B)), cex = labelsize, xpd = TRUE)
#x
text(0.15, 1.85, expression(paste(t[j])), cex = labelsize, xpd = TRUE)
#text(1, -0.07, expression(paste(1)), cex = labelsize, xpd = TRUE)

#Text annotations
text(0.22, 5, expression(paste(w== frac(underline(u),t[j])+b)), cex = labelsize, xpd = TRUE)
text(1.15, 2.2, expression(paste(u+b)), cex = labelsize, xpd = TRUE)
text(1.15, 4.5, expression(paste(gamma[p])), cex = labelsize, xpd = TRUE)
text(0.3, 2.5, expression(paste("Employee's rent")), cex = labelsize, xpd = TRUE)
text(0.6, 3.5, expression(paste("Employer's profit")), cex = labelsize, xpd = TRUE)

# #Arrows
Arrows(0.3, 2.6, .3, 3, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
Arrows(0.3, 2.4, 0.3, 2.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
Arrows(.6, 3.6, 0.6, 4.4, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
Arrows(.6, 3.4, 0.6, 2.6, col = "black", lty = 1, lwd = 2, arr.type = "triangle")

#Axes labels
text(0.5, 1.6, expression(paste("Degree of contractual completeness")), xpd = TRUE, cex = axislabelsize) 



dev.off()

