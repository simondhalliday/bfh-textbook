#Graph Designer: Simon Halliday & Scott Cohn
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "capitalism/workerwage_pref_riskaversion.pdf", width = 10, height = 8)


# Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(4, 5, 1, 1))


pref <- function(stdev = xx1, r, k = 16.5, h = - 2.5){
  sqrt( -k^2 + 2*k*stdev + r^2 - stdev^2) + h
}

# Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 20)
xlims <- c(0, 20)

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

ticksx <- c(xlims[1], xlims[2])
xlabels <- c(0, NA)
#expression(paste(Delta))

ticksy <- c(ylims[1], ylims[2])
ylabels <- c(NA, NA)
#expression(paste("y"))

axis(1,at = ticksx,  pos = 0, labels = xlabels)
axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)

# Axis labels and draw linear utility function
mtext(expression(paste("Standard deviation of income ,", sigma)), side = 1, line = 2.5, cex = axislabelsize)
text(-1.5, 0.5*ylims[2], expression(paste("Expected value of income, ", mu)), xpd = TRUE, cex = axislabelsize, srt = 90) 


# Lines 
lines(xx1, pref(r = 12), col = COLB[4], lty = 1, lwd = graphlinewidth)
lines(xx1, pref(r = 18), col = COLA[4], lty = 1, lwd = graphlinewidth)


# Labels
# Segments


# Points


dev.off()