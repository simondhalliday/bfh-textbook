pdf(file = "firmmarketsupply/ss_st.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

xlims <- c(0, 20)
ylims <- c(0, 20)

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

ticksy <- c(0, ylims[2])
ylabels <- c(0, ylims[2])
ticksx <- c(0, xlims[2])
xlabels <- c(0, xlims[2])

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

cost <- function(x = xx1, a = 18){
  a*x^(-0.43)
}

# Cost Func.
lines(xx1, cost(), col = COLB[4], lwd = graphlinewidth)

# Label Cost Func
text(5, 7, expression(paste("Stainless Steel")), cex = labelsize)
text(5, 6, expression(paste("Storage Tank")), cex = labelsize)

text(19, 4, expression(paste(C(x))), cex = labelsize)

# Point
points(5, cost(5), pch = 16, col = "black", cex = 1.5)
text(5.5, cost(5)+0.5, expression(a))

# Label x,y axis
mtext(expression(paste("Capacity, Gal")), side=1, line = 2.5, cex = axislabelsize)
text(-1.4, 0.5*ylims[2], expression(paste("Purchased Cost, Dollars")), xpd = TRUE, cex = axislabelsize, srt = 90) 


dev.off()