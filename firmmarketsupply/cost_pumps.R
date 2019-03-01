# To Do: See Scott's notes. 
# Filler file for later.

require(shape)
pdf(file = "firmmarketsupply/cost_pumps.pdf", width = 7, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

# Correct the functional forms
varSpeedDrive <- function(gal){ 
  gal + 2
}
  
gearPump <- function(gal){
  gal + 1
}


xlims <- c(0, 300)
ylims <- c(0, 300)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

#Customize ticks and labels for the plot
ticksy <- seq(ylims[1], ylims[2], 100)
ylabels <- seq(ylims[1], ylims[2], 100)
ticksx <- seq(xlims[1], xlims[2], 80)
xlabels <- seq(xlims[1], xlims[2], 80)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Label axes
mtext(expression(paste("Relative Cost, Dollars ", C)), side=1, line = 2.5, cex = axislabelsize)
text(-100, 0.5*ylims[2], expression(paste("Capacity, gal", K )), xpd = TRUE, cex = axislabelsize, srt = 90) 




#Label axes



#Vectors for points



#Vectors for labels



dev.off()