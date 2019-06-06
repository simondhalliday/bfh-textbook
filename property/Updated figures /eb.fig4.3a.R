require(shape)
pdf(file = "bfh-textbook/property/property_fig4.3a.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1


#COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
#COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
par(mar =  c(6, 4, 4, 4))
xlims <- c(0, 10)
ylims <- c(0, 15)
xlims2 <- c(10, 0)
ylims2 <- c(15, 0)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("A's coffee (kilograms), ", x^A)),
     ylab = expression(paste("A's data (gigabytes), ", y^A)), 
     line = 2.5,
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")


npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
#xx2 <- seq(xlims[1], xlims[2], length.out = npts)
#xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
#xx4 <- seq(xlims[1], 10, length.out = npts2)


#Customize ticks and labels for the plot
ticksy <- seq(from = 0, to = 15, by = 1)
ylabels <- seq(from = 0, to = 15, by = 1)
ticksx <- seq(from = 0, to = 10, by = 1)
xlabels <- seq(from = 0, to = 10, by = 1)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0)

#Add arrows:
arrows(-1.3, 10, -1.3, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(7.2, -1.6, 9, -1.6, xpd = TRUE, length=0.1,angle=40,lwd=3)


#Set up second axes and labels

par(new = TRUE)
#par(mar =  c(6, 4, 4, 4))

xlims2 <- c(10, 0)
ylims2 <- c(15, 0)

plot(0, 0, xlim = xlims2, ylim = ylims2, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")),
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = 1.3, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

axis(side = 3, at = ticksx, pos = 0, labels = xlabels)
axis(side = 4, at = ticksy, pos = 0, labels = ylabels, las = 0)
#axis(side=3, at = xlims2, pos = 0))
mtext(expression(paste("B's coffee (kilograms) ,",x^B )), side=3, line = 2.5, cex = axislabelsize)
#axis(side = 4, at = ylims2, pos = 0)
text(-1.3, 7, expression(paste("B's data (gigabytes) ,",y^B )), xpd = TRUE, cex = axislabelsize, srt = 270) 

#Add arrows:
arrows(-1.3, 10, -1.3, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(7.2, -1.8, 9, -1.8, xpd = TRUE, length=0.1,angle=40,lwd=3)

COLB <- c("#fc9272", "#fb6a4a", "#ef3b2c","#cb181d", "#99000d")

#Add a point for the initial endowment
points(1, 14, pch = 16, col = "black", cex = 1.5)

#Annotating B's endowment
text(.8, 13.5, expression(z))

dev.off()
