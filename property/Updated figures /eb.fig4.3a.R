require(shape)
#pdf(file="eb.fig4.3aSTEP1.pdf", width = 9, height = 7)
#pdf(file="eb.fig4.3aSTEP2.pdf", width = 9, height = 7)
pdf(file="eb.fig4.3a.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1


COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

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

#Add arrows and names:
arrows(-.8, 10, -.8, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(6.3, -1.6, 9, -1.6, xpd = TRUE, length=0.1,angle=40,lwd=3)
mtext("Ayanda", side=1, line=3, at=0, col=COLA[4])

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
text(-.8, 7, expression(paste("B's data (gigabytes) ,",y^B )), xpd = TRUE, cex = axislabelsize, srt = 270) 

#Add arrows and names:
arrows(-.8, 10, -.8, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
arrows(6.3, -1.8, 9, -1.8, xpd = TRUE, length=0.1,angle=40,lwd=3)
mtext("Bongani", side=3, line=3, at=0, col= COLB[2])

COLB <- c("#fc9272", "#fb6a4a", "#ef3b2c","#cb181d", "#99000d")

#Add a point for a feasible allocation
points(1, 14, pch = 16, col = "black", cex = 1.5)

#Annotating the feasible allocation
text(.8, 13.5, expression(z))

dev.off()

