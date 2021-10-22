require(shape)
library(extrafont)
library(pBrackets)
library(emojifont)
library(gridSVG)
load.emojifont('OpenSansEmoji.ttf')

pdf(file = "random/bertrand_unit_pixel.pdf", width = 6, height = 3.5)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(0.5, 1, 0.5, 1))

xlims <- c(0, 6)
ylims <- c(0.5, 1.5)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
#quartz()

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

ticksy <- seq(from = 0, to = 6, by = 1)
ylabels <- seq(from = 0, to = 10, by = 1)
ticksx <- c(xlims[1], 3, 4, xlims[2])
xlabels <- c(0, NA, NA, 1)
axis(1, at = ticksx, pos = 1, labels = xlabels, cex.axis = labelsize)
#axis(2, at = NA, pos = 0, labels = NA, las = 0, cex.axis = labelsize)


text(3, 0.88, expression(paste(frac(1,2))), 
     xpd = TRUE, srt = 0, cex = labelsize)
# text(4, 0.88, expression(paste(frac(1,2) + Delta*p)), 
#      xpd = TRUE, srt = 0, cex = labelsize)
text(4, 0.9, labels=emoji("neutral_face"), family="OpenSansEmoji", cex=2.9)

brackets(x1 = 4, y1 = 1.03, x2 = 6, y2 = 1.03,  
         ticks = 0.5, curvature = 0.5, type = 1, 
         col = "black", lwd = 2, lty = 1, xpd = TRUE)
text(5, 1.2, expression(paste("Demand for Pixels")), 
     xpd = TRUE, srt = 0, cex = annotatesize)
# text(5, 1.23, expression(paste(Q[2](p[1],p[2])  == frac(1,2) - Delta*p)), 
#      xpd = TRUE, srt = 0, cex = annotatesize)
#== 1 - Q[1](p[1],p[2]), phantom()

brackets(x1 = 4, y1 = 0.83, x2 = 0, y2 = 0.83,  
         ticks = 0.5, curvature = 0.5, type = 1, 
         col = "black", lwd = 2, lty = 1, xpd = TRUE)
text(2, 0.67, expression(paste("Demand for iPhones")), 
     xpd = TRUE, srt = 0, cex = annotatesize)
# text(2, 0.555, expression(paste(Q[1](p[1],p[2]) == frac(1,2) + Delta*p)), 
#      xpd = TRUE, srt = 0, cex = annotatesize)


text(2.7, 1.48, expression(paste("When Pixels are more expensive than iPhones")), 
     xpd = TRUE, srt = 0, cex = annotatesize)
text(0.9, 1.4, expression(paste(Delta*p == p[2] - p[1], phantom() > 0)), 
     xpd = TRUE, srt = 0, cex = annotatesize)


text(4.34, 0.5, labels=emoji("neutral_face"), family="OpenSansEmoji", cex=1, xpd = TRUE)
text(5.3, 0.5, expression(paste("=indifferent consumer")), 
     xpd = TRUE, srt = 0, cex = 1)

dev.off()

