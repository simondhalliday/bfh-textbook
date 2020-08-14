library(shape)
library(pBrackets)

pdf(file = "public_mechanism/smoking_MU_MC_initial.pdf", width = 9, height = 7)

# Set parameters for graphics
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
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442","#D55E00")

par(mar =  c(3, 5, 3, 1))

MU <- function(Xi, Xj, alpha = 10, beta = 2){
  ((alpha+beta*Xj)/Xi) 
}

xlims <- c(0, 6)
ylims <- c(-1.1, 12)

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

ticksy <- c(0, 4,  ylims[2])
ylabels <- c(NA, expression(paste(p[0])),  NA)
ticksx <- c(0, 5, xlims[2])
xlabels <- c(NA, expression(paste(x[a]^A)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

text(0.5*xlims[2], -1.5, expression(paste("A's smoking, ", x^A)), xpd = TRUE, cex = axislabelsize) 
text(-.6, 0.5*ylims[2], expression(paste("Benefits and costs, $")), xpd = TRUE, cex = axislabelsize, srt = 90) 

#text(0.5, 11.5, expression(paste(u[A],"(",x[A],",",x[Bt],")")), xpd = TRUE, cex = labelsize) 
text(2.3, 11.5, expression(paste(mb^A,"(",x^A,",",x[a]^B,")")), xpd = TRUE, cex = labelsize) 


npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#segments(1.25, 0, 1.25, 10, lty = 2, col = grays[20], lwd = segmentlinewidth)
#segments(2, 0, 2, 10, lty = 2, col = grays[20], lwd = segmentlinewidth)
segments(5, 0, 5, 4, lty = 2, col = grays[20], lwd = segmentlinewidth)


lines(xx1, MU(xx1, Xj=5), col = CBCols[1], lwd = graphlinewidth)
#lines(xx1, MU(xx1, Xj=1.25), col = COLA[4], lwd = graphlinewidth)

segments(0, 4, xlims[2], 4, lty = 1, col = CBCols[2] , lwd = graphlinewidth)
#segments(0, 10, xlims[2], 10, lty = 1, col = COLB[3] , lwd = segmentlinewidth)

# text(5.6, 11, expression(paste("Price")), cex = labelsize)
# text(5.6, 10.3, expression(paste("after tax")), cex = labelsize)

text(5.6, 5, expression(paste("Initial")), cex = labelsize)
text(5.6, 4.3, expression(paste("price")), cex = labelsize, xpd = TRUE)

# brackets(2, -1, 1.25, -1, h = .25,  ticks = 0.5, curvature = 0.5, type = 1,
#          col = 1, lwd = segmentlinewidth, lty = 1, xpd = TRUE)
# 
# brackets(5, -1, 2, -1, h = .3,  ticks = 0.5, curvature = 0.5, type = 1,
#          col = 1, lwd = segmentlinewidth, lty = 1, xpd = TRUE)
# 
# text(1.65, -2.5, expression(paste("Indirect \n effects")), cex = labelsize, xpd = TRUE)
# text(3.5, -2.5, expression(paste("Direct \n effect")), cex = labelsize, xpd = TRUE)
points(5, 4, pch = 16, col = "black", cex = 1.5)
text(5 - 0.1, 4 - 0.4, expression(paste(a)), cex = labelsize)


text(1.3, ylims[2] + 0.5, expression(paste("A's marginal benefits")), cex = labelsize, xpd = TRUE)

Arrows(5, 5.8, 5, 4.4, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5, xpd = TRUE)

text(5, 6, expression(paste(mb == mc)), cex = labelsize, xpd = TRUE)

dev.off()
