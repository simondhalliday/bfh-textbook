require(shape)
require(pBrackets)
pdf(file = "coordination_failures/mb_mc_dynamics.pdf", width = 9, height = 7)

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
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")
Grays <- gray.colors(25, start = 1, end = 0)

#Need to create a stacked graph and 
#use the option mfrow = c(2,1) for that
par(mar =  c(5, 7, 1, 3))

indiffA <- function(ea, uA = 46.08) {
  uA + (1/2)*(ea)^2
}

output <- function(ea, eb = 12, alpha = 30, beta = 1/2){
  (alpha - beta*(ea + eb))*ea
}

uA <- function(ea, ya = output) {
  ya - 0.5*(ea)^2
}

mrsA <- function(ea){
  ea
}

slopeline <- function(ea, yint = 0.5, slope = 2){
  yint + slope*ea
}


xlims <- c(0, 24)
ylims <- c(0, 24)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
#Notice ylims[2] is lower to reduce length of indifference curves
#so they don't overlap with labels
y <- seq(ylims[1], ylims[2] - 20, length.out = npts) 
a <- c(uA(ea = 12, output(ea = 12, eb = 12)), uA(ea = 15, output(ea = 15, eb = 0)), 300)


MBenefit <- function(ea, eb = 12, alpha = 30, beta = 1/2) {
  (alpha - beta*eb - 2*beta*ea)
}

MCost <- function(ea, slope = 1, intercept = 0){
  slope*ea
}


xlims <- c(0, 24)
ylims <- c(0, 24)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(12, 46.08, 90)



plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")


ticksy <- c(0, MBenefit(12), MBenefit(ea = 0), 30, ylims[2])
ylabels <- c(NA, expression(paste(12)), expression(paste(24)), expression(paste(30)), NA)

ticksx <- seq(xlims[1], xlims[2], 3)
xlabels <- seq(xlims[1], xlims[2], 3)

axis(1, at = ticksx, pos = 0, labels = xlabels ,las = 1, cex.axis = labelsize - 0.05)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize - 0.05)

text(0.5*xlims[2], -3, expression(paste("A's hours, ", h^A)), xpd = TRUE, cex = axislabelsize)
text(-3, 0.5*ylims[2], expression(paste("A's marginal costs & benefits (lb)")), xpd = TRUE, cex = axislabelsize, srt = 90) 

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(4, 8, length.out = npts)
xx3 <- seq(xlims[1], 24, length.out = npts)

lines(xx3, MBenefit(xx3, eb = 12), col = COLA[6], lwd = graphlinewidth)
#lines(xx3, MBenefit(xx3, eb = 0), col = COLA[3], lwd = graphlinewidth)
lines(xx1, MCost(xx1), col = COLB[4], lwd = graphlinewidth)

#For NE hours = 12

segments(12, 0, 12, 12, lty = 2, col = grays[20], lwd = segmentlinewidth, xpd = 350)
#segments(0, 12, 12, 12, lty = 2, col = grays[20], lwd = segmentlinewidth, xpd = 350)
#segments(6, 0, 6, MBenefit(6), lty = 2, col = grays[20], lwd = segmentlinewidth, xpd = 350)
#segments(18, 0, 18, MCost(18), lty = 2, col = grays[20], lwd = segmentlinewidth, xpd = 350)
#text(20, 16, expression(mb == mc), cex = annotatesize)


# text(4.2, 31, expression(paste("Marginal benefit")), cex = labelsize, xpd = TRUE)
# text(4.2, 29.5, expression(paste("when, ", h^B == 0 )), cex = labelsize, xpd = TRUE)

text(4.7, 24, expression(paste("Marginal benefit ")), cex = labelsize, xpd = TRUE)
text(4.7, 22.5, expression(paste("when ", h^B == 12 )), cex = labelsize)


text(20, 24, expression(paste("Marginal disutility")), cex = labelsize, xpd = TRUE)
text(20, 22.5, expression(paste(mc^A == h^A)), cex = labelsize)




points(12, 12, pch = 16, col = "black", cex = 1.5)
text(12 + 0.85, 12.1, expression(paste(n)), cex = labelsize)

# points(15, 15, pch = 16, col = "black", cex = 1.5)
# text(15 + 0.85, 15.1, expression(paste(s)), cex = labelsize)

brackets(x1 = 6 - 0.2, y1 = MCost(6), x2 = 6 - 0.2, y2 = MBenefit(6),  
         ticks = 0.5, curvature = 0.5, type = 1, h = 1,  
         col = "black", lwd = 2, lty = 1, xpd = TRUE)
text(2.5, 12.6, expression(paste(mb^A > mc^A)), xpd = TRUE, cex = annotatesize)
text(2.5, 11.4, expression(paste("at 6 hours")), xpd = TRUE, cex = annotatesize)

brackets(x1 = 18 + 0.2, y1 = MCost(18), x2 = 18 + 0.2, y2 = MBenefit(18),  
         ticks = 0.5, curvature = 0.5, type = 1, h = 1,  
         col = "black", lwd = 2, lty = 1, xpd = TRUE)
text(21.5, 12.6, expression(paste(mb^A < mc^A)), xpd = TRUE, cex = annotatesize)
text(21.5, 11.4, expression(paste("at 18 hours")), xpd = TRUE, cex = annotatesize)


dev.off()
