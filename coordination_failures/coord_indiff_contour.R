require(shape)
pdf(file = "coordination_failures/coord_indiff_contour.pdf", width = 9, height = 7)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(5, 9, 1, 1))

indiffA <- function(ea, uA = 46.08) {
  uA + (1/2)*(ea)^2
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

output <- function(ea, eb = 12, alpha = 30, beta = 1/2){
  (alpha - beta*(ea+eb))*ea
}

xlims <- c(0, 24)
ylims <- c(0, 500)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

a <- c(112, uA(ea = 12, output(ea = 12, eb = 12)), uA(ea = 15, output(ea = 15, eb = 0)), 300)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
ticksx <- seq(from = 0, to = xlims[2], by = 3)
xlabels <- seq(from = 0, to = xlims[2], by = 3)
#ticksy <- c(0, 1, 10, 12, 28, 30, 46.08, 70)
ticksy <- c(ylims[1], 112, 195, 255, ylims[2])
ylabels <- c(NA, expression(z), expression(paste(y[a]== 190)), expression(paste(y[b] == 247.5)), NA)
# ticksy <- c(ylims[1], 100, uA(ea = 12, output(ea = 12, eb = 12)), uA(ea = 15, output(ea = 15, eb = 0)), 300, ylims[2])
# ylabels <- c(NA, expression(z),  expression(paste(a)),expression(paste(b)), expression(paste()), NA)
# ticksx <- c(0, 6.9, 12, 9.6, 16, 24, 26)
# xlabels <- c(NA, expression(paste(e^A,"*")), expression(paste(1/2*beta)), expression(paste(e^{AN})), expression(paste(alpha)), expression(paste(1/beta)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize - 0.05)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis= labelsize - 0.05)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

contour(x, y,
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLB[4],
        lwd = graphlinewidth,
        levels = a,
        xaxs="i",
        yaxs="i",
        add = TRUE)



# lines(xx1, indiffA(xx1, uA = 1), col = COLA[4], lwd = graphlinewidth)
# lines(xx1, indiffA(xx1, uA = 10), col = COLA[4], lwd = graphlinewidth)
# lines(xx1, indiffA(xx1, uA = 30), col = COLA[4], lwd = graphlinewidth)
# lines(xx1, indiffA(xx1, uA = 46.08), col = COLA[4], lwd = graphlinewidth)

# lines(xx1, brfB(xx1, alpha = 16, beta = 1/24), col = COLB[4], lwd = graphlinewidth)
# lines(xx1, brfPEA(xx1, alpha = 16, beta = 1/24), col = COLA[4], lwd = graphlinewidth)
# lines(xx1, brfPEB(xx1, alpha = 16, beta = 1/24), col = COLB[4], lwd = graphlinewidth)


segments(10, 0, 10, ylims[2], lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(15, 0, 15, ylims[2], lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 195, 10, 195, ylims[2], lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 255, 15, 255, ylims[2], lty = 2, col = "gray" , lwd = segmentlinewidth)


#mtext(expression(paste("A's hours, ", h^A)), side=1, line = 3.2, cex = axislabelsize)
text(0.5*xlims[2], -55, expression(paste("A's hours, ", h^A)), xpd = TRUE, cex = axislabelsize)
text(-5, 0.5*ylims[2], expression(paste("Consumption, ", y^A)), xpd = TRUE, cex = axislabelsize, srt = 90)

#
text(3, 350, expression(paste("slope", phantom()==h^A, phantom() == 10)), cex = annotatesize)
Arrows(6, 350, 9.2, 350, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#
text(3, indiffA(ea = 15, uA = 301), expression(paste("slope", phantom()==h^B, phantom() == 15)), cex = annotatesize)
Arrows(6, indiffA(ea = 15, uA = 301), 14.2, indiffA(ea = 15, uA = 301), 
       col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the iso-welfare functions for the HG, Aisha
text(21, 310, expression(u[z]^A), cex = annotatesize)
text(21, 390, expression(u[1]^A), cex = annotatesize)
text(21, 470, expression(u[2]^A), cex = annotatesize)
text(17, 470, expression(u[3]^A), cex = annotatesize)

text(9.5, 210, expression(a), cex = annotatesize)
text(14.5, 270, expression(b), cex = annotatesize)



lowint <- c(248, 174, 92)

slopex1 <- seq(8.5,11.5,length.out = 200)
lines(slopex1, slopeline(slopex1, yint = 60, slope = mrsA(ea = 10)), col = "black", lty = 2, lwd = segmentlinewidth)
lines(slopex1, slopeline(slopex1, yint = lowint[1], slope = mrsA(ea = 10)), col = "black", lty = 2, lwd = segmentlinewidth)
lines(slopex1, slopeline(slopex1, yint = lowint[2], slope = mrsA(ea = 10)), col = "black", lty = 2, lwd = segmentlinewidth)
lines(slopex1, slopeline(slopex1, yint = lowint[3], slope = mrsA(ea = 10)), col = "black", lty = 2, lwd = segmentlinewidth)

highint <- c(186, 111, 30)
slopex2 <- seq(13.5,16.5,length.out = 200)
lines(slopex2, slopeline(slopex2, yint = -2, slope = mrsA(ea = 15)), col = "black", lty = 2, lwd = segmentlinewidth)
lines(slopex2, slopeline(slopex2, yint = highint[1], slope = mrsA(ea = 15)), col = "black", lty = 2, lwd = segmentlinewidth)
lines(slopex2, slopeline(slopex2, yint = highint[2], slope = mrsA(ea = 15)), col = "black", lty = 2, lwd = segmentlinewidth)
lines(slopex2, slopeline(slopex2, yint = highint[3], slope = mrsA(ea = 15)), col = "black", lty = 2, lwd = segmentlinewidth)


firstpointsx <- c(10, 10, 10, 10)
firstpointsy <- c(indiffA(ea = firstpointsx[1], uA = 300), indiffA(ea = firstpointsx[2], uA = 225), indiffA(ea = firstpointsx[3], uA = 145), indiffA(ea = firstpointsx[4], uA = 112))
points(firstpointsx, firstpointsy, pch = 16, col = "black", cex = 1.5)

secondpointsx <- c(15, 15, 15, 15)
secondpointsy <- c(indiffA(ea = 15, uA = 301), indiffA(ea = 15, uA = 225), indiffA(ea = 15, uA = 144), indiffA(ea = 15, uA = 111))
points(secondpointsx, secondpointsy, pch = 16, col = "black", cex = 1.5)




dev.off()
