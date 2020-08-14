require(shape)
pdf(file = "coordination_failures/mb_mc_hours_base.pdf", width = 9, height = 12)

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
par(mar =  c(4, 6, 1, 1), mfrow = c(2,1))

indiffA <- function(ea, uA = 46.08) {
  uA + (1/2)*(ea)^2
}

output <- function(ea, eb = 12, alpha = 30, beta = 1/2){
  (alpha - beta*(ea+eb))*ea
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


xlims <- c(0, 31)
ylims <- c(0, 500)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

a <- c(uA(ea = 12, output(ea = 12, eb = 12)), uA(ea = 15, output(ea = 15, eb = 0)), 300)

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
ticksx <- c(seq(xlims[1], xlims[2], 3), xlims[2])
xlabels <- c(seq(xlims[1], xlims[2], 3), NA)
ticksy <- c(0, 144, 225, 300, ylims[2])
ylabels <- c(NA, 144, 225, 300, NA)
# ticksx <- c(0, 6.9, 12, 9.6, 16, 24, 26)
# xlabels <- c(NA, expression(paste(e^A,"*")), expression(paste(1/2*beta)), expression(paste(e^{AN})), expression(paste(alpha)), expression(paste(1/beta)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis= labelsize - 0.05)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis= labelsize - 0.05)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

#Feasible set
xpolyF <- seq(from = 0, to = xlims[2], length.out = 501)
ypolyF <- output(xpolyF, eb = 0)
polygon(c(0, xpolyF, xlims[2], xlims[1]), 
        c(output(0, eb = 0), output(xpolyF, eb = 0), 0, 0),
        border = FALSE, col = COLA[1])

text(24, 150, expression(paste("Feasible set, ", h^B == 0)), xpd = TRUE, cex = labelsize)


lines(xx1, output(xx1, eb = 0), col = COLA[4], lwd = graphlinewidth)
#lines(xx1, output(xx1, eb = 12), col = COLA[4], lwd = graphlinewidth)

contour(x, y,
        outer(x, y, uA),
        drawlabels = FALSE,
        col = COLB[4],
        lwd = graphlinewidth,
        levels = a,
        xaxs="i",
        yaxs="i",
        add = TRUE)

# segments(12, 300, 12, -300, lty = 2, col = grays[20], lwd = segmentlinewidth, xpd = 350)
segments(15, output(ea = 15, eb = 0), 15, -300, lty = 2, col = grays[20], lwd = segmentlinewidth, xpd = 350)
segments(6, output(6, eb = 0), 6, -300, lty = 2, col = grays[20], lwd = segmentlinewidth, xpd = 350)
points(6, output(ea = 6, eb = 0), pch = 16, col = "black", cex = 1.5)
text(6.5, output(ea = 6, eb = 0) - 8, expression(f), cex = annotatesize)



# segments(2, 0, 2, ylims[2], lty = 2, col = grays[20] , lwd = segmentlinewidth)
# segments(6, 0, 6, ylims[2], lty = 2, col = grays[20] , lwd = segmentlinewidth)

#Axis labels
#mtext(expression(paste("A's hours, ", h^A)), side=1, line = 3.2, cex = axislabelsize)
#text(0.5*xlims[2], -40, expression(paste("A's hours, ", h^A)), xpd = TRUE, cex = axislabelsize)
text(-4, 0.5*ylims[2], expression(paste("Consumption, ", y^A," (pounds, lb)")), xpd = TRUE, cex = axislabelsize, srt = 90)

# text(-0.9, 0.5*ylims[2], expression(paste("Output, ", y^A)), xpd = TRUE, cex = axislabelsize, srt = 90)
# text(5, 12, expression(paste("slope", phantom()==h^A, phantom() == 2)), cex = annotatesize)
# Arrows(3.6, 12, 2.4, 12, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
# text(9, 28, expression(paste("slope", phantom()==h^A, phantom() == 6)), cex = annotatesize)
# Arrows(7.6, 28, 6.4, 28, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the iso-welfare functions for the HG, Aisha
text(2, 125, expression(u[1]^A), cex = annotatesize)
text(2, 205, expression(u[2]^A), cex = annotatesize)
text(2, 280, expression(u[3]^A), cex = annotatesize)

# points(12, output(ea = 12), pch = 16, col = "black", cex = 1.5)
# text(12 + 0.5, output(ea = 12) - 6, expression(paste(n)), cex = annotatesize)

points(15, output(ea = 15, eb = 0), pch = 16, col = "black", cex = 1.5)
text(15 + 0.5, output(ea = 15, eb = 0) - 6, expression(paste(s)), cex = annotatesize)


#text(28, 420, expression(paste(x^A*bgroup("(",list(h^A, h^B == 0),")") ) ), cex = annotatesize)
text(27.5, 420, expression(paste("A's production") ), cex = annotatesize)
text(27.5, 390, expression(paste("function") ), cex = annotatesize)
text(27.5, 360, expression(paste(x^A*(list(h^A, h^B == 0)) ) ), cex = annotatesize)
text(27.5, 330, expression(paste("defines feasible") ), cex = annotatesize)
text(27.5, 300, expression(paste("consumption, ", y^A) ), cex = annotatesize, xpd = TRUE)

# text(19.8, 260, expression(paste(y(h^A, h^B >0))), cex = annotatesize)


MBenefit <- function(ea, eb = 12, alpha = 30, beta = 1/2) {
  (alpha - beta*eb - 2*beta*ea)
}

MCost <- function(ea, slope = 1, intercept = 0){
  slope*ea
}


xlims <- c(0, 31)
ylims <- c(0, 31)

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
     yaxs="i")

# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)
# ticksx <- seq(from = 0, to = xlims[2], by = 2)
# xlabels <- seq(from = 0, to = xlims[2], by = 2)
ticksy <- c(0, MCost(ea = 6), MBenefit(15, eb = 0), MBenefit(ea = 6, eb = 0), 30, ylims[2])
ylabels <- c(NA, 6, 15, 24, expression(paste(alpha)), NA)
# ticksx <- c(0, 12, 15, 24, 30, xlims[2])
# xlabels <- c(0, 12, 15, 24, 30, xlims[2])
ticksx <- c(seq(xlims[1], xlims[2], 3), xlims[2])
xlabels <- c(seq(xlims[1], xlims[2], 3), NA)

#xlabels <- c(NA, expression(paste(h[1]^A)), expression(paste(h[2]^A)), expression(paste(frac(alpha - beta*h^b,2*beta) )), expression(paste(frac(alpha,2*beta) )), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels ,las = 1, cex.axis = labelsize - 0.05)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize - 0.05)
#text(x = c(0, 12, 15, 24, 30, xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE, cex = labelsize - 0.05)

#mtext(expression(paste("Aram's hours, ", h^A)), side=1, line = 4, cex = axislabelsize)
text(0.5*xlims[2], -3.8, expression(paste("A's hours, ", h^A)), xpd = TRUE, cex = axislabelsize)
text(-4, 0.5*ylims[2], expression(paste("A's marginal costs & benefits (lb)")), xpd = TRUE, cex = axislabelsize, srt = 90) 

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(4, 8, length.out = npts)

segments(0, 15, xlims[2], 15, lty = 2, col = grays[20], lwd = segmentlinewidth, xpd = 350)
text(24, 16, expression(mb == mc), cex = annotatesize)

#lines(xx1, MBenefit(xx1, eb = 12), col = COLA[4], lwd = graphlinewidth)
lines(xx1, MBenefit(xx1, eb = 0), col = COLA[4], lwd = graphlinewidth)
lines(xx1, MCost(xx1), col = COLB[4], lwd = graphlinewidth)

#For NE hours = 12
# segments(0, 12, 12, 12, lty = 2, col =  grays[20], lwd = segmentlinewidth)
# segments(12, 0, 12, 300, lty = 2, col = grays[20], lwd = segmentlinewidth, xpd = 350)
segments(15, 0, 15, 300, lty = 2, col = grays[20], lwd = segmentlinewidth, xpd = 350)
segments(6, 0, 6, 300, lty = 2, col = grays[20], lwd = segmentlinewidth, xpd = 350)
segments(0, MBenefit(ea = 6, eb = 0), 6, MBenefit(ea = 6, eb = 0), lty = 2, col = grays[20], lwd = segmentlinewidth, xpd = 350)
points(6, MBenefit(ea = 6, eb = 0), pch = 16, col = "black", cex = 1.5)
text(6 + 0.6, MBenefit(ea = 6, eb = 0) + 0.5, expression(g), cex = annotatesize)

segments(0, MCost(ea = 6), 6, MCost(ea = 6), lty = 2, col = grays[20], lwd = segmentlinewidth, xpd = 350)
points(6, MCost(ea = 6), pch = 16, col = "black", cex = 1.5)
text(6 + 0.6, MCost(ea = 6) - 0.5, expression(h), cex = annotatesize)


#segments(0, 12, 300, 12, lty = 2, col =  grays[20], lwd = segmentlinewidth, xpd = TRUE)

#For P-efficient hours = 10
#segments(0, MBenefit(15, eb = 0), 15, MBenefit(15, eb = 0), lty = 2, col = grays[20] , lwd = segmentlinewidth)
#segments(15, 0, 15, MBenefit(15, eb = 0),  lty = 2, col = grays[20] , lwd = segmentlinewidth)


text(24, 11.5, expression(paste("Marginal benefit")), cex = labelsize)
text(24, 10, expression(paste("when, ", h^B== 0 )), cex = labelsize)
#text(8, 26.5, expression(paste("Marginal Benefit")), cex = labelsize)
#text(8, 25, expression(paste("when, ", h^B== 0 )), cex = labelsize)
#text(11.5, 24.3, expression(paste(mb^A== alpha - 2*beta*h^A )), cex = labelsize)
#text(11.5, 22.5, expression(paste("when, ", h^B== 0 )), cex = labelsize)

# text(6, 14.5, expression(paste("Marginal Benefit ")), cex = labelsize)
# text(6, 13, expression(paste("when, ", h^B > 0 )), cex = labelsize)
#text(5.5, 15.5, expression(paste("Marginal Benefit ")), cex = labelsize)
#text(5.5, 14, expression(paste("when, ", h^B > 0 )), cex = labelsize)
#text(24.5, 13.5, expression(paste(mb^A == alpha - beta*h^B - 2*beta*h^A)), cex = labelsize)
#text(24.5, 12, expression(paste("when, ", h^B > 0 )), cex = labelsize)
#Arrows(21.5, 12.5, 17, 8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


text(24, 30.2, expression(paste("Marginal cost")), cex = labelsize)
text(24, 28.5, expression(paste("or disutility")), cex = labelsize)
text(24, 27, expression(paste(mc^A == h^A)), cex = labelsize)


# points(12, 12, pch = 16, col = "black", cex = 1.5)
# text(12 + 0.85, 12.1, expression(paste(n)), cex = labelsize)

points(15, 15, pch = 16, col = "black", cex = 1.5)
text(15 + 0.35, 15 + 1.25, expression(paste(s)), cex = labelsize)

dev.off()
