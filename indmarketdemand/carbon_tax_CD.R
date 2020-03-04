#Graph Designer: Scott Cohn + Simon Halliday
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

require(shape)
require(pBrackets)

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

#Budget constraint

bcA <- function(x, m = 40, px = 0.5, py = 1) {
  m/py - (px/py)*x
}

#Utility functions 

uFn <- function(x, y, alpha = 0.5){
  (x^alpha)*(y^(1-alpha))
}

#Compensated budget constraint parallel to bc1

cbc1 <- function(x, int = 48) {
  int - .5*x
}


npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
#a <- c(13, 16.75, 19.25)
a <- c(25.65, 23.5, 15)

# -----------------------------
# Carbon Tax 1 -- Cobb Douglas
# -----------------------------

pdf(file = "indmarketdemand/carbon_tax_CD_1.pdf", width = 9, height = 7)

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 7, 1, 1))

#Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 65)
xlims <- c(0, 110)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

ulevels <- c(28.25, 40) #alpha = 0.6

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axistitlesize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i"
)

#x and y limits with plain axes without ticks/numbers to match previous graph

ticksx <- c(0, 40, 57.5, 80, xlims[2])
xlabels <- c(NA, expression(paste(x[b])), expression(paste(x[c])) , expression(paste(x[a])), NA)
ticksy <- c(0, bcA(40), bcA(57.5, m = 56.5), bcA(0), bcA(0, m = 56.5), ylims[2])
ylabels <- c(NA, expression(paste(y[b] == y[a])), expression(paste(y[c])), expression(paste(frac(m[b],p[y]) == frac(m[a],p[y]) )), expression(paste(frac(m[c],p[y]) )), NA)

axis(1, at = ticksx,  pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy,  pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

contour(x, y, 
        outer(x, y, uFn),
        drawlabels = FALSE,
        col = COLA[4],
        lwd = graphlinewidth,
        levels = ulevels, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE)

#Axis labels
#mtext(expression(paste("Fossil fuels consumed, ", x)), side = 1, line = 2.5, cex = axistitlesize)

text((40 + 57.5)/2, -3, expression(paste("Income")), xpd = TRUE, cex = annotatesize - 0.15)
text((40 + 57.5)/2, -5.5, expression(paste("effect")), xpd = TRUE, cex = annotatesize - 0.15) 
text((80 + 57.5)/2, -3, expression(paste("Substitution")), xpd = TRUE, cex = annotatesize - 0.15) 
text((80 + 57.5)/2, -5.5, expression(paste("effect")), xpd = TRUE, cex = annotatesize - 0.15) 

text(0.5*xlims[2], -9, expression(paste("Fossil fuels consumed, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-18, 0.5*ylims[2], expression(paste("Consumption of other goods, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

lines(xx1, bcA(xx1), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, px = 0.25), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, m = 56.5), col = COLB[5], lwd = graphlinewidth)


#Label curves

text(16, 61, expression(u[1]), cex = labelsize)
text(29, 61, expression(u[2]), cex = labelsize)
text(105, 12, expression(bc[a]), cex = labelsize)
text(70, 2.5, expression(bc[b]), cex = labelsize)
text(105, 2.5, expression(bc[c]), cex = labelsize)

#Label points e-sub, e, e'

text(80, bcA(80, px = 0.25) + 2, expression(paste(a)), cex = labelsize)
segments(80, 0, 80, bcA(x = 80, px = 0.25), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, bcA(x = 80, px = 0.25), 80, bcA(80, px = 0.25), lty = 2, col = "gray", lwd = segmentlinewidth)
points(80, bcA(80, px = 0.25), pch = 16, col = "black", cex = 1.5)

text(40, bcA(40) + 2, expression(paste(b)), cex = labelsize)
segments(40, 0, 40, bcA(x = 40), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, bcA(x = 40), 56, bcA(x = 40), lty = 2, col = "gray", lwd = segmentlinewidth)
points(40, bcA(40), pch = 16, col = "black", cex = 1.5)

text(57.5, bcA(57.5, m = 56.5) + 2, expression(paste(c)), cex = labelsize)
segments(57.5, 0, 57.5, bcA(57.5, m = 56.5), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, bcA(57.5, m = 56.5), 56, bcA(57.5, m = 56.5), lty = 2, col = "gray", lwd = segmentlinewidth)
points(57.5, bcA(57.5, m = 56.5), pch = 16, col = "black", cex = 1.5)

brackets(56.5, -0.6, 41, -0.6,  h = 1,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = segmentlinewidth, lty = 1, xpd = TRUE)

brackets(79, -0.6, 58.5, -0.6,  h = 1,  ticks = 0.5, curvature = 0.5, type = 1,
         col = 1, lwd = segmentlinewidth, lty = 1, xpd = TRUE)



#Label y-sub,x-sub,etc. on axes

#text(37.5, -.9, expression(paste(x[sub])), xpd = TRUE, cex = labelsize)
#text(40.8, -.9, expression(paste(x[e'])),  xpd = TRUE,  cex = labelsize)
#text(56.9, -.9, expression(paste(x[e])),  xpd = TRUE, cex = labelsize)

#text(-2, cbc1(47), expression(paste(y[sub])), xpd = TRUE, cex = labelsize)
#text(-2, bc1(39) + .5, expression(paste(y[e*minute] == y[e])),  xpd = TRUE,  cex = labelsize)
#text(-2, bc1(39) - 1, expression(paste(y[e])),  xpd = TRUE, cex = labelsize)


dev.off()



# -----------------------------
# Carbon Tax 2 -- Cobb Douglas
# -----------------------------

pdf(file = "indmarketdemand/carbon_tax_CD_2.pdf", width = 9, height = 7)

#Edited the margins to cater for the larger LHS labels
par(mar =  c(5, 6, 1, 1))

#Add limits on axes and levels of utility for each indifference curve
ylims <- c(0, 65)
xlims <- c(0, 110)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

ulevels <- c(28.25, 40) #alpha = 0.6

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axistitlesize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i"
)

#x and y limits with plain axes without ticks/numbers to match previous graph

ticksx <- c(0, 40, 60, xlims[2])
xlabels <- c(NA, expression(paste(x[b])), expression(paste(x[d])), NA)
ticksy <- c(0, bcA(40), bcA(60, m = 62), bcA(0), bcA(0, m = 62), ylims[2])
ylabels <- c(NA, expression(paste(y[b])), expression(paste(y[d])), expression(paste(m[b])), expression(paste(m[d])), NA)

axis(1, at = ticksx,  pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy,  pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

ulevels <- c(28.25, 40, 43.9) #alpha = 0.6

contour(x, y, 
        outer(x, y, uFn),
        drawlabels = FALSE,
        col = COLA[4],
        lwd = graphlinewidth,
        levels = ulevels, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE)

#Axis labels
text(0.5*xlims[2], -9, expression(paste("Fossil fuels consumed, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-12.5, 0.5*ylims[2], expression(paste("Consumption of other goods, ", y)), xpd = TRUE, cex = axislabelsize, srt = 90) 

lines(xx1, bcA(xx1), col = COLB[3], lwd = graphlinewidth)
#lines(xx1, bcA(xx1, px = 0.25), col = COLB[3], lwd = graphlinewidth)
lines(xx1, bcA(xx1, m = 62), col = COLB[5], lwd = graphlinewidth)


#Label curves

text(16, 61, expression(u[1]), cex = labelsize)
text(28.5, 61, expression(u[2]), cex = labelsize)
text(34, 61, expression(u[3]), cex = labelsize)
text(105, 12, expression(bc[d]), cex = labelsize)
text(70, 2.5, expression(bc[b]), cex = labelsize)
#text(105, 2.5, expression(bc[c]), cex = labelsize)

#Label points e-sub, e, e'

text(80 - 2, bcA(80, px = 0.25) - 1.5, expression(paste(a)), cex = labelsize)
# segments(80, 0, 80, bcA(x = 80, px = 0.25), lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, bcA(x = 80, px = 0.25), 80, bcA(80, px = 0.25), lty = 2, col = "gray", lwd = segmentlinewidth)
points(80, bcA(80, px = 0.25), pch = 16, col = "black", cex = 1.5)

text(40, bcA(40) + 2, expression(paste(b)), cex = labelsize)
segments(40, 0, 40, bcA(x = 40), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, bcA(x = 40), 40, bcA(x = 40), lty = 2, col = "gray", lwd = segmentlinewidth)
points(40, bcA(40), pch = 16, col = "black", cex = 1.5)

text(60, bcA(60, m = 62) + 2, expression(paste(d)), cex = labelsize)
segments(60, 0, 60, bcA(60, m = 62), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, bcA(60, m = 62), 60, bcA(60, m = 62), lty = 2, col = "gray", lwd = segmentlinewidth)
points(60, bcA(60, m = 62), pch = 16, col = "black", cex = 1.5)

dev.off()