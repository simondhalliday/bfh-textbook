#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(extrafont)
library(pBrackets)
pdf(file = "employment/employment_rentsv2.pdf", width = 8, height = 6)

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.4
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
grays <- gray.colors(25, start = 1, end = 0)

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 7, 1, 1))

PCFn <- function(delta, mu = 0.5) {
  delta/mu
}

isoreturnFn <- function(delta, pi = 0.125) {
  1 - (pi)/delta
}

xlims <- c(0, 0.7)
ylims <- c(0, 0.9)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i"
)


# ticksy <- seq(from = 0, to = ylims[2], by = 1)
# ylabels <- seq(from = 0, to = ylims[2], by = 1)
#ticksx <- seq(from = 0, to = xlims[2], by = 1)
# xlabels <- seq(from = 0, to = xlims[2], by = 1)
ticksy <- c(ylims[1], 0.2, 0.6, 0.8, ylims[2])
ylabels <- c(NA, NA, NA, expression(paste(w^N)), NA)
xlabels <- c(NA, expression(paste(s[1])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, ticks = FALSE, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

Arrows(0, 0, xlims[2], 0, col = "black", lty = 1, col = COLB[4] , arr.xpd = TRUE, arr.type = "triangle", arr.lwd = segmentlinewidth)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(1.5, 2.5, length.out = npts)
xx4 <- seq(3, 5, length.out = npts)
xx5 <- seq(7, 9, length.out = npts)

#Draw the polygon for feasibility
xpoly1 <- c(0, 0.5, 0.5, 0, 0)
ypoly1 <- c(0, 0, 0.2, 0.2, 0)
polygon(x = xpoly1, y = ypoly1, col=COLA[1], density=NULL, border = NA)

xpoly2 <- c(0, 0.5, 0.5, 0, 0)
ypoly2 <- c(0.2, 0.2, 0.6, 0.6, 0.3)
polygon(x = xpoly2, y = ypoly2, col="gray", density=NULL, border = NA)

xpoly3 <- c(0, 0.5, 0.5, 0, 0)
ypoly3 <- c(0.6, 0.6, 0.8, 0.8, 0.4)
polygon(x = xpoly3, y = ypoly3, col=COLB[1], density=NULL, border = NA)

#Label the feasible frontier
text(0.25, 0.4, expression("Employment rent"), cex = labelsize)
#text(0.25, 0.1, expression("Unemployment Benefit, B"), cex = labelsize)
#text(0.25, 0.6, expression("Rent when employed"), cex = labelsize)

#Braces for labels
brackets(x1 = 0.51, y1 = 0.59, x2 = 0.51, y2 = 0.21,  ticks = 0.5, curvature = 0.5, type = 1, 
         col = "black", lwd = 2, lty = 1, xpd = FALSE)

text(0.63, 0.45, expression("Per period"), cex = labelsize,xpd = TRUE)
text(0.63, 0.41, expression("employment"), cex = labelsize,xpd = TRUE)
text(0.63, 0.38, expression("rent"), cex = labelsize,xpd = TRUE)
#text(0.62, 0.22, expression("of time"), cex = labelsize,xpd = TRUE)
#brackets(x1 = 0.51, y1 = 0.19, x2 = 0.51, y2 = 0.01,  ticks = 0.5, curvature = 0.5, type = 1, 
    #     col = "black", lwd = 2, lty = 1, xpd = FALSE)

#text(0.6, 0.125, expression("Benefits"), cex = labelsize,xpd = TRUE)
#text(0.6, 0.075, expression("per week"), cex = labelsize,xpd = TRUE)
#text(0.62, 0.04, expression("of time"), cex = labelsize,xpd = TRUE)

#Axis labels
text(0.6, -0.1 , expression(paste("Time in weeks")), xpd = TRUE, cex = axislabelsize) 
text(-0.08, 0.85*(ylims[2]), expression(paste("Wage, ", w)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Spell of unemployment
brackets(x1 = 0.49, y1 = -0.02, x2 = 0.01, y2 = -0.02,  
         ticks = 0.5, curvature = 0.5, type = 1, 
         col = "black", lwd = 2, lty = 1, xpd = TRUE)

text(0.25, -0.1, expression(paste("Spell of unemployment, ", s[1])), cex = labelsize, xpd = TRUE)

Arrows(0, 0.2, 0.5, 0.2, lty = 1, col = COLA[4] , arr.type = "0", arr.lwd = segmentlinewidth)

#Arrows for the rents
#Arrows(0, 0.8, 0.49, 0.8, lty = 1, col = COLB[4] , arr.type = "triangle", arr.lwd = segmentlinewidth)
#Arrows(0.26, 0.8, 0.49, 0.8, lty = 1, col = COLB[4] , arr.type = "triangle", arr.lwd = segmentlinewidth)
Arrows(0, 0.8, 0.5, 0.8, lty = 1, col = COLB[4] , arr.type = "triangle", arr.lwd = segmentlinewidth)

Arrows(0, 0.6, 0.06, 0.6, lty = 1, col = COLB[4] , arr.type = "triangle", arr.lwd = segmentlinewidth)
Arrows(0.1, 0.6, 0.1, 0.4, lty = 1, col = COLB[4] , arr.type = "triangle", arr.lwd = segmentlinewidth)
Arrows(0.1, 0.6, 0.1, 0.2, lty = 1, col = COLB[4] , arr.type = "0", arr.lwd = segmentlinewidth)
Arrows(0.3, 0.6, 0.45, 0.6, lty = 1, col = COLB[4] , arr.type = "triangle", arr.lwd = segmentlinewidth)
Arrows(0.4, 0.23, 0.4, 0.41, lty = 1, col = COLB[4] , arr.type = "triangle", arr.lwd = segmentlinewidth)
Arrows(0.4, 0.6, 0.4, 0.2, lty = 1, col = COLB[4] , arr.type = "0", arr.lwd = segmentlinewidth)

Arrows(0, 0.6, 0.5, 0.6, lty = 1, col = COLB[4] , arr.type = "0", arr.lwd = segmentlinewidth)

Arrows(0.1, 0.6, 0.2, 0.6, lty = 1, col = COLB[4] , arr.type = "triangle", arr.lwd = segmentlinewidth)
Arrows(0.2, 0.6, 0.35, 0.6, lty = 1, col = COLB[4] , arr.type = "triangle", arr.lwd = segmentlinewidth)

Arrows(0.1, 0.2, 0.2, 0.2, lty = 1, col = COLB[4] , arr.type = "triangle", arr.lwd = segmentlinewidth)
Arrows(0.2, 0.2, 0.385, 0.2, lty = 1, col = COLB[4] , arr.type = "triangle", arr.lwd = segmentlinewidth)


text(-0.07, 0.6, expression("per period"), cex = labelsize,xpd = TRUE)
text(-0.07, 0.55, expression("value of"), cex = labelsize,xpd = TRUE)
text(-0.07, 0.5, expression("the job"), cex = labelsize,xpd = TRUE)


text(-0.07, 0.2, expression("per period"), cex = labelsize,xpd = TRUE)
text(-0.07, 0.15, expression("reservation"), cex = labelsize,xpd = TRUE)
text(-0.07, 0.1, expression("wage"), cex = labelsize,xpd = TRUE)

#Arrows(0, 0.2, 0.25, 0.2, lty = 1, col = COLA[4] , arr.type = "triangle", arr.lwd = segmentlinewidth)
#Arrows(0.5, 0.2, 0.5, 0.49, lty = 1, col = COLA[4] , arr.type = "triangle", arr.lwd = segmentlinewidth)
#Arrows(0.5, 0.5, 0.5, 0.77, lty = 1, col = COLA[4] , arr.type = "triangle", arr.lwd = segmentlinewidth)
#Arrows(0.5, 0.78, 0.65, 0.78, lty = 1, col = COLA[4] , arr.type = "triangle", arr.lwd = segmentlinewidth)


#brackets(x1 = 0.68, y1 = 0.79, x2 = 0.68, y2 = 0.21,  ticks = 0.5, curvature = 0.5, type = 1, 
#         col = "black", lwd = 2, lty = 1, xpd = FALSE)

#text(0.79, 0.53, expression(paste(hat(c), ", cost of")), cex = labelsize,xpd = TRUE)
#text(0.79, 0.47, expression(paste("job loss")), cex = labelsize, xpd = TRUE)


#brackets(x1 = -0.037, y1 = 0.01, x2 = -0.037, y2 = 0.39,  ticks = 0.5, curvature = 0.5, type = 1, 
       #  col = "black", lwd = 2, lty = 1, xpd = TRUE)
#text(-0.11, 0.2, expression(paste("Opportunity cost")), xpd = TRUE, cex = labelsize, srt = 90) 
#text(-0.09, 0.205, expression(paste("of work")), xpd = TRUE, cex = labelsize, srt = 90) 


# brackets(x1 = 0.63, y1 = 0.39, x2 = 0.63, y2 = 0.01,  ticks = 0.5, curvature = 0.5, type = 1, 
#          col = "black", lwd = 2, lty = 1, xpd = FALSE)
# 
# text(0.72, 0.24, expression(paste("opportunity")), cex = labelsize)
# text(0.72, 0.2, expression(paste("cost of")), cex = labelsize)
# text(0.72, 0.15, expression(paste("working")), cex = labelsize)


dev.off()
