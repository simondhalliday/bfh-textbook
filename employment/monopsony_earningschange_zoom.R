#' Graph Designer: Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(plotrix)
pdf(file = "employment/monopsony_earningschange_zoom.pdf", width = 9, height = 7)

#Set parameters for graphics
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


WageFn <- function(h, ubar = 3, B = 2, t = 0.8) {
  B + ubar + (ubar - ubar*t +  ubar*t*h )/(t*(1 - h))
}

Mch <- function(h, ubar = 3, B = 2, t = 0.8) {
  B + ubar + (ubar - ubar*t +  ubar*t*h )/(t*(1 - h))+ (ubar*h)/(t*(1-h)^2)
}


MRP <- function(h, constant = 7, slope = 1){
  constant/(slope*h)
}

# mrp2 <- function(h, constant = 4, power = 2){
#   constant/(h^power)
# }

costRevSol <- function(h, constant = 7, ubar = 3, B = 2, t = 0.8){
  B + ubar + (ubar - ubar*t +  ubar*t*h )/(t*(1 - h)) + (ubar*h)/(t*(1-h)^2) - constant/h
}

#uniroot(costRevSol(x, constant = 7, ubar = 3, B = 2, t = 0.8), lower = 0.01, upper = 1)

par(mar =  c(5, 6, 1, 1))

xlims <- c(0, 0.63525)
ylims <- c(0, 24.2)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = "",
     ylab = "",
     xaxt = "n", 
     yaxt = "n",
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i")

npts <- 500 
npts2 <- 501

#Guesses at intersections
LPoints <- c(0.465, 0.56) 
BPoints <- c(0.37, 0.53, 0.78)

#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], BPoints[2], length.out = npts)
xx2 <- seq(BPoints[2], 1, length.out = npts)
xx3 <- seq(xlims[1], BPoints[3], length.out = npts)
xx4 <- seq(BPoints[3], 1, length.out = npts)
xx5 <- seq(xlims[1], 1, length.out = npts)






#Customize ticks and labels for the plot
ticksy <- c(0, MRP(BPoints[1]), WageFn(LPoints[1]), ylims[2])
ylabels <- c(0, NA, expression(paste(w^M)), NA)
ticksx <- c(0, BPoints[1], LPoints[1], BPoints[3], 1, xlims[2])
xlabels <- c(0, expression(paste(h[d])), expression(paste(h^M)), expression(paste(h[g])),  1.0, NA)

xpoly1 <- c(0, 0, BPoints[1], BPoints[1])
ypoly1 <- c(0, WageFn(LPoints[1]), WageFn(LPoints[1]), 0)
polygon(x = xpoly1, y = ypoly1, col=COLA[1], density=NULL, border = NA)

xpoly2 <- c(LPoints[1], LPoints[1], BPoints[1], BPoints[1])
ypoly2 <- c(0, WageFn(LPoints[1]), WageFn(LPoints[1]), 0)
polygon(x = xpoly2, y = ypoly2, col=COLB[1], density=NULL, border = NA)

xpoly3 <- c(0, 0, BPoints[1], BPoints[1])
ypoly3 <- c(19, WageFn(LPoints[1]), WageFn(LPoints[1]), 19)
polygon(x = xpoly3, y = ypoly3, col=COLA[2], density=NULL, border = NA)

#Draw the lines for the graphs
lines(xx3, WageFn(xx3), col = COLA[6], lwd = graphlinewidth, lty = 2)
lines(xx4, WageFn(xx4), col = COLA[6], lwd = graphlinewidth, lty = 1)
lines(xx1, Mch(xx1), col = COLA[3], lwd = graphlinewidth, lty = 2)
lines(xx2, Mch(xx2), col = COLA[3], lwd = graphlinewidth, lty = 2)
lines(xx5, MRP(xx5), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, mrp2(xx1), col = COLB[4], lwd = graphlinewidth)

#lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 4)
#lines(xx2, solowInfeas(xx2, delta = 5), col = COL[1], lwd = 4, lty = 2)


axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1,cex.axis = labelsize)


text(-0.035, 19 + 0.75,  expression(paste(w[d]^min)), xpd = TRUE, cex = labelsize) 



#Axis labels 
text(0.5*xlims[2], -3.4,  expression(paste("Hours of employment as a fraction of local labor supply, ", h)), xpd = TRUE, cex = axislabelsize) 
text(-0.08, 0.5*ylims[2], expression(paste("Wages, costs, and revenues ($)")), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Annotation of the  graphs
#text(0.86, 43, expression(paste("Average cost")), cex = labelsize, xpd = TRUE)
text(0.625, 13, expression(paste(ach)), cex = labelsize, xpd = TRUE)

#text(0.62, 43, expression(paste("Marginal cost")), cex = labelsize, xpd = TRUE)
text(0.56, 24, expression(paste(mch)), cex = labelsize, xpd = TRUE)

#text(0.2, 43, expression(paste("Marginal revenue product")), cex = labelsize, xpd = TRUE)
text(0.26, 24, expression(paste(mrph)), cex = labelsize, xpd = TRUE)



#Labor supply
segments(1, 0, 1, 42, lty = 2, lwd = segmentlinewidth, col = grays[20])
text(1.04, 43, expression(paste("Labor")), cex = labelsize, xpd = TRUE)
text(1.04, 41, expression(paste("supply")), cex = labelsize, xpd = TRUE)



#segments(0, Mch(LPoints[1]), LPoints[1], Mch(LPoints[1]), lty = 2, lwd = segmentlinewidth, col = grays[20])
segments(LPoints[1], 0 , LPoints[1], Mch(LPoints[1]), lty = 2, lwd = segmentlinewidth, col = grays[20])
segments(0, WageFn(LPoints[1]), LPoints[1], WageFn(LPoints[1]), lty = 2, lwd = segmentlinewidth, col = grays[20])

segments(BPoints[1], 0, BPoints[1], MRP(BPoints[1]), lty = 2, lwd = segmentlinewidth, col = grays[20])

#Binding minimum wage
segments(0, 19, BPoints[3], 19, lty = 1, lwd = graphlinewidth + 0.2, col = COL[2])
segments(BPoints[3], 19, 1, 19, lty = 2, lwd = graphlinewidth + 0.2, col = COL[2])

#Green part of min wage
segments(0, 19 - 0.3, BPoints[3], WageFn(BPoints[3]) - 0.3, lty = 1, lwd = graphlinewidth, col = COLA[4])

points(BPoints[1], MRP(BPoints[1]), pch = 16, col = "black", cex = 1.5)
text(BPoints[1], MRP(BPoints[1]) + 1.5, expression(paste(d)), cex = labelsize)


segments(BPoints[3], 0, BPoints[3], WageFn(BPoints[3]), lty = 2, lwd = segmentlinewidth, col = grays[20])
points(BPoints[3], WageFn(BPoints[3]), pch = 16, col = "black", cex = 1.5)
text(BPoints[3] + 0.02, WageFn(BPoints[3]) - 1.2, expression(paste(g)), cex = labelsize)


points(LPoints[1], Mch(LPoints[1]), pch = 16, col = "black", cex = 1.5)
text(LPoints[1] - 0.03, MRP(LPoints[1]), expression(paste(a)), cex = labelsize)


points(LPoints[1], WageFn(LPoints[1]), pch = 16, col = "black", cex = 1.5)
text(LPoints[1] + 0.02, WageFn(LPoints[1]) - 1, expression(paste(m)), cex = labelsize)

#Arrows for effects of min wage
Arrows(LPoints[1] - 0.02, -1, BPoints[1] + 0.025, -1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", xpd = TRUE)
Arrows(-0.04, WageFn(LPoints[1]) + 1.5, - 0.04, MRP(BPoints[1]) - 1.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", xpd = TRUE)


text(0.15, MRP(BPoints[1]) + 3, expression(paste("Minimum")), cex = labelsize)
text(0.15, MRP(BPoints[1]) + 1.6, expression(paste("wage, ", w[d]^min)), cex = labelsize)

text(0.1, 13.5, expression(paste("Increase")), cex = labelsize)
text(0.1, 12.2, expression(paste("in earnings")), cex = labelsize)

text(0.1, 3.65, expression(paste("Earnings shared")), cex = labelsize)
text(0.1, 2.35, expression(paste("by both cases")), cex = labelsize)

# text(0.475, 4, expression(paste("Decrease")), cex = labelsize)
# text(0.475, 2, expression(paste("in earnings")), cex = labelsize)

text(0.575, 3.65, expression(paste("Decrease")), cex = labelsize)
text(0.575, 2.35, expression(paste("in earnings")), cex = labelsize)
Arrows(LPoints[1] + 0.04, 3, BPoints[1] + 0.05, 3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", xpd = TRUE)

dev.off()