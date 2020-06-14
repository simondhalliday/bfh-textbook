require(shape)
require(plotrix)
pdf(file = "employment/monopsony_minwage_increase.pdf", width = 9, height = 7)

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
grays <- gray.colors(25, start = 1, end = 0)


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

par(mar =  c(5, 6, 4, 2))

xlims <- c(0, 1.05)
ylims <- c(0, 40)

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
IPoints <- c(0.415, 0.54, 0.66)
BPoints <- c(0.37, 0.53, 0.78)

#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], IPoints[3], length.out = npts)
xx2 <- seq(IPoints[3], 1, length.out = npts)
xx3 <- seq(xlims[1], IPoints[3], length.out = npts)
xx4 <- seq(IPoints[3], 1, length.out = npts)
xx5 <- seq(xlims[1], 1, length.out = npts)


#Draw the lines for the graphs
lines(xx3, WageFn(xx3), col = COLA[6], lwd = graphlinewidth, lty = 2)
lines(xx4, WageFn(xx4), col = COLA[6], lwd = graphlinewidth, lty = 1)
lines(xx1, Mch(xx1), col = COLA[3], lwd = graphlinewidth, lty = 2)
lines(xx2, Mch(xx2), col = COLA[3], lwd = graphlinewidth, lty = 1)
lines(xx5, MRP(xx5), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, mrp2(xx1), col = COLB[4], lwd = graphlinewidth)

#lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 4)
#lines(xx2, solowInfeas(xx2, delta = 5), col = COL[1], lwd = 4, lty = 2)




#Customize ticks and labels for the plot
ticksy <- c(0, 13, WageFn(LPoints[1]), 40)
ylabels <- c(0, NA, expression(paste(w^M)), NA)
ticksx <- c(0,  LPoints[1], IPoints[2],  IPoints[3], 1, xlims[2])
xlabels <- c(0,  expression(paste(h^M)), expression(paste(h[i])), expression(paste(h[j])),  1.0, NA)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1,cex.axis = labelsize)

text(-0.035, 13 + 0.75,  expression(paste(w[i]^min)), xpd = TRUE, cex = labelsize) 


#Axis labels 
text(0.5*xlims[2], -4.5,  expression(paste("Hours of employment as a fraction of local labor supply, ", h)), xpd = TRUE, cex = axislabelsize) 
text(-0.13, 0.5*ylims[2], expression(paste("Wages, costs, and revenues ($)")), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Annotation of the  graphs
text(0.86, 43, expression(paste("Average cost")), cex = labelsize, xpd = TRUE)
text(0.86, 41, expression(paste(ach)), cex = labelsize, xpd = TRUE)

text(0.62, 43, expression(paste("Marginal cost")), cex = labelsize, xpd = TRUE)
text(0.62, 41, expression(paste(mch)), cex = labelsize, xpd = TRUE)

text(0.2, 43, expression(paste("Marginal revenue product")), cex = labelsize, xpd = TRUE)
text(0.2, 41, expression(paste(mrph)), cex = labelsize, xpd = TRUE)



#Labor supply
segments(1, 0, 1, 42, lty = 2, lwd = segmentlinewidth, col = grays[20])
text(1.04, 43, expression(paste("Labor")), cex = labelsize, xpd = TRUE)
text(1.04, 41, expression(paste("supply")), cex = labelsize, xpd = TRUE)

#Line for the absolute maximum quality
#segments(1, 0, 1, 42, lty = 2, lwd = 3, col = "darkgray")
#segments(0.75, 0, 0.75, 20, lty = 2, lwd = 2, col = "darkgray")

#Arrows(0.8, 15, 0.8, 19, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#Arrows(0.8, 15, 0.8, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#text(0.92, 12.5, expression(paste("Employment Rent")))

#Text to indicate delta = 5
#text(0.2, 38, expression(paste("Wage Function")))
#text(0.2, 36, expression(paste("set to ", delta, " = 5")))


#Fixed wage comparison
#segments(0, 12.5, 1, 12.5, lty = 2, lwd = graphlinewidth, col = COLB[3])


#Guesses at intersections
LPoints <- c(0.465, 0.56) 
IPoints <- c(0.415, 0.54, 0.66)
# points(BPoints[2], Mch(BPoints[2]), pch = 16, col = "black", cex = 1.5)
# text(BPoints[2] + 0.02, Mch(BPoints[2]) - 1, expression(paste(m)), cex = labelsize)

#Binding minimum wage
segments(0, 13, IPoints[3], 13, lty = 1, lwd = graphlinewidth, col = COL[2])
segments(IPoints[3], 13, 1, 13, lty = 2, lwd = graphlinewidth, col = COL[2])

#Green part of min wage
segments(0, 13 - 0.3, IPoints[3], WageFn(IPoints[3]) - 0.3, lty = 1, lwd = graphlinewidth, col = COLA[4])

#Segment of discontinuous mch
segments(IPoints[3], 13, IPoints[3], Mch(IPoints[3]), lty = 2, lwd = graphlinewidth, col = COLA[3])


segments(IPoints[2], 0, IPoints[2], MRP(IPoints[2]), lty = 2, lwd = segmentlinewidth, col = grays[20])
points(IPoints[2], MRP(IPoints[2]), pch = 16, col = "black", cex = 1.5)
text(IPoints[2] + 0.01, MRP(IPoints[2]) + 1, expression(paste(i)), cex = labelsize)

points(IPoints[3], Mch(IPoints[3]), pch = 16, col = "black", cex = 1.5)
text(IPoints[3] + 0.02, Mch(IPoints[3]), expression(paste(f)), cex = labelsize)

# points(IPoints[1], Mch(IPoints[1]), pch = 16, col = "black", cex = 1.5)
# text(IPoints[1] + 0.01, Mch(IPoints[1]) - 1.2, expression(paste(g)), cex = labelsize)

points(IPoints[3], WageFn(IPoints[3]), pch = 16, col = "black", cex = 1.5)
text(IPoints[3] + 0.01, WageFn(IPoints[3]) - 1.2, expression(paste(j)), cex = labelsize)

#Segment to original monopsony outcome
#segments(0, Mch(LPoints[1]), LPoints[1], Mch(LPoints[1]), lty = 2, lwd = segmentlinewidth, col = grays[20])
segments(LPoints[1], 0 , LPoints[1], Mch(LPoints[1]), lty = 2, lwd = segmentlinewidth, col = grays[20])
segments(0, WageFn(LPoints[1]), LPoints[1], WageFn(LPoints[1]), lty = 2, lwd = segmentlinewidth, col = grays[20])

#Point where min wage intersects mc(h)
# points(IPoints[1], Mch(IPoints[1]), pch = 16, col = "black", cex = 1.5)
# text(IPoints[1] - 0.03, MRP(IPoints[1]), expression(paste(a)), cex = labelsize)


#Monopsony points
# points(LPoints[1], Mch(LPoints[1]), pch = 16, col = "black", cex = 1.5)
# text(LPoints[1] - 0.03, MRP(LPoints[1]), expression(paste(a)), cex = labelsize)
# points(LPoints[1], WageFn(LPoints[1]), pch = 16, col = "black", cex = 1.5)
# text(LPoints[1] + 0.02, WageFn(LPoints[1]) - 1, expression(paste(m)), cex = labelsize)



#Arrow to mrp = mcl
Arrows(IPoints[2], MRP(IPoints[2]) + 13, IPoints[2], MRP(LPoints[2]) + 2, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(LPoints[1] - 0.02, Mch(LPoints[1]) + 15, expression(paste("Profit maximum at")), cex = labelsize)
text(LPoints[1]- 0.02, Mch(LPoints[1]) + 12, expression(paste(mrph == mch,phantom() == w[i]^min)), cex = labelsize)

#Arrows for effects of min wage
Arrows(LPoints[1] + 0.01, -2.1, IPoints[2] - 0.03, -2.1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", xpd = TRUE)

Arrows(-0.06, WageFn(LPoints[1]) + 0.5, - 0.06, MRP(IPoints[2]) - 1.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", xpd = TRUE)


text(0.15, MRP(IPoints[2]) + 3.5, expression(paste("Minimum")), cex = labelsize)
text(0.15, MRP(IPoints[2]) + 1.5, expression(paste("wage, ", w[i]^min)), cex = labelsize)

# segments(LPoints[1], 0 , LPoints[1], Mch(LPoints[1]), lty = 2, lwd = segmentlinewidth, col = grays[20])
# segments(0, WageFn(LPoints[1]), LPoints[1], WageFn(LPoints[1]), lty = 2, lwd = segmentlinewidth, col = grays[20])



points(LPoints[1], Mch(LPoints[1]), pch = 16, col = "black", cex = 1.5)
text(LPoints[1] - 0.03, MRP(LPoints[1]), expression(paste(a)), cex = labelsize)


points(LPoints[1], WageFn(LPoints[1]), pch = 16, col = "black", cex = 1.5)
text(LPoints[1] + 0.02, WageFn(LPoints[1]) - 1, expression(paste(m)), cex = labelsize)


dev.off()

