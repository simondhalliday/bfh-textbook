#' Graph Designer: Simon Halliday
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
library(plotrix)
library(pBrackets)
pdf(file = "capitalism/monopsony_monopoly_unions.pdf", width = 9, height = 7)

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
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442","#D55E00")

grays <- gray.colors(25, start = 1, end = 0, alpha = 1)


WageFn <- function(h, ubar = 2.5, B = 2, t = 0.7) {
  B + ubar + (ubar - ubar*t +  ubar*t*h )/(t*(1 - h))
}

Mch <- function(h, ubar = 2.5, B = 2, t = 0.7) {
  B + ubar + (ubar - ubar*t +  ubar*t*h )/(t*(1 - h))+ (ubar*h)/(t*(1-h)^2)
}

AvgRevenue <- function(h, rmax = 40, xmax = 1.8){
  rmax - (rmax/xmax)*h
}

MRevenue <- function(h, rmax = 40, xmax = 1.8){
  rmax - 2*(rmax/xmax)*h
}


# MRP <- function(h, constant = 7, slope = 1){
#   constant/(slope*h)
# }

# mrp2 <- function(h, constant = 4, power = 2){
#   constant/(h^power)
# }

costRevSol <- function(h, constant = 7, ubar = 3, B = 2, t = 0.8){
  B + ubar + (ubar - ubar*t +  ubar*t*h )/(t*(1 - h)) + (ubar*h)/(t*(1-h)^2) - constant/h
}

#uniroot(costRevSol(x, constant = 7, ubar = 3, B = 2, t = 0.8), lower = 0.01, upper = 1)

par(mar =  c(5, 6.5, 2, 2))

xlims <- c(0, 1)
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

#Intersections
x <- seq(xlims[1], xlims[2], length.out = npts)
#Wage levels specified for ACL w/o monopsony and apl w/o monopoly

eq1 <- uniroot(function(x)  MRevenue(x) - Mch(x)  , c(.01,1), tol=1e-8)   
wagelvls <- c(WageFn(as.numeric(eq1[1])), MRevenue(as.numeric(eq1[1])), AvgRevenue(as.numeric(eq1[1])), 4.5)
eq2 <- uniroot(function(x)  wagelvls[3] - Mch(x)  , c(.01,1), tol=1e-8)   
eq3 <- uniroot(function(x)  MRevenue(x) - wagelvls[1] , c(.01,1), tol=1e-8)   
eq4 <- uniroot(function(x)  MRevenue(x) - WageFn(x) , c(.01,1), tol=1e-8)   

#MRevenue(as.numeric(eq1[1]))
#AvgRevenue(as.numeric(eq1[1]))

#Create a vector of the solved points
wagelvls <- c(WageFn(as.numeric(eq1[1])), MRevenue(as.numeric(eq1[1])), AvgRevenue(as.numeric(eq1[1])), 4.5, WageFn(LPoints[4]), WageFn(LPoints[4]) + 2)
eq5 <- uniroot(function(x)  MRevenue(x) - wagelvls[6], c(.01,1), tol=1e-8)   

LPoints <- as.numeric(c(eq5[1], eq2[1], eq3[1], eq4[1], eq1[1]))
wagelvls <- c(WageFn(as.numeric(eq5[1])), MRevenue(as.numeric(eq1[1])), AvgRevenue(LPoints[1]), 4.5, WageFn(LPoints[4]), WageFn(LPoints[4]) + 2)




#I don't know what these are any more...
BPoints <- c(0.37, 0.53, 0.78)

#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], BPoints[2], length.out = npts)
xx2 <- seq(BPoints[2], 1, length.out = npts)
xx3 <- seq(xlims[1], BPoints[3], length.out = npts)
xx4 <- seq(BPoints[3], 1, length.out = npts)
xx5 <- seq(xlims[1], 1, length.out = npts)

#Customize ticks and labels for the plot
ticksy <- c(0, wagelvls[4], wagelvls[1], WageFn(LPoints[4]) + 2, wagelvls[3], 40)
ylabels <- c(0, expression(paste(underline(u) + B)), expression(paste(w[m]*(h[u]))), expression(paste(w[u])), expression(paste(arp[u])), NA)
ticksx <- c(0, LPoints[5], LPoints[1], 1, xlims[2])
xlabels <- c(0, expression(paste(h[m])), expression(paste(h[u])), 1.0, NA)

# Owners' rents
xpoly1 <- c(0, 0, LPoints[1], LPoints[1], 0)
ypoly1 <- c(wagelvls[3] - 2, wagelvls[5] + 2 , wagelvls[5] + 2, wagelvls[3] - 2, wagelvls[3] - 2)
polygon(x = xpoly1, y = ypoly1, col=COLA[1], density=NULL, border = NA)

xpoly1 <- c(0, 0, LPoints[1], LPoints[1], 0)
ypoly1 <- c(wagelvls[3] - 2, wagelvls[3] , wagelvls[3], wagelvls[3] - 2, wagelvls[3] - 2)
polygon(x = xpoly1, y = ypoly1, col=COLB[2], density=NULL, border = NA)


# Employment rents
xpoly2 <- c(0, 0, LPoints[1], LPoints[1], 0)
ypoly2 <- c(wagelvls[4], wagelvls[1], wagelvls[1], wagelvls[4], wagelvls[4])
polygon(x = xpoly2, y = ypoly2, col=COL[4], density=NULL, border = NA)
# Fallback with u + B
xpoly3 <- c(0, 0, LPoints[1], LPoints[1], 0)
ypoly3 <- c(wagelvls[4], 0, 0, wagelvls[4], wagelvls[4])
polygon(x = xpoly3, y = ypoly3, col=COLB[1], density=NULL, border = NA)
# Union rents
xpoly4 <- c(0, 0, LPoints[1], LPoints[1], 0)
ypoly4 <- c(wagelvls[5] + 2, wagelvls[1], wagelvls[1], wagelvls[5] + 2, wagelvls[5] + 2)
polygon(x = xpoly4, y = ypoly4, col= rgb(213/255, 94/255, 0, 0.5), density=NULL, border = NA)

#Draw the lines for the graphs
lines(xx3, WageFn(xx3), col = COLA[6], lwd = graphlinewidth, lty = 1)
lines(xx4, WageFn(xx4), col = COLA[6], lwd = graphlinewidth, lty = 1)
lines(xx1, Mch(xx1), col = COLA[3], lwd = graphlinewidth, lty = 1)
lines(xx2, Mch(xx2), col = COLA[3], lwd = graphlinewidth, lty = 1)
lines(xx5, MRevenue(xx5), col = COLB[3], lwd = graphlinewidth)
lines(xx5, AvgRevenue(xx5), col = COLB[4], lwd = graphlinewidth)
#lines(xx1, mrp2(xx1), col = COLB[4], lwd = graphlinewidth)

#lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 4)
#lines(xx2, solowInfeas(xx2, delta = 5), col = COL[1], lwd = 4, lty = 2)


axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1,cex.axis = labelsize)


text(0.3, wagelvls[2] + 5,  expression(paste("Owners'")), xpd = TRUE, cex = labelsize) 
text(0.3, wagelvls[2] + 3,  expression(paste("rents")), xpd = TRUE, cex = labelsize)

text(0.3, wagelvls[1] + 2.25,  expression(paste("Union")), xpd = TRUE, cex = labelsize) 
text(0.3, wagelvls[1] + 0.75,  expression(paste("rents")), xpd = TRUE, cex = labelsize)


# text(0.2, wagelvls[2],  expression(paste(mrp(h) == mc(h))), xpd = TRUE, cex = labelsize) 
# Arrows(0.32, wagelvls[2], LPoints[1] - 0.025, wagelvls[2], col = "black", lty = 1, lwd = 2, arr.type = "triangle", xpd = TRUE)

text(0.3, wagelvls[4] - 1,  expression(paste("Workers'")), xpd = TRUE, cex = labelsize)
text(0.3, wagelvls[4] - 3,  expression(paste("fallbacks")), xpd = TRUE, cex = labelsize)





#text(0.85, wagelvls[1] - 1,  expression(paste(ac)), xpd = TRUE, cex = labelsize)
text(0.9, wagelvls[6] + 3,  expression(paste("Union")), xpd = TRUE, cex = labelsize)
text(0.9, wagelvls[6] + 1,  expression(paste("wage, ", w[u])), xpd = TRUE, cex = labelsize)

text(0.85, wagelvls[1] - 2.25,  expression(paste("Employment")), xpd = TRUE, cex = labelsize)
text(0.85, wagelvls[1] - 4.25,  expression(paste("rents")), xpd = TRUE, cex = labelsize)
Arrows(0.75, wagelvls[1] - 2.25, LPoints[1] - 0.08, wagelvls[1] - 2.25, col = "black", lty = 1, lwd = 2, arr.type = "triangle", xpd = TRUE)

text(0.3, wagelvls[3] + 12.5,  expression(paste("Oportunity cost")), xpd = TRUE, cex = labelsize)
text(0.3, wagelvls[3] + 10.5,  expression(paste("of capital, ", rho%.%w[u]%.%h[u])), xpd = TRUE, cex = labelsize)
Arrows(0.3, wagelvls[3] + 9.5, 0.3, wagelvls[3] - 0.25, col = "black", lty = 1, lwd = 2, arr.type = "triangle", xpd = TRUE)


# text(0.15, wagelvls[3] - 1,  expression(paste(arp)), xpd = TRUE, cex = labelsize)
# text(0.15, wagelvls[3] - 3,  expression(paste("(no monopoly)")), xpd = TRUE, cex = labelsize)


#Axis labels 
text(0.5*xlims[2], -4.7,  expression(paste("Hours of employment as a fraction of local labor supply, ", h)), xpd = TRUE, cex = axislabelsize) 
text(-0.15, 0.5*ylims[2], expression(paste("Wages, costs, and revenues ($)")), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Annotation of the  graphs
#text(0.86, 43, expression(paste("Average cost")), cex = labelsize, xpd = TRUE)
text(0.9, 41, expression(paste(ac(h) )), cex = labelsize, xpd = TRUE)

#text(0.62, 43, expression(paste("Marginal cost")), cex = labelsize, xpd = TRUE)
text(0.69, 41, expression(paste(mc(h) )), cex = labelsize, xpd = TRUE)

#text(0.1, 30, expression(paste("Marginal revenue")), cex = labelsize, xpd = TRUE)
#text(0.1, 28, expression(paste("product")), cex = labelsize, xpd = TRUE)
#Marginal revenue product
text(0.1, 31, expression(paste(mrp(h) )), cex = labelsize, xpd = TRUE)

#Average revenue product
text(0.1, 39.5, expression(paste(arp(h))), cex = labelsize, xpd = TRUE)



#Labor supply
# segments(1, 0, 1, 42, lty = 2, lwd = segmentlinewidth, col = grays[20])
# text(1.04, 43, expression(paste("Labor")), cex = labelsize, xpd = TRUE)
# text(1.04, 41, expression(paste("supply")), cex = labelsize, xpd = TRUE)



segments(0, wagelvls[4], LPoints[1], wagelvls[4], lty = 2, lwd = segmentlinewidth, col = grays[20])

#segments(0, Mch(LPoints[1]), LPoints[1], Mch(LPoints[1]), lty = 2, lwd = segmentlinewidth, col = grays[20])
segments(LPoints[1], 0 , LPoints[1], Mch(LPoints[1]), lty = 2, lwd = segmentlinewidth, col = grays[20])
segments(0, WageFn(LPoints[1]), LPoints[1], WageFn(LPoints[1]), lty = 2, lwd = segmentlinewidth, col = grays[20])

#segments(BPoints[1], 0, BPoints[1], MRP(BPoints[1]), lty = 2, lwd = segmentlinewidth, col = grays[20])

#ARPl, no monopoly
segments(0, wagelvls[3], LPoints[1], wagelvls[3], lty = 2, lwd = segmentlinewidth, col = grays[22])
segments(LPoints[1], 0, LPoints[1], wagelvls[3], lty = 2, lwd = segmentlinewidth, col = grays[22])

#Union wage
segments(0, wagelvls[6], xlims[2], wagelvls[6], lty = 1, lwd = graphlinewidth, col = CBCols[3])


#Green part of min wage
#segments(0, 19 - 0.3, BPoints[3], WageFn(BPoints[3]) - 0.3, lty = 1, lwd = graphlinewidth, col = COLA[4])

# points(BPoints[1], MRP(BPoints[1]), pch = 16, col = "black", cex = 1.5)
# text(BPoints[1], MRP(BPoints[1]) + 1.5, expression(paste(d)), cex = labelsize)


# segments(BPoints[3], 0, BPoints[3], WageFn(BPoints[3]), lty = 2, lwd = segmentlinewidth, col = grays[20])
# points(BPoints[3], WageFn(BPoints[3]), pch = 16, col = "black", cex = 1.5)
# text(BPoints[3] + 0.02, WageFn(BPoints[3]) - 1.2, expression(paste(g)), cex = labelsize)


points(LPoints[1], MRevenue(LPoints[1]), pch = 16, col = "black", cex = 1.5)
text(LPoints[1] + 0.02, MRevenue(LPoints[1]) + 1, expression(paste(u)), cex = labelsize)

points(LPoints[1], AvgRevenue(LPoints[1]), pch = 16, col = "black", cex = 1.5)
text(LPoints[1], AvgRevenue(LPoints[1]) + 1.5, expression(paste(u*minute)), cex = labelsize)

points(LPoints[1], WageFn(LPoints[1]), pch = 16, col = "black", cex = 1.5)
text(LPoints[1] + 0.03, WageFn(LPoints[1]) - 0.8, expression(paste(u*minute*minute)), cex = labelsize)


# segments(LPoints[3], 0, LPoints[3], AvgRevenue(LPoints[3]), lty = 2, lwd = segmentlinewidth, col = grays[22])
# points(LPoints[3], MRevenue(LPoints[3]), pch = 16, col = "black", cex = 1.5)
# text(LPoints[3] - 0.02, MRevenue(LPoints[3]) - 1, expression(paste(b)), cex = labelsize)
# points(LPoints[3], AvgRevenue(LPoints[3]), pch = 16, col = "black", cex = 1.5)
# text(LPoints[3], AvgRevenue(LPoints[3]) + 1.3, expression(paste(b*minute)), cex = labelsize)

# brackets(x1 = LPoints[3] - 0.015, y1 = MRevenue(LPoints[3])  + 0.5, 
#          x2 = LPoints[3] - 0.015, y2 = AvgRevenue(LPoints[3]) - 0.5,  
#          ticks = 0.3, curvature = 0.5, type = 1, h = 0.02,
#          col = "black", lwd = 1.5, lty = 1, xpd = TRUE)
# 
# text(LPoints[3] - 0.05, MRevenue(LPoints[3]) + 5, expression(paste(B)), cex = labelsize)


# segments(LPoints[2], 0, LPoints[2], Mch(LPoints[2]), lty = 2, lwd = segmentlinewidth, col = grays[22])
# points(LPoints[2], Mch(LPoints[2]), pch = 16, col = "black", cex = 1.5)
# text(LPoints[2] - 0.02, Mch(LPoints[2]) + 1, expression(paste(a)), cex = labelsize)
# points(LPoints[2], WageFn(LPoints[2]), pch = 16, col = "black", cex = 1.5)
# text(LPoints[2] - 0.02, WageFn(LPoints[2]) + 1, expression(paste(a*minute)), cex = labelsize)

# brackets(x1 = LPoints[2] + 0.015, y1 = Mch(LPoints[2])  - 0.5, 
#          x2 = LPoints[2] + 0.015, y2 = WageFn(LPoints[2]) + 0.6,  
#          ticks = 0.2, curvature = 0.5, type = 1, h = 0.02,
#          col = "black", lwd = 1.5, lty = 1, xpd = TRUE)
# 
# text(LPoints[2] + 0.05, Mch(LPoints[2]) - 3.5, expression(paste(A)), cex = labelsize)


# points(LPoints[1], WageFn(LPoints[1]), pch = 16, col = "black", cex = 1.5)
# text(LPoints[1] + 0.02, WageFn(LPoints[1]) - 1, expression(paste(m)), cex = labelsize)

#Arrows for effects of min wage
# Arrows(LPoints[1] - 0.01, -1, BPoints[1] + 0.02, -1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", xpd = TRUE)
# Arrows(-0.05, WageFn(LPoints[1]) + 1.5, - 0.05, MRP(BPoints[1]) - 1.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", xpd = TRUE)


# text(0.15, MRP(BPoints[1]) + 3.6, expression(paste("Minimum")), cex = labelsize)
# text(0.15, MRP(BPoints[1]) + 1.6, expression(paste("wage, ", w[d]^min)), cex = labelsize)

# text(0.1, 14, expression(paste("Increase")), cex = labelsize)
# text(0.1, 12, expression(paste("in earnings")), cex = labelsize)
# 
# text(0.15, 4, expression(paste("Earnings shared")), cex = labelsize)
# text(0.15, 2, expression(paste("by both cases")), cex = labelsize)

# text(0.475, 4, expression(paste("Decrease")), cex = labelsize)
# text(0.475, 2, expression(paste("in earnings")), cex = labelsize)

# text(0.6, 4, expression(paste("Decrease")), cex = labelsize)
# text(0.6, 2, expression(paste("in earnings")), cex = labelsize)
# Arrows(LPoints[1] + 0.04, 3, BPoints[1] + 0.05, 3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", xpd = TRUE)

dev.off()

