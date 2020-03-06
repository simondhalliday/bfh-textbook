require(shape)
pdf(file = "indmarketdemand/cs_tax_jane.pdf", width = 9, height = 7)

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

par(mar =  c(5, 7, 1, 1))

mrsA <- function(x, rmax = 20, xmax = 10) {
  rmax - (rmax/xmax)*x
}

uA <- function(x, y, rmax = 20, xmax = 10) {
  y + rmax*x - (1/2)(rmax/xmax)*x^2
}

xlims <- c(0, 11)
ylims <- c(0, 22)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(46.08, 55, 64)
b <- c(46.08, 55, 64)

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
ticksy <- c(0, 10, 12, 20, ylims[2])
ylabels <- c(NA, expression(paste(p[b] == 10)), expression(paste(p[a] == 12)), expression(paste(bar(p) == 20)), NA)
ticksx <- c(0, 4, 5, 10, xlims[2])
xlabels <- c(0, expression(paste(x[a]) == 4), expression(paste(x[b]) == 5), expression(paste(bar(x) == 10)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

xpoly <- c(0, 4, 0)
ypoly <- c(12, 12, 20)
polygon(x = xpoly, y = ypoly, col = COL[4], density=NULL, border = NA)

xpoly1 <- c(0, 4, 4, 0)
ypoly1 <- c(10, 10, 12, 12)
polygon(x = xpoly1, y = ypoly1, col = COLB[1], density=NULL, border = NA)


xpoly2 <- c(4, 5, 4)
ypoly2 <- c(10, 10, 12)
polygon(x = xpoly2, y = ypoly2, col = COLA[1], density=NULL, border = NA)


#Lines for mrs graph
lines(xx1, mrsA(xx1, rmax = 20, xmax = 10), col = COLA[4], lwd = graphlinewidth)

segments(0, 12, xlims[2], 12, lty = 1, col = COLB[5] , lwd = graphlinewidth)
segments(0, 10, xlims[2], 10, lty = 1, col = COLB[4] , lwd = graphlinewidth)
segments(5, 0, 5, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(4, 0, 4, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)

#Label axes
text(0.5*xlims[2], -3, expression(paste("Quantity of fish, ", x)), xpd = TRUE, cex = axislabelsize) 
text(-1.8, 0.5*ylims[2], expression(paste("Price per kilogram, ", p)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label i
points(5, 10, pch = 16, col = "black", cex = 1.5)
text(5, 10.75, expression(b), cex = annotatesize)

#Label g
points(4, 12, pch = 16, col = "black", cex = 1.5)
text(4, 12.75, expression(a), cex = annotatesize)

#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
text(8.5, 12.5, expression(paste("Market price with tax, ", p + t == 12)), cex = labelsize)
text(7.75, 10.5, expression(paste("Market price, ", p == 10)), cex = labelsize)
#text(7.3, 2.5, expression("Curve"))
#Arrows(7.3, 3.5, 7.3, 6.1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the iso-welfare functions for the HG, Aisha
# text(3.8, 1.5, expression(u[1]^A))
# text(4.6, 1.5, expression(u[2]^A))
# text(5.5, 1.5, expression(u[3]^A))
#text(6.6, 8.3, expression(u[4]^A))


#Label the indifference curves for the HG, Betty
# text(7.6, 17, expression(u[1]^B))
# text(6.75, 17, expression(u[2]^B))
# text(6, 17, expression(u[3]^B))
#text(3.4, 6.9, expression(v[4]^B))

#Label mrs function
# text(8.6, 5.5, expression(paste(p(x) == 20 - 2*x)))
text(8.05, 8, expression(paste("Demand, ", p(x) == 20 - 2*x)), cex = labelsize)

#Arrows(10, 7.5, 10, 5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label satiation
#text(20, 3.5, expression(paste(x[max] == "Point")))
#text(20, 3, expression(paste("of Satiation")))
#Arrows(20, 2.5, 20, 0.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label highest willingness to pay
text(1, 16, expression(paste("Consumer" )), cex = annotatesize)
text(1, 14.7, expression(paste("Surplus" )), cex = annotatesize)
text(1.25, 13.2, expression(paste(cs[a]==frac(1, 2)*bgroup("(",bar(p) - p[a],")")*x)), cex = annotatesize)
#text(2.5, 19, expression(paste(CS==frac(1, 2)*bgroup("(",20 - 12,")")*4, phantom()== 16)))
#Arrows(2, 18, 2, 13, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label tax revenue
text(2, 11, expression(paste("Tax Revenue, ", R==t%.%x[a])), cex = annotatesize)
#text(3.5, 18, expression(paste(CS==frac(1, 2)*bgroup("(",r[max] - p,")")*x)))
#text(2, 2, expression(paste()))
#text(2.5, 0.75, expression(paste(R==2*bgroup("(",12 - 10,")")*4, phantom()== 8)))
#Arrows(2, 4, 2, 10.75, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label deadweight loss
text(4.25, 17, expression("Deadweight Loss"), cex = annotatesize)
#text(3.5, 18, expression(paste(CS==frac(1, 2)*bgroup("(",r[max] - p,")")*x)))
text(4.25, 15.5, expression(paste(dwl==frac(1,2)*bgroup("(",x[b] - x[a],")")*t)), cex = annotatesize)
#text(4.5, 14.5, expression(paste(DWL==frac(1,2)*bgroup("(",12 - 10,")")*bgroup("(",5 - 4,")"), phantom()== 1)))
Arrows(4.25, 14, 4.25, 10.75, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)



dev.off()
