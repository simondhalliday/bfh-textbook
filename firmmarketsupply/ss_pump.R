#require(shape)
pdf(file = "firmmarketsupply/ss_pump.pdf", width = 9, height = 7)
par(mar =  c(5, 6, 1, 1))
#Set parameters for graphics

# I tried to get three points that I could roughly read off of the graph (with error, because it's hard to tell exactly). 
# x = 3 => total cost of 750 to 800 i.e. ac of ~250ish
# x = 10 => 1600 to 1800, i.e. ac(x) of 160 to 180
# x = 20 => 2600 to 2900, i.e. ac(x) of 130 to 145. 
# This was reasonably close to that, so I think I'm going to run with it. 
#just for up to date lookngness i suggest we put it in 2020 prices 
#i.e from https://www.multpl.com/gdp-deflator/table/by-year 
#just multiply everything by 112.8/64.4 = 1.75 
#(thats  2020 gdp def in jan 2020/dec 1990)

#Changed for marginnote
pointsize <- 1.8
axislabelsize <- 2.2
labelsize <- 2
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")

xlims <- c(0, 30)
ylims <- c(0, 600)

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

cost <- function(x = xx1, a = 370, b = 0.65, deflator = 1.75){
  deflator*a*x^(b - 1)
}

totalcost <- function(x = xx1,  a = 370, b = 0.65){
    a*x^(b)
}

pricechange <- function(x, deflator = 1.75){
  deflator*x
} 

points <- c(3, 10, 20)
ticksy <- c(0, cost(points), ylims[2])
ylabels <- c(0, round(cost(points), 0), ylims[2])
ticksx <- c(0, points, xlims[2])
xlabels <- c(0, points, xlims[2]) 

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)



# Cost Func.
lines(xx1, cost(), col = CBCols[1], lwd = graphlinewidth)

# Label Cost Func
#text(3.5, 18, expression(paste("Stainless steel")), cex = labelsize)
#text(3.5, 17, expression(paste("gear pump")), cex = labelsize)

text(26.5, cost(26.5) + 60, expression(paste("Average cost")), cex = labelsize, xpd = TRUE)
text(26.5, cost(26.5) + 25, expression(paste(ac(x))), cex = labelsize, xpd = TRUE)

# Point
points <- c(3, 10, 20)
segments(0, cost(points[1]), points[1], cost(points[1]), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(points[1], 0, points[1], cost(points[1]), lty = 2, col = grays[20] , lwd = segmentlinewidth)
points(points[1], cost(points[1]), pch = 16, col = "black", cex = 1.5)
text(points[1] + 0.75, cost(points[1]) + points[1], expression(e), cex = labelsize)


segments(0, cost(points[2]), points[2], cost(points[2]), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(points[2], 0, points[2], cost(points[2]), lty = 2, col = grays[20] , lwd = segmentlinewidth)
points(points[2], cost(points[2]), pch = 16, col = "black", cex = 1.5)
text(points[2] + 0.5, cost(points[2]) + 15, expression(f), cex = labelsize)

segments(0, cost(points[3]), points[3], cost(points[3]), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(points[3], 0, points[3], cost(points[3]), lty = 2, col = grays[20] , lwd = segmentlinewidth)
points(points[3], cost(points[3]), pch = 16, col = "black", cex = 1.5)
text(points[3] + 0.5, cost(points[3]) + 15, expression(g), cex = labelsize)


# Label x,y axis
#mtext(expression(paste("Capacity, gallons per minute")), side=1, line = 2.5, cex = axislabelsize)
text(0.5*xlims[2], -70,  expression(paste("Capacity, gallons per minute")), xpd = TRUE, cex = axislabelsize) 
text(-4, 0.5*ylims[2], expression(paste("Pump cost per gallons per minute, $")), xpd = TRUE, cex = axislabelsize, srt = 90) 


dev.off()