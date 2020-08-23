require(shape)
require(plotrix)
pdf(file = "capitalism/employment_unions_bad_v2.pdf", width = 9, height = 7)

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
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442")

WageFn <- function(h, ubar = 3, B = 2, t = 0.8) {
  B + ubar + (ubar - ubar*t +  ubar*t*h )/(t*(1 - h))
}


par(mar =  c(5, 6, 1.1, 1.1))
xlims <- c(0, 1)
ylims <- c(0, 50)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = "",
     ylab = "",
     xaxt = "n", 
     yaxt = "n",
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs = "i", 
     yaxs = "i")

text(-0.11, 0.5*ylims[2], expression(paste("Wage, ", w)), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(0.5*xlims[2], -6, expression(paste("Hours of employment as a proportion, ", H)), xpd = TRUE, cex = axislabelsize) 


npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], 1, length.out = npts)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
xx4 <- seq(xlims[1], 25, length.out = npts2)

#Draw the lines for the graphs
lines(xx1, WageFn(xx1), col = CBCols[1], lwd = graphlinewidth)
#lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 4)
#lines(xx2, solowInfeas(xx2, delta = 5), col = COL[1], lwd = 4, lty = 2)

#Union raises the wage function
lines(xx1, WageFn(xx1, ubar = 7), col = CBCols[1], lwd = graphlinewidth)

#Customize ticks and labels for the plot
#ticksy <- c(0, 5, 10, 20,  40)
#ylabels <- c(0, expression(paste(B)), expression(paste(B+underline(u)/t[0])), expression(paste(B+underline(u)/t[1])), expression(paste(w[0])), NA)
#ylabels <- c(0,  expression(paste(B + underline(u[0]))), expression(paste(B + underline(u[1]))), expression(paste(w^c)), NA)
ticksy <- c(0, 20,  40, 50)
#ylabels <- c(0, expression(paste(B)), expression(paste(B+underline(u)/t[0])), expression(paste(B+underline(u)/t[1])), expression(paste(w[0])), NA)
ylabels <- c(0,  expression(paste(w^c)), expression(paste(gamma[0])), NA)


ticksx <- c(0, 0.513, 0.7925, 1, xlims[2])
xlabels <- c(0, expression(paste(H[1]^N)),expression(paste(H[0]^N)), 1.0, NA)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

#Annotation of the  graphs
text(0.97, 50, expression(paste("Wage")), cex = labelsize,xpd = TRUE)
text(0.97, 47.5, expression(paste("curve")), cex = labelsize,xpd = TRUE)
text(0.97, 44.5, expression(paste(w[0]^N*(H))), cex = labelsize,xpd = TRUE)


text(0.73, 44.5, expression(paste(w[1]^N*(H))), cex = labelsize, xpd = TRUE)

#segments(1, 0, 1, 42, lty = 2, lwd = 3, col = "darkgray")
segments(0.7925, 0, 0.7925, 20, lty = 2, lwd = segmentlinewidth, col = grays[20])

#Arrows(0.8, 15, 0.8, 19, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#Arrows(0.8, 15, 0.8, 11, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#text(0.98, 16.5, expression(paste("Employment rent")))
#text(0.98, 15, expression(paste("decreases with union")))

#Labor productivity 
segments(0, 40, xlims[2], 40, lty = 2, lwd = segmentlinewidth, col = CBCols[3], xpd = TRUE)


#Original competition condition 
segments(0, 20, 0.7925, 20, lty = 1, lwd = segmentlinewidth, col = CBCols[2])
segments(0.7925, 20, 1.2, 20, lty = 2, lwd = segmentlinewidth, col = CBCols[2])

points(0.513, 20, pch = 16, col = "black", cex = 1.5)
text(0.77, 21.6, expression(paste(n[0])), cex = labelsize)

#segments(0, 10, 1.2, 10, lty = 2, lwd = 2, col = "darkgray")
#text(0.97, 11, expression(paste(B + a[1])))



# Arrows(0.7, 20.5, 0.7, 29, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
# text(0.51, 28.2, expression(paste("Union raises")), cex = labelsize, xpd = TRUE)
# text(0.51, 26, expression(paste("wage curve")), cex = labelsize, xpd = TRUE)

segments(0.513, 0, 0.513, 20, lty = 2, lwd = segmentlinewidth, col = grays[20])
points(0.7925, 20, pch = 16, col = "black", cex = 1.5)
text(0.5, 21.6, expression(paste(n[1])), cex = labelsize)
#text(1.02, 26, expression(paste(zpc[1], ", ", w == w[1])))


#Unemployment benefits & a
#segments(0, 5, 1.2, 5, lty = 2, lwd = 2, col = "darkgray")
#segments(0, 2.5, 1.2, 2.5, lty = 2, lwd = segmentlinewidth, col = grays[20])

#Zero profit condition
text(0.2, 23.7, expression(paste("Competition")), cex = labelsize, xpd = TRUE)
text(0.2, 21.5, expression(paste("condition, ", w^c)), cex = labelsize, xpd = TRUE)
#text(1.00, 18.5, expression(paste(w^c)), cex = labelsize, xpd = TRUE)

Arrows(0.32, 8.2, 0.32, 13.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(0.2, 10.7, expression(paste("Union raises")), cex = labelsize, xpd = TRUE)
text(0.2, 8.3, expression(paste("wage curve")), cex = labelsize, xpd = TRUE)


#text(0.97, 6, expression(paste(B + a[0])))
#text(0.97, 3.5, expression(paste(B, " (unemployment benefits)")))
#text(1.08, 36, expression(paste("level of")))
#text(1.08, 34, expression(paste("employment, ", bar(H))))


dev.off()

