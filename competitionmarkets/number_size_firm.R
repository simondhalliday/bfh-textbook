require(shape)
pdf(file = "/Users/saimadhurika/Dropbox/Fig 1/fig1", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 4, 6, 4))

A <- function(s2 = 2.1, x, pmin = -6) {
 (pmin + s2*x)
}

B <- function(s1 = 1.2, x) {
  (0 + s1*x)
}  

C <- function(s1 = 1.5, x) {
  (0 + s1*x)
}  


xlims <- c(-1, 20)
ylims <- c(-8, 20)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
#a <- c(2, 18, 32.5)
#b <- c(60, 72, 81)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

 ticksy <- seq(from = -7, to = ylims[2], by = 2)
 ylabels <- seq(from = 0, to = ylims[2], by = 2)
 ticksx <- seq(from = 0, to = xlims[2], by = 2)
 xlabels <- seq(from = 0, to = xlims[2], by = 2)
 
 
 ticksy <- c(-8, 8, 15, ylims[2])
 ylabels <- c(NA, NA, NA, NA)
 ticksx <- c(-1, 6.66, 10, xlims[2])
 xlabels <- c(NA, expression(paste(bar(s),"*")),expression(paste(bar(s)^"+")),NA)
 
 

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)


npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

lines(xx1, A(xx1, s2 = 2.1, pmin = -6), col = COLA[4], lwd = graphlinewidth)
lines(xx1, B(xx1, s1 = 1.2), col = COLB[4], lwd = graphlinewidth)
lines(xx1, C(xx1, s1 = 1.5), col = COLB[4], lwd = graphlinewidth, lty=2)

mtext(expression(paste("Average firm size, ", bar(s))), side=1, line = 2.5, cex = axislabelsize)
text(-2, 0.5*ylims[2], expression(paste("")), xpd = TRUE, cex = axislabelsize, srt = 90)
mtext(expression(paste("Increase or decrease in average firm size")), side=2, line = 1, cex = axislabelsize)



segments(0, 8, 6.66, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(6.66, 0, 6.66, 8, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(0, 15, 10, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(10, 0, 10, 15, lty = 2, col = "gray" , lwd = segmentlinewidth)
points(0, -6, pch = 1, col = "black", cex = 0)
text(-0.65, -6, expression(-delta*underline(s)) )

text(10, A(x=11.4), expression(delta(bar(s) - underline(s))))
text(15, B(x=12.4), expression(bar(s)(1 - delta)*b))
text(13.5, C(x=12), expression(paste(bar(s)(1 - delta)*b^"+")))
text(9.5, A(x=12), expression(paste("Firm death effect")))
text(14.5, B(x=11.5), expression(paste("Firm growth effect")))

dev.off()