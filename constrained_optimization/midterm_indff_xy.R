require(ggplot2)
require(shape)
pdf(file = "constrained_optimization/midterm_indiff_xy.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 15)
ylims <- c(0, 200)

mrs <- function(x){
  20 - x
}

utility <- function(x, y){
  y + 20*x - 0.5*(20/20)*x^2
}

#Plot command 
plot(0, 0, xlim = xlims, ylim = ylims, 
     type = "n",
     ylab = expression(paste("Quantity of money, ", y)),
     xlab = expression(paste("Quantity of hot chocolate, ", x)),
     axes = FALSE,
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i"
)


#Customize ticks and labels for the plot
# ticksy <- seq(ylims[1], ylims[2], 5)
# ylabels <- seq(ylims[1], ylims[2], 5)
# ticksx <- seq(xlims[1], xlims[2], 5)
# xlabels <- seq(xlims[1], xlims[2], 5)
ticksy <- seq(ylims[1], ylims[2], 10)
ylabels <- seq(ylims[1], ylims[2], 10)
ticksx <-seq(xlims[1], xlims[2], 5)
xlabels <-seq(xlims[1], xlims[2], 5)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)


xx1 <- seq(xlims[1], xlims[2], length.out = 500)
#lines(xx1, mrs(xx1), col = COLB[4], lwd = graphlinewidth)

a <- c(175, 200, 225)
npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts)

contour(x, y,
        outer(x, y, utility),
        drawlabels = FALSE,
        col = COLA[3],
        lwd = graphlinewidth,
        levels = a,
        xaxs="i",
        yaxs="i",
        add = TRUE)


#Label utility possibilities frontier
# Arrows(40, 80, 40, 65, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
text(12, 10, expression(paste(u[1])))
text(12, 35, expression(paste(u[2])))
text(12, 60, expression(paste(u[3])))

dev.off()

