require(ggplot2)
require(shape)
pdf(file = "risk/risk_return_shaded.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 15)
ylims <- c(0, 18)

riskreturn <- function(g, int1 = 14, int2 = 4, coeff = 1/3){
  int1 - (int2 - (coeff)*g)^2
}

indiffA <- function(g, intercept = 5.6, slope = 0.125){
  intercept  + slope*g^2
}


#Plot command 
plot(0, 0, xlim = xlims, ylim = ylims, 
     type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     axes = FALSE,
     # xlab = expression(paste("Bob's Payoff, ", u^B)),
     # ylab = expression(paste("Alfredo's Payoff, ", u^A)),
     #xaxt = "n", 
     #yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i"
)


#Customize ticks and labels for the plot
# ticksy <- seq(ylims[1], ylims[2], 3)
# ylabels <- seq(ylims[1], ylims[2], 3)
# ticksx <- seq(xlims[1], xlims[2], 5)
# xlabels <- seq(xlims[1], xlims[2], 5)
ticksy <- c(0,  riskreturn(g = 12) ,ylims[2])
ylabels <- c(NA, expression(paste(omega[max])), NA)
ticksx <- c(0,  12, xlims[2])
xlabels <- c(NA, expression(paste(Delta[omega*max])), NA)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)
mtext(expression(paste("Difference in wealth (good versus bad outcome), ", Delta, ", risk")), side = 1, line = 2.5, cex = axislabelsize)
text(xlims[1] - 1.5, ylims[2] - 0.5*(ylims[2] - ylims[1]), expression(paste("Expected wealth, ", omega)), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Draw the polygon for shading the utility areas
xpoly1 <- seq(from = xlims[1], to = xlims[2], length.out = 500)
ypoly1 <- riskreturn(xpoly1)
polygon(x = c(xlims[2], xlims[2], rev(xpoly1), 0),
        y = c(ylims[2], 0, rev(ypoly1), ylims[2]),
        col=COLB[1], density=NULL, border = NA)

xpoly2 <- seq(from = 3*(4 - sqrt(14)), to = xlims[2], length.out = 500)
ypoly2 <- riskreturn(xpoly2)
polygon(x = c(xpoly2[1], xlims[2], rev(xpoly2)),
        y = c(ylims[1], ylims[1], rev(ypoly2)),
        col=COLA[1], density=NULL, border = NA)


text(9.5, 10, expression(paste("Risk-return schedule, ", omega = g(Delta) )), cex = axislabelsize)
Arrows(9.5, 10.5, 9.5, 12.8, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(11, 3, expression(paste("Feasible combinations ")), cex = axislabelsize)
text(11, 2, expression(paste("of risk and expected wealth")), cex = axislabelsize)

text(4, 17, expression(paste("Infeasible combinations ")), cex = axislabelsize)
text(4, 16, expression(paste("of risk and expected wealth")), cex = axislabelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
lines(xx1, riskreturn(xx1), col = COLA[4], lwd = graphlinewidth, lty = 1)
#lines(xx1, indiffA(xx1, intercept = 5.6), col = COLB[4], lwd = graphlinewidth, lty = 1)


#Add points a, b, c and c
#points(5.6, 9.5, pch = 16, col = "black", cex = 1.5)



dev.off()

