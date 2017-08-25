#Graph Designer: Simon Halliday & Riley Boeth '17 & Madeleine Wettach '20
#Authors: Bowles, Foley and Halliday
#Title: Microeconomics: Competition, Conflict and Coordination

library(shape)
pdf(file = "constrained_optimization/step_by_step_graphs_3/indiff_feasible_environ_rd_step1.pdf", width = 8, height = 8)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.1
graphlinewidth <- 3
segmentlinewidth <- 2

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 4, 4, 4))

#Feasibility frontier equation and new feasbility frontier equation with R&D

ppf <- function(x, y) {
  100-((x^2)/2)
}

ppf_rd <- function(x, y) {
  100-((x^2)/3)
}

#change alpha to .4

uFn <- function(x, y, alpha = 0.4){
  (x^alpha)*(y^(1-alpha))
}

#Add limits on axes and levels of utility for each indifference curve

ylims <- c(0, 130)
xlims <- c(0, 20)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

a <- c(27, 29.16, 31.616) #alpha = 0.4

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

#x and y limits and ticks 

# ticksx <- seq(from = 0, to = xlims[2], by = 1)
# xlabels <- seq(from = 0, to = xlims[2], by = 1)
# ticksy <- seq(from = 0, to = ylims[2], by = 10)
# ylabels <- seq(from = 0, to = ylims[2], by = 10)

ticksx <- c(xlims[1], 7.07, 8.66, xlims[2])
xlabels <- c(NA, expression(paste(e^"*")), expression(paste(e[RD]^"*")), NA)
ticksy <- c(ylims[1], ppf(x = 7.07), 100, ylims[2])
ylabels <- c(NA, expression(paste(g^"*")), expression(paste(g[max])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, xlims[2], length.out = npts)
xx3 <- seq(xlims[1], 0, length.out = npts)
xx4 <- seq(-11, 0, length.out = npts)

#Arrows(1, 78, 1, 97, col = "black", lty = 1, lwd = 2, code = 3, arr.type = "triangle", arr.lwd = 0.5)
#text(2.5, 88, expression("Abatement"), cex = labelsize)
#text(2.5, 84, expression("Cost"), cex = labelsize)

#Draw the feasibility frontiers
#lines(xx1, ppf(xx1, y), col = COLA[5], lwd = graphlinewidth)
#lines(xx1, ppf_rd(xx1, y), col = COLA[5], lwd = graphlinewidth)

#Label the feasibility frontier frontier
#text(14.8, 15, expression("Initial"), cex = labelsize)
#text(14.8, 11, expression("Feasible"), cex = labelsize)
#text(14.8, 7, expression("Frontier"), cex = labelsize)


#Label the feasibility frontier with R&D frontier
#text(18.2, 15, expression("Feasible"), cex = labelsize)
#text(18.2, 11, expression("Frontier"), cex = labelsize)
#text(18.2, 7, expression("With R&D"), cex = labelsize)
#Arrows(14.2, 3, 16.6, 3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

contour(x, y, 
        outer(x, y, uFn),
        drawlabels = FALSE,
        col = COLB[3],
        lwd = graphlinewidth,
        levels = a, 
        xaxs="i", 
        yaxs="i", 
        add = TRUE)

#Axis labels
mtext(expression(paste("Environmental quality, ", e)), side = 1, line = 2.5, cex = axislabelsize)
text(-1.8, 0.5*ylims[2], expression(paste("Goods in millions, ", g)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the indifference curves generally
text(7.2, 120, expression("Policy-Maker's"), cex = labelsize)
text(7.2, 116, expression("Indifference Curves"), cex = labelsize)

#Label the indifference curves and place labels properly
text(19, 36, expression(u[1]), cex = labelsize)
text(19, 40.8, expression(u[2]), cex = labelsize)
text(19, 46.5, expression(u[3]), cex = labelsize)

#Annotate max u point on feasibility with r&D frontier
#text(9.1, ppf_rd(8.66) + 3, expression(paste(h)), cex = labelsize)
#segments(8.66, 0, 8.66, ppf_rd(x = 8.66), lty = 2, col = "gray", lwd = segmentlinewidth)
#segments(0, ppf_rd(x = 8.66), 8.66, ppf_rd(x = 8.66), lty = 2, col = "gray", lwd = segmentlinewidth)
#points(8.66, ppf_rd(x = 8.66), pch = 16, col = "black", cex = 1.5)

#Annotate max u point on feasibility frontier
#text(7.3, ppf(7.07) + 3, expression(paste(f)), cex = labelsize)
#segments(7.07, 0, 7.07, ppf(x = 7.07), lty = 2, col = "gray", lwd = segmentlinewidth)
#segments(0, ppf(x = 7.07), 7.07, ppf(x = 7.07), lty = 2, col = "gray", lwd = segmentlinewidth)
#points(7.07, ppf(x = 7.07), pch = 16, col = "black", cex = 1.5)

#Added arrow for abatement costs and labeled the arrow as such
# arrows(.2, 75, .2, 100, length = 0.1, angle = 30, code = 3, col = par("fg"), lty = par("lty"),   lwd = par("lwd"))
# text(1.71, 88, expression(paste("Abatement")), cex = labelsize)
# text(1.05, 84, expression(paste("Costs")), cex = labelsize)


dev.off()