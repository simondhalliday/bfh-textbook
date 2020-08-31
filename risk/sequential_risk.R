#' Graph Designer: Simon Halliday, Scott Cohn
#' Authors: Bowles and Halliday
#' Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "risk/sequential_risk.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 2
actionlabelsize <- 1.5
graphlinewidth <- 3
segmentlinewidth <- 2
nodesize <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(5, 5, 4, 0))

xlims <- c(0, 10)
ylims <- c(0, 10)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

ticksy <- seq(from = 0, to = ylims[2], by = 2)
ylabels <- seq(from = 0, to = ylims[2], by = 2)
ticksx <- seq(from = 0, to = xlims[2], by = 2)
xlabels <- seq(from = 0, to = xlims[2], by = 2)

#Branches for Agent
segments(5, 9, 5, 10, lty = 1, col = rgb(1, 0, 0, 0.5), lwd = graphlinewidth, xpd = TRUE)
#Fish 10
segments(5, 9, 2.5, 4, col = rgb(0, 0, 1, 0.5), lty = 1, lwd = graphlinewidth)
segments(2.5, 4, 1, 1, lty = 1,  lwd = graphlinewidth)

#Fish 12
segments(5, 9, 7.5, 4, lty = 1, col = rgb(0, 0, 1, 0.5), lwd = graphlinewidth)

segments(7.5, 4, 9, 1, lty = 1, lwd = graphlinewidth)

#Branches for Agent
#From Aram Fishing 10
#Bina fishes 10
segments(7.5, 4, 6, 1, lty = 1,  lwd = graphlinewidth)
#Bina fishes 12
segments(2.5, 4, 4, 1, lty = 1,  lwd = graphlinewidth)

#From Aram fishing 12
#Bina fishes 10
#segments(7.5, 5, 6, 1, lty = 1, col = rgb(1, 0, 0, 0.5), lwd = graphlinewidth)


#Bina fishes 12
#segments(7.5, 5, 9, 1, lty = 1, col = rgb(1, 0, 0, 0.5), lwd = graphlinewidth)

#Initial node
points(5, 10, pch = 16, col = "black", cex = nodesize, xpd = TRUE)
points(5, 9, pch = 16, col = "black", cex = nodesize)

#P1 Nodes
points(2.5, 4, pch = 16, col = "black", cex = nodesize)
points(7.5, 4, pch = 16, col = "black", cex = nodesize) 

#P2 Terminal Nodes
points(1, 1, pch = 16, col = "black", cex = nodesize)
points(4, 1, pch = 16, col = "black", cex = nodesize)
points(6, 1, pch = 16, col = "black", cex = nodesize)
points(9, 1, pch = 16, col = "black", cex = nodesize)

text(-0.55, 10.5, expression(paste("Insurer:")), cex = actionlabelsize, xpd = TRUE)
text(5, 10.5, expression(paste("W offers insurance at ", p)), cex = actionlabelsize, xpd = TRUE)
#text(5, 10.55, expression(paste("and wage, ", w)), cex = actionlabelsize, xpd = TRUE)

text(-0.45, 8.5, expression(paste("J decides:")), cex = actionlabelsize, xpd = TRUE)

text(2.75, 7.5, expression(paste("Pay")), cex = actionlabelsize)
text(2.75, 7, expression(paste(p[s](bar(Delta) - Delta^J))), cex = actionlabelsize)


text(6.95, 7.5, expression(paste("Reject")), cex = actionlabelsize)
text(6.95, 7, expression("Pay 0"), cex = actionlabelsize)

#text(-0.575, 6.15, expression(paste("Chance:")), cex = actionlabelsize, xpd = TRUE)

# text(2.2, 6.15-0.5, expression(paste("Worker is")), cex = actionlabelsize)
# text(2.2, 5.7-0.5, expression(paste("terminated")), cex = actionlabelsize)
# text(2.2, 5.25-0.5, expression(paste((t))), cex = actionlabelsize)

text(-0.575, 3.55, expression(paste("Chance:")), cex = actionlabelsize, xpd = TRUE)

text(1.4, 3.55, expression(paste("Bad")), cex = actionlabelsize)
text(3.6, 3.55, expression(paste("Good")), cex = actionlabelsize)
text(6.4, 3.55, expression(paste("Bad")), cex = actionlabelsize)
text(8.6, 3.55, expression(paste("Good")), cex = actionlabelsize)



#Payoffs 
text(-1, 0.5, expression(paste("W:")), cex = actionlabelsize, col = rgb(1, 0, 0, 1), xpd = TRUE)
text(-1, -1.2, expression(paste("J:")), cex = actionlabelsize, col = rgb(0, 0, 1, 1), xpd = TRUE)


text(0.6, 0.5, expression(y[z]^W - p[s](bar(Delta) - Delta^J) - frac(Delta^W, 2)), cex = actionlabelsize, col = rgb(1, 0, 0, 1), xpd = TRUE)
text(0.6, -1.2, expression(y[z]^J - p[s](bar(Delta) - Delta^J) - frac(Delta^J, 2)), cex = actionlabelsize, col = rgb(0, 0, 1, 1),xpd = TRUE)

text(3.7, 0.5, expression(y[z]^W - p[s](bar(Delta) - Delta^J) + frac(Delta^W, 2)), col = rgb(1, 0, 0, 1), cex = actionlabelsize, xpd = TRUE)
text(3.7, -1.2, expression(y[z]^J - p[s](bar(Delta) - Delta^J) + frac(Delta^J, 2)), col = rgb(0, 0, 1, 1), cex = actionlabelsize, xpd = TRUE)

text(6, 0.5, expression(y[z]^W), cex = actionlabelsize, col = rgb(1, 0, 0, 1), xpd = TRUE)
text(6, -1.2, expression(y[z]^J - frac(bar(Delta), 2)), col = rgb(0, 0, 1, 1), cex = actionlabelsize, xpd = TRUE)

text(9, 0.5, expression(y[z]^W), cex = actionlabelsize, col = rgb(1, 0, 0, 1), xpd = TRUE)
text(9, -1.2, expression(y[z]^J + frac(bar(Delta), 2)), col = rgb(0, 0, 1, 1), cex = actionlabelsize, xpd = TRUE)

dev.off()