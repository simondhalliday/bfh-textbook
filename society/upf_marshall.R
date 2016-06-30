require(ggplot2)
require(shape)
pdf(file = "society/upf_marshall.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
par(mar =  c(5, 5, 4, 2))
xlims <- c(0, 115)
ylims <- c(0, 115)

production <- function(ea, eb){
  (15 - (1/4)*eb)*ea
}

uA1 <- function(ea, eb){
  (15 - (1/4)*eb)*ea - (1/2)*(ea)^2  
}

uB1 <- function(ea, eb){
  (15 - (1/4)*ea)*eb - (1/2)*(eb)^2  
}

uAalt <- function(ea, production){
  production - (1/2)*(ea)^2  
}

eA <- function(gamma, delta = 12){
  delta/(1 + gamma)
}

eAstar <- function(gamma, delta = 12){
  delta/(1 + 2*gamma)
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
ticksy <- seq(ylims[1], ylims[2], 5)
ylabels <- seq(ylims[1], ylims[2], 5)
ticksx <- seq(xlims[1], xlims[2], 5)
xlabels <- seq(xlims[1], xlims[2], 5)
# ticksy <- c(65, 70, 72, 73, 75, 78, ylims[2])
# ylabels <- c(65, 70, 72, 73, 75, 78, NA)
# ticksx <- c(65, 70, 72, 75, 77, 78, xlims[2])
# xlabels <- c(65, 70, 72, 75, 77, 78, NA)
axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Rent polygon
# xrent <- c(2, 3, 3.5, 2)
# yrent <- c(3.5, 3, 2, 2)
# polygon(xrent, yrent, col="powderblue", density=NULL, border = NA)


#Lines for the coordinates of the Nash equilbrium
#segments(70, 78, 75, 75, lty = 1, col = "#7fc97f", lwd = 4)
#segments(78, 70, 75, 75, lty = 1, col = "#7fc97f", lwd = 4)

#Lines for Fallback positions
segments(65, 72, xlims[2], 72, lty = 2, col = "darkgrey", lwd = 3)
text(100, 75, expression(paste("A's fallback")))
segments(72, 65, 72, ylims[2], lty = 2, col = "darkgrey", lwd = 3)
text(75, 100, expression(paste("B's fallback")))

#Add points a, b, c and c
# points(70, 78, pch = 16, col = "black", cex = 1.5)
# text(1.1, 4.1, expression(paste("b")))
points(75, 75, pch = 16, col = "black", cex = 1.5)
text(3.1, 3.1, expression(paste("c")))
# points(78, 70, pch = 16, col = "black", cex = 1.5)
# text(4.1, 1.1, expression(paste("d")))
points(72, 72, pch = 16, col = "black", cex = 1.5)
text(72.5, 72.5, expression(paste("a")))
# points(3.5, 2, pch = 16, col = "black", cex = 1.5)
# text(3.6, 2.1, expression(paste("f")))
# points(2, 3.5, pch = 16, col = "black", cex = 1.5)
# text(2.1, 3.6, expression(paste("e")))
text(73, 73, expression(paste("Nash equilibrium")))


points(77, 73, pch = 16, col = "black", cex = 1.5)
text(77.5, 73.5, expression(paste("q")))

points(73, 77, pch = 16, col = "black", cex = 1.5)
text(73.5, 77.5, expression(paste("p")))

points(62, 84.5, pch = 16, col = "black", cex = 1.5)
points(84.5, 62, pch = 16, col = "black", cex = 1.5)
points(38, 98, pch = 16, col = "black", cex = 1.5)
points(98, 38, pch = 16, col = "black", cex = 1.5)
points(91.125, 51.75, pch = 16, col = "black", cex = 1.5)
points(51.75, 91.125, pch = 16, col = "black", cex = 1.5)
points(20.75, 105.25, pch = 16, col = "black", cex = 1.5)
points(105.25, 20.75, pch = 16, col = "black", cex = 1.5)
points(0, 112.5, pch = 16, col = "black", cex = 1.5)
points(112.5, 0, pch = 16, col = "black", cex = 1.5)


#Label utility possibilities frontier
# text(3.8, 2.6, expression(paste("Utility Possibilities")))
# text(3.8, 2.35, expression(paste("Frontier (upf)")))

#Arrows and economic rent label
# Arrows(2.7, 3.7, 2.7, 2.7, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
# text(2.8, 3.8,  expression(paste("Economic Rent")))

dev.off()

