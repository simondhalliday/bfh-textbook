require(shape)
pdf(file = "society/society_fig1_pefficiency.pdf", width = 9, height = 7)
#SDH: I changed these proportions to 9 by 7 for proprtionality to the figure its adjacent to

#Set parameters for graphics
axislabelsize <- 2
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.8
graphlinewidth <- 2
segmentlinewidth <- 1.5

par(mar =  c(4, 4, 0.5, 0.5))
xlims <- c(0, 6)
ylims <- c(0, 6)

ticksy <- c(ylims[1], 1, 2, 3, 4, 5, ylims[2])
ylabels <- c(ylims[1], 1, 2, 3, 4, 5, ylims[2])
ticksx <- c(xlims[1], 1, 2, 3, 4, 5, xlims[2])
xlabels <- c(xlims[1], 1, 2, 3, 4, 5, xlims[2])

#Plot command 
plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")),
     xaxt = "n", yaxt = "n", cex.lab = labelsize, bty = "n")

#Customize ticks and labels for the plot
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)


mtext(expression(paste("Alfredo's payoffs")), side=1, line = 2.5, cex = axislabelsize)
text(-0.6, 0.5*ylims[2], expression(paste("Bob's payoffs")), xpd = TRUE, cex = axislabelsize, srt = 90) 


#Lines for the coordinates of the Nash equilbrium
#segments(1, 4, 3, 3, lty = 1, col = "#7fc97f", lwd = 4)
#segments(3, 3, 4, 1, lty = 1, col = "#7fc97f", lwd = 4)

#P-efficiency polygons
x22 <- c(2, xlims[2], xlims[2], 2, 2)
y22 <- c(2, 2, ylims[2], ylims[2], 2)
polygon(x22, y22, col=rgb(1, 1, 0, 0.5), density=NULL, border = NA)

#P-efficiency polygons
x14 <- c(1, xlims[2], xlims[2], 1, 1)
y14 <- c(4, 4, ylims[2], ylims[2], 4)
polygon(x14, y14, col=rgb(1, 0, 0, 0.5), density=NULL, border = NA)

#P-efficiency polygons
x41 <- c(4, xlims[2], xlims[2], 4, 4)
y41 <- c(1, 1, ylims[2], ylims[2], 1)
polygon(x41, y41, col=rgb(0, 1, 0, 0.5), density=NULL, border = NA)

#P-efficiency polygons
x33 <- c(3, xlims[2], xlims[2], 3, 3)
y33 <- c(3, 3, ylims[2], ylims[2], 3)
polygon(x33, y33, col=rgb(0, 0, 1, 0.5), density=NULL, border = NA)



#Add points a, b, c and c
points(1, 4, pch = 16, col = "black", cex = pointsize)
text(0.85, 3.9, expression(paste(d)), cex = annotatesize)
points(3, 3, pch = 16, col = "black", cex = pointsize)
text(2.85, 2.9, expression(paste(c)), cex = annotatesize)
points(4, 1, pch = 16, col = "black", cex = pointsize)
text(3.85, 0.9, expression(paste(b)), cex = annotatesize)
points(2, 2, pch = 16, col = "black", cex = pointsize)
text(1.85, 1.9, expression(paste(a)), cex = annotatesize)
#text(1.5, 1.8, expression(paste("Nash equilibrium")))

#90 degree angle indicators -TEST
# segments(1, 4.1, 1.1, 4.1, lty = 1, col = "black", lwd = 1)
# segments(1.1, 4, 1.1, 4.1, lty = 1, col = "black", lwd = 1)
# 
# segments(3, 3.1, 3.1, 3.1, lty = 1, col = "black", lwd = 1)
# segments(3.1, 3, 3.1, 3.1, lty = 1, col = "black", lwd = 1)
# 
# segments(4, 1.1, 4.1, 1.1, lty = 1, col = "black", lwd = 1)
# segments(4.1, 1, 4.1, 1.1, lty = 1, col = "black", lwd = 1)
# 
# segments(2, 2.1, 2.1, 2.1, lty = 1, col = "black", lwd = 1)
# segments(2.1, 2, 2.1, 2.1, lty = 1, col = "black", lwd = 1)

#Label utility possibilities frontier
#text(3.9, 2.6, expression(paste("Utility Possibilities")))
#text(3.9, 2.35, expression(paste("Frontier (upf)")))


dev.off()

