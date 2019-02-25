require(shape)
pdf(file = "firmmarketsupply/problem_tech_choices.pdf", width = 7, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(5, 5, 4, 4))

isocost <- function(l, w = 15, ck = 10, c = 12000) {
  c/ck - (w / ck)* l
}

cost <- function(l, k, w = 15, ck = 10){
  w * l + ck * k
}


#Levels for the barriers to entry specified here
barriers <- c(0.2, 0.5, 0.71)

xlims <- c(0, 800)
ylims <- c(0, 1000)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

#Customize ticks and labels for the plot
ticksy <- seq(ylims[1], ylims[2], 100)
ylabels <- seq(ylims[1], ylims[2], 100)
ticksx <- seq(xlims[1], xlims[2], 80)
xlabels <- seq(xlims[1], xlims[2], 80)
# ticksy <- c(0, 10, cournotPrice(n = nstar(b = barriers[1]), c = costs[2]),  ylims[2])
# ylabels <- c(NA, expression(c), expression(paste(p,"(n*)")), NA)
# ticksx <- c(0, nstar(b = barriers[1]), xlims[2])
# xlabels <- c(NA, expression(paste(n,"*")), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)



#Label axes
mtext(expression(paste("Hours of engineer time, ", L)), side=1, line = 2.5, cex = axislabelsize)
text(-100, 0.5*ylims[2], expression(paste("Hours of machine time, ", K )), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Vectors for points
xtech <- c(90, 240, 240, 450, 720)
ytech <- c(900, 700, 500, 350, 350)


#Lines for isocosts for solution
# lines(xx1, 
#       isocost(l = xx1, 
#               c = cost(l = xtech[3], k = ytech[3])), 
#       col = COLA[3], 
#       lty = 2, 
#       lwd = segmentlinewidth)
# 
# lines(xx1, 
#       isocost(l = xx1, 
#               c = cost(l = xtech[4], k = ytech[4], w = 6), 
#               w = 6), 
#       col = COLA[3], 
#       lty = 2, 
#       lwd = segmentlinewidth)




#Apply tech vector to points function
points(xtech,  ytech,
       pch = 16, col = "black", cex = 1.5
       )

#Vectors for labels
xadj <- rep(25, 5)
yadj <- rep(10, 5)
techlab <- c("A", "B", "C", "D", "E")

#Apply labels to technologies
text(x = xtech + xadj,
     y = ytech + yadj,
     techlab, 
     cex = labelsize)

#Add new technology point for rent question
# i <- c(210, 280)
# 
# points(i[1], i[2],
#        pch = 16, col = "black", cex = 1.5
#        )
# 
# lines(xx1,
#       isocost(l = xx1,
#               c = cost(l = i[1], k = i[2], w = 15),
#               w = 15),
#       col = COLA[3],
#       lty = 2,
#       lwd = segmentlinewidth)
# lines(xx1,
#       isocost(l = xx1,
#               c = cost(l = i[1], k = i[2], w = 6),
#               w = 6),
#       col = COLA[3],
#       lty = 2,
#       lwd = segmentlinewidth)


dev.off()