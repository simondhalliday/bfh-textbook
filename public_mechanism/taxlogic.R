library(shape)
library(diagram)
pdf(file = "public_mechanism/taxlogic.pdf", width = 7, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5
rx <- 0.15
ry <- 0.15
framewidth <- 1.2
textsize <- 1.5
symbolsize <- 5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar = c(0, 0, 0, 0))
openplotmat()
rx <- 0.07
ry <- 0.07
circlex <- 0.1
circley <- 0.1
pos <- coordinates(c(3, 3, 3))

#Arrows
#From 4 to 3
straightarrow(from = pos[4, ] + c(0.04,0), 
              to = pos[3, ] + c(-0.09,0), 
              lty = 1, lcol = 1)
#From 4 to 8
straightarrow(from = pos[4, ], to = pos[8, ], lty = 1, lcol = 1)

#From 8 to 3
straightarrow(from = pos[8,], to = pos[3, ], lty = 1, lcol = 1)

#From 11 to 7
straightarrow(from = pos[11,] + c(0.07,0), to = pos[7, ] + c(0, 0.02), 
              lty = 1, lcol = 1, arr.pos = 0.6)

#From 9 to 10
straightarrow(from = pos[9,], to = pos[10, ], 
              lty = 1, lcol = 1, arr.pos = 0.55)

#From 10 to 11
straightarrow(from = pos[10,], to = pos[11, ], lty = 1, lcol = 1, arr.pos = 0.5)


#From 3 to 12
straightarrow(from = pos[3,]+c(0.04,0), to = pos[12, ], lty = 1, lcol = 1, arr.pos = 0.5)


#First row

textround(mid = pos[3,], 
          radx = rx, 
          rady = ry,
          lab = c("Your", expression(paste("smoking, ", x^A))),  
          lwd = framewidth, 
          cex = textsize, 
          lcol = "#238b45",
          nr = 3, 
          shadow.col = FALSE)

#Second row
textround(mid = pos[4,], radx = rx, rady = ry, 
          lab = c("Tax on", "smoking"),
          lwd = framewidth, lcol = "#99000d", 
          cex = textsize, shadow.col = FALSE)

# Third Row
textround(mid = pos[8,], radx = rx, rady = ry, 
          lab = c("Others'", expression(paste("smoking, ", x^B))), 
          lwd = framewidth, lcol = "#084594", 
          cex = textsize, 
          shadow.col = FALSE)

textellipse(mid = pos[6,]-0.08, 
            radx = 0.04, 
            rady = 0.04, 
            lab = c(expression("+")), 
            lwd = framewidth, 
            lcol =  FALSE, 
            cex = symbolsize, 
            shadow.col = FALSE)

textellipse(mid = pos[2,] - 0.1, 
            radx = 0.04, 
            rady = 0.04, 
            lab = c(expression(paste(phantom()-phantom()))), 
            lwd = framewidth, 
            lcol =  FALSE, 
            cex = symbolsize, 
            shadow.col = FALSE)


textellipse(mid = pos[7,] + 0.1, 
            radx = 0.04, 
            rady = 0.04, 
            lab = c(expression(paste(phantom()-phantom()))), 
            lwd = framewidth, 
            lcol =  FALSE, 
            cex = symbolsize, 
            shadow.col = FALSE)

dev.off()
