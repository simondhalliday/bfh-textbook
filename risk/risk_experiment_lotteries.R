library(shape)
library(diagram)
pdf(file = "risk/risk_experiment_lotteries.pdf", width = 7, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5
framewidth <- 2
textsize <- 1.5
symbolsize <- 5

# COLS <- c("#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#990000") # reds
COLS <- c("#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#084594") # blues
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)

par(mar = c(0, 0, 0, 0))
openplotmat()
rx <- 0.13
ry <- 0.13
circlex <- 0.1
circley <- 0.1
pos <- coordinates(c(4, 4, 4))

#Arrows
#From 4 to 3
# straightarrow(from = pos[4, ] + c(0.04,0), 
#               to = pos[3, ] + c(-0.09,0), 
#               lty = 1, lcol = 1)
#From 4 to 8
#straightarrow(from = pos[4, ], to = pos[8, ], lty = 1, lcol = 1)

#From 8 to 3
#straightarrow(from = pos[8,], to = pos[3, ], lty = 1, lcol = 1)

#From 11 to 7
#straightarrow(from = pos[11,] + c(0.07,0), to = pos[7, ] + c(0, 0.02), 
#              lty = 1, lcol = 1, arr.pos = 0.6)

#From 9 to 10
# straightarrow(from = pos[9,], to = pos[10, ], 
#               lty = 1, lcol = 1, arr.pos = 0.55)

#From 10 to 11
#straightarrow(from = pos[10,], to = pos[11, ], lty = 1, lcol = 1, arr.pos = 0.5)


#From 3 to 12
#straightarrow(from = pos[3,]+c(0.04,0), to = pos[12, ], lty = 1, lcol = 1, arr.pos = 0.5)


#First row
textellipse(mid = pos[2,] + c(-0.025, 0), 
          radx = rx, 
          rady = ry,
          lab = c("-$2   $54"),
          lwd = framewidth, 
          cex = textsize, 
          lcol = COLS[6],
          shadow.col = FALSE)

textellipse(mid = pos[2,] + c(-0.22, 0), 
            radx = 0.05, 
            rady = 0.05, 
            lab = c("L6"),
            lwd = framewidth, 
            lcol = grays[20],
            cex = textsize, 
            shadow.col = FALSE)

#Midline 
textrect(mid = pos[2,] + c(-0.025, 0), 
            radx = 0.0001, 
            rady = ry,
            lab = c(""),  
            lwd = framewidth, 
            cex = textsize, 
            lcol = COLS[6],
            shadow.col = FALSE)

#Lottery 1
textellipse(mid = pos[3,] + c(0.025, 0), 
          radx = rx, 
          rady = ry, 
          lab = c("$18   $18"),
          lwd = framewidth, 
          lcol = COLS[1],
          cex = textsize, shadow.col = FALSE)

textellipse(mid = pos[3,] + c(0.22, 0), 
            radx = 0.05, 
            rady = 0.05, 
            lab = c("L1"),
            lwd = framewidth, 
            lcol = grays[20],
            cex = textsize, shadow.col = FALSE)

#Midline
textrect(mid = pos[3,] + c(0.025, 0), 
         radx = 0.0001, 
         rady = ry,
         lab = c(""),  
         lwd = framewidth, 
         cex = textsize, 
         lcol = COLS[1],
         shadow.col = FALSE)


# Second Row
##Lottery 5
textellipse(mid = pos[5,] + c(0.125, 0),  
            radx = rx, rady = ry, 
          lab = c("$2   $50"),
          lwd = framewidth, 
          lcol = COLS[5],
          cex = textsize, 
          shadow.col = FALSE)

#Midline
textrect(mid = pos[5,] + c(0.125, 0), 
         radx = 0.0001, 
         rady = ry,
         lab = c(""),  
         lwd = framewidth, 
         cex = textsize, 
         lcol = COLS[5],
         shadow.col = FALSE)

textellipse(mid = pos[5,] + c(-0.07, 0), 
            radx = 0.05, 
            rady = 0.05, 
            lab = c("L5"),
            lwd = framewidth, 
            lcol = grays[20],
            cex = textsize, shadow.col = FALSE)

##Lottery 2
textellipse(mid = pos[8,] + c(-0.125, 0),  
            radx = rx, rady = ry, 
          lab = c("$14   $26"),
          lwd = framewidth, 
          lcol = COLS[2],
          cex = textsize, 
          shadow.col = FALSE)

#Midline
textrect(mid = pos[8,] + c(-0.125, 0), 
         radx = 0.0001, 
         rady = ry,
         lab = c(""),  
         lwd = framewidth, 
         cex = textsize, 
         lcol = COLS[2],
         shadow.col = FALSE)

textellipse(mid = pos[8,] + c(0.07, 0), 
            radx = 0.05, 
            rady = 0.05, 
            lab = c("L2"),
            lwd = framewidth, 
            lcol = grays[20],
            cex = textsize, shadow.col = FALSE)


#Third Row
##Lottery 4
textellipse(mid = pos[10,] + c(-0.025, 0),  
          radx = rx, 
          rady = ry,
          lab = c("$6   $42"),
          lwd = framewidth, 
          cex = textsize, 
          lcol = COLS[4],
          shadow.col = FALSE)

textellipse(mid = pos[10,] + c(-0.22, 0), 
            radx = 0.05, 
            rady = 0.05, 
            lab = c("L4"),
            lwd = framewidth, 
            lcol = grays[20],
            cex = textsize, shadow.col = FALSE)


#Midline
textrect(mid = pos[10,] + c(-0.025, 0), 
         radx = 0.0001, 
         rady = ry,
         lab = c(""),  
         lwd = framewidth, 
         cex = textsize, 
         lcol = COLS[4],
         shadow.col = FALSE)

#Lottery 3
textellipse(mid = pos[11,] + c(0.025, 0),  
          radx = rx, rady = ry, 
          lab = c("$10   $34"),
          lwd = framewidth, 
          lcol = COLS[3],
          cex = textsize, shadow.col = FALSE)

textellipse(mid = pos[11,] + c(0.22, 0), 
            radx = 0.05, 
            rady = 0.05, 
            lab = c("L3"),
            lwd = framewidth, 
            lcol = grays[20],
            cex = textsize, shadow.col = FALSE)


#Midline
textrect(mid = pos[11,] + c(0.025, 0), 
         radx = 0.0001, 
         rady = ry,
         lab = c(""),  
         lwd = framewidth, 
         cex = textsize, 
         lcol = COLS[3],
         shadow.col = FALSE)


dev.off()
