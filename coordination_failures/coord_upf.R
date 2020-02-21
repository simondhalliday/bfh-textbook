#Graph Designer: Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

####################################
# Version with Feas Set Full Shade
####################################

# Graph Initialize --------------------------------------------------------

require(shape)
pdf(file = "coordination_failures/coord_upf.pdf", width = 9, height = 7)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 7, 1, 1))

# Functions ---------------------------------------------------------------

uA <- function(ha, hb, alpha = 30, beta = 1/2) {
  (alpha - beta * (ha + hb)) * ha - 0.5 * (ha)^2
}

uB <- function(ha, hb, alpha = 30, beta = 1/2) {
  (alpha - beta * (ha + hb)) * hb - 0.5 * (hb)^2
}

PEC <- function(ha, alpha = 30, beta = 1/2) {
  a = beta * (1 + 2 * beta)
  
  b = -alpha * (1 + 3 * beta) + (1 + 2 * beta)^2 * ha
  
  c = alpha^2 - alpha * (1 + 3 * beta) * ha + beta * (1 + 2 * beta) * ha^2
  
  delta = b ^ 2 - 4 * a * c
  
  return((-b - sqrt(delta)) / (2 * a))
}

# Axis Setup --------------------------------------------------------------

xlims <- c(0, 300)
ylims <- c(0, 300)

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
     xaxs = "i", 
     yaxs = "i")

ticksy <- c(0, 112, 150, ylims[2])
ylabels <- c(NA, 112, 150, NA)
ticksx <- c(0, 112, 150, xlims[2])
xlabels <- c(NA, 112, 150, NA)

axis(1, at = ticksx,  pos = 0, labels = xlabels, las = 1, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

text(0.5*xlims[2], -40, expression(paste("A's utility, ", u^A)), xpd = TRUE, cex = axislabelsize)
text(-40, 0.5*ylims[2], expression(paste("B's utility, ", u^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 


# Calculations ------------------------------------------------------------

# Create `ha` and `hb`

ha <- seq(xlims[1], 15, length.out = npts) 
hb <- PEC(ha)

# Plug `ha` and `hb` into `uA` and `uB`
uA_vec <- uA(ha, hb)  # x-coord
uB_vec <- uB(ha, hb)  # y-coord

# W-hat

W = c(250, 300, 350)

# Shading -----------------------------------------------------------------

polygon(uA_vec, 
        uB_vec,
        border = FALSE, col = COLA[1])

# Comment this section out to shade above z
polygon(c(0, 0, max(uA_vec)),
        c(0, max(uB_vec), 0),
        border = FALSE, col = COLA[1])

# Segments ----------------------------------------------------------------

# to i
segments(0, 150, 150, 150, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(150, 0, 150, 150, lty = 2, col = "gray" , lwd = segmentlinewidth)

# to z
segments(0, 112, 112, 112, lty = 2, col = "gray" , lwd = segmentlinewidth)
segments(112, 0, 112, 112, lty = 2, col = "gray" , lwd = segmentlinewidth)


# Lines -------------------------------------------------------------------

# extend uA to eliminate scope error
extend <- seq(226, xlims[2], length.out = npts)
uA_vec_extend <- append(uA_vec, extend)

lines(uA_vec,
      uB_vec,
      col = COLA[4],
      lty = 1,
      lwd = graphlinewidth
)

# Iso-Soc Wel -> uB = W - uA
lines(
  uA_vec_extend,
  W[1] - uA_vec_extend,
  col = COLB[4],
  lty = 1,
  lwd = graphlinewidth
)

lines(
  uA_vec_extend,
  W[2] - uA_vec_extend,
  col = COLB[4],
  lty = 1,
  lwd = graphlinewidth
)

lines(
  uA_vec_extend,
  W[3] - uA_vec_extend,
  col = COLB[4],
  lty = 1,
  lwd = graphlinewidth
)

# Labels ------------------------------------------------------------------

# Label i 
points(150, 150, pch = 16, col = "black", cex = 1.5)
text(155, 155, expression(i), cex = labelsize)

# label z
points(112, 112, pch = 16, col = "black", cex = 1.5)
text(105, 105, expression(z), cex = labelsize)

# label tA and tB
points(112, uB_vec[216], pch = 16, col = "black", cex = 1.5)
text(108, uB_vec[216] - 10, expression(t^A), cex = labelsize)

points(uB_vec[216], 112, pch = 16, col = "black", cex = 1.5)
text(uB_vec[216] - 5, 103, expression(t^B), cex = labelsize)



dev.off()

