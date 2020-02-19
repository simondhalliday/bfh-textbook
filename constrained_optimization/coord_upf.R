#Graph Designer: Scott Cohn
#Authors: Bowles and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics


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
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 7, 1, 1))

# Functions ---------------------------------------------------------------

uA <- function(ha, hb, alpha = 30, beta = 1/2) {
  (alpha - beta * (ha + hb)) * ha - 0.5 * (ha)^2
}

uB <- function(ha, hb, alpha = 30, beta = 1/2) {
  (alpha - beta * (ha + hb)) * hb - 0.5 * (hb)^2
}

brfB <- function(ha, alpha = 30, beta = 1/2) {
  (alpha - beta * ha) / (1 + 2 * beta)
}

brfA <- function(ha, alpha = 30, beta = 1/2) {
  (alpha - ha * (1 + 2 * beta)) / (beta)
}

# brfPE <- function(ha, alpha = 30, beta = 1/8) {
#   alpha*(1 - beta*ha)
# }
# 
# hANE <- function(alpha, beta = 1/2){
#   alpha/(1 + 3*beta)
# }
# 
hApEff2 <- function(alpha, beta = 1/2){
  alpha/(1 + 4*beta)
}
# 
# intercept1 <- function(alpha, beta = 1/2){
#   alpha/(1 + 2*beta)
# }

PEC <- function(ha, alpha = 30, beta = 1/2) {
  a = beta * (1 + 2 * beta)
  
  b = -alpha * (1 + 3 * beta) + (1 + 2 * beta)^2 * ha
  
  c = alpha^2 - alpha * (1 + 3 * beta) * ha + beta * (1 + 2 * beta) * ha^2
  
  delta = b ^ 2 - 4 * a * c
  
  return((-b - sqrt(delta)) / (2 * a))
}


# indiffA <- function(ha, alpha = 30, beta = 1/2, uA = 144) {
#   (alpha*ha - beta*(ha)^2 - 0.5*(ha)^2 - uA)/(beta*ha)
# }
# 
# indiffBroot1 <- function(ha, alpha = 30, beta = 30, uB = 144){
#   (-sqrt(alpha^2 - 2*alpha*beta*ha + beta^2*ha^2 - 4*beta*uB - 2*uB) + alpha - beta*ha)/(2*beta + 1)
# }
# 
# indiffBroot2 <- function(ha, alpha = 30, beta = 30, uB = 144){
#   (sqrt(alpha^2 - 2*alpha*beta*ha + beta^2*ha^2 - 4*beta*uB - 2*uB) + alpha - beta*ha)/(2*beta + 1)
# }
# 
# 
# indiffB <- function(hb, alpha = 30, beta = 1/2, uB = 144) {
#   (alpha*hb - uB - 0.5*hb^2)/(alpha*beta*hb)
# }
# 
# indiffB2 <- function(ha, alpha = 30, beta = 1/2, uB = 144) {
#   sqrt(alpha^2*(beta*ha - 1)^2 - 2*uB) - alpha*beta*ha + alpha
# }
# 
# indiffB3 <- function(ha, alpha = 30, beta = 1/2, uB = 144) {
#   -sqrt(alpha^2*(beta*ha - 1)^2 - 2*uB) - alpha*beta*ha + alpha
# }


# Axis Setup --------------------------------------------------------------

xlims <- c(0, 20)
ylims <- c(0, 20)

npts <- 501 
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

# a <- c(112, 
#        uA(hANE(alpha = 30, beta = 1/2), hANE(alpha = 30, beta = 1/2)), 
#        155.8)
# b <- c(112, 
#        uA(hANE(alpha = 30, beta = 1/2),hANE(alpha = 30, beta = 1/2)), 
#        155.8)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs = "i", 
     yaxs = "i")

ticksy <- c(0, ylims[2])
ylabels <- c(NA, NA)
ticksx <- c(0, xlims[2])
xlabels <- c(NA, NA)

axis(1, at = ticksx,  pos = 0, labels = xlabels, las = 1, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)

text(0.5*xlims[2], -3, expression(paste("A's hours, ", h^A)), xpd = TRUE, cex = axislabelsize)
text(-4.5, 9, expression(paste("B's hours, ", h^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 


# Calculations ------------------------------------------------------------

# Create `ha` and `hb`

ha <- seq(xlims[1], 0.75 * xlims[2], length.out = npts) # TODO figure out how to define `ha`
hb <- PEC(ha)

# Plug `ha` and `hb` into `uA` and `uB`
uA_vec <- uA(ha, hb)  # x-coord
uB_vec <- uB(ha, hb)  # y-coord

# plot(ha, hb)
# plot(uA_vec, uB_vec)

# Lines -------------------------------------------------------------------

# lines(uA_vec, uB_vec, col = COLA[4], lty = 1, lwd = graphlinewidth)

# Labels ------------------------------------------------------------------



dev.off()