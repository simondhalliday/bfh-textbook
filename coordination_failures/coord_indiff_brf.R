  require(shape)
  pdf(file = "coordination_failures/coord_indiff_brf/coord_indiff_brf.pdf", width = 9, height = 7)
  
  #Set parameters for graphics
  axislabelsize <- 1.5
  graphlinewidth <- 2
  segmentlinewidth <- 1.5
  
  COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
  COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
  COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")
  Grays <- gray.colors(25, start =1, end = 0)
  
  par(mar =  c(5, 5, 4, 4))
  
  # uA <- function(ea, eb, alpha = 16, beta = 1/24) {
  #   alpha*(1 - beta*eb)*ea - 0.5*(ea)^2
  # }
  
  uA <- function(ea, eb, alpha = 30, beta = 1/2) {
    (alpha - beta*(ea+eb))*ea - 0.5*(ea)^2
  }
  
  hANE <- function(alpha, beta = 1/2){
    alpha/(1 + 3*beta)
  }
  
  brfA <- function(ea, alpha = 30, beta = 1/2) {
    (alpha - ea*(1 + 2*beta))/(beta)
  }
  
  # brfA <- function(ea, alpha = 16, beta = 1/24) {
  #   (alpha - ea)/(alpha * beta)
  # }
  
  
  xlims <- c(0, 20)
  ylims <- c(0, 20)
  
  npts <- 501 
  x <- seq(xlims[1], xlims[2], length.out = npts)
  y <- seq(ylims[1], ylims[2], length.out = npts) 
  a <- c(uA(10.8, brfA(10.8)), 
         uA(hANE(alpha = 30, beta = 1/2), hANE(alpha = 30, beta = 1/2)), 
         uA(13.2, brfA(13.2))
  )
  
  plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
       xlab = expression(paste("")),
       ylab = expression(paste("")), 
       xaxt = "n", 
       yaxt = "n", 
       cex.lab = axislabelsize, 
       bty = "n",
       xaxs="i", 
       yaxs="i")
  
  # ticksy <- seq(from = 0, to = ylims[2], by = 2)
  # ylabels <- seq(from = 0, to = ylims[2], by = 2)
  # ticksx <- seq(from = 0, to = xlims[2], by = 2)
  # xlabels <- seq(from = 0, to = xlims[2], by = 2)
  ticksy <- c(0,  brfA(13.2), 12,  brfA(10.8), ylims[2])
  ylabels <- c(NA, expression(paste(h[Low]^B)), expression(paste(h^{BN})), expression(paste(h[High]^B)), NA)
  ticksx <- c(0, 12,  xlims[2])
  xlabels <- c(NA, expression(paste(h^{AN})), NA)
  
  
  axis(1, at = ticksx, pos = 0, labels = xlabels)
  axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)
  
  npts <- 500 
  xx1 <- seq(xlims[1], xlims[2], length.out = npts)
  lines(xx1, brfA(xx1, alpha = 30, beta = 1/2), col = COLA[4], lwd = graphlinewidth)
  
  
  contour(y, x, 
          outer(x, y, uA),
          drawlabels = FALSE,
          col = COLA[3],
          lwd = graphlinewidth,
          levels = a, 
          xaxs="i", 
          yaxs="i", 
          add = TRUE) 
  
  text(0.5*xlims[2], -2.1, expression(paste("A's hours, ", h^A)), xpd = TRUE, cex = axislabelsize)
  text(-1.9, 9, expression(paste("B's hours,", h^B)), xpd = TRUE, cex = axislabelsize, srt = 90) 
  
  #Label the iso-welfare functions for the A
  text(4.4, 1.5, expression(u[1]^A))
  text(5.9, 1.5, expression(u[2]^A))
  text(7.9, 1.5, expression(u[3]^A))
  
  #Label Nash Equilibrium 
  segments(0, 12, 14.6, 12, lty = 2, col = Grays[18], lwd = segmentlinewidth)
  segments(12, 0, 12, 12, lty = 2, col = Grays[18], lwd = segmentlinewidth)
  points(12, 12, pch = 16, col = "black", cex = 1.5)
  #text(11.3, 10.1, expression(paste("Nash Equilibrium")))
  text(12 - 0.3, 12 - 0.4, expression(paste("n")))
  
  
  
  segments(0,  brfA(13.2),  13.2 + 2,  brfA(13.2), lty = 2, col = Grays[18], lwd = segmentlinewidth)
  segments(13.2,  brfA(13.2) - 2,  13.2,  brfA(13.2), lty = 2, col = Grays[18], lwd = segmentlinewidth)
  points(13.2, brfA(ea = 13.2), pch = 16, col = "black", cex = 1.5)
  text(13.2 - 0.3, brfA(ea = 13.2) - 0.4, expression(paste("j")))
  
  segments(0, brfA(10.8), 10.8 + 2, brfA(10.8), lty = 2, col = Grays[18], lwd = segmentlinewidth)
  segments(10.8, brfA(10.8) - 2, 10.8, brfA(10.8), lty = 2, col = Grays[18], lwd = segmentlinewidth)
  points(10.8, brfA(10.8), pch = 16, col = "black", cex = 1.5)
  text(10.8 - 0.3, brfA(10.8) - 0.4, expression(paste("k")))
  
  text(15, 19.5, expression(paste("A's best response")), xpd = TRUE)
  text(15, 18.5, expression(paste("function")), xpd = TRUE)
  text(15, 17.2, expression(paste(h^{A}*(h^B) == frac(alpha - beta*h^B, 1 + 2*beta))), xpd = TRUE)
  Arrows(14, 18.5, 10.8, 18.5, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)
  
  
  
  dev.off()
