# Author: Weikai Chen & ...
## the competition condition
w <- function(b, gamma, rho){
    (1-b)*gamma / (1+rho)
}

## wage-setting curve

h <- function(u0,w,t,B) 1 - u0/((w-B)*t)

H <- function(h,P,E) (P-E)*h

n <- function(H,P)   H/P
u <- function(H,E,P) (P - E - H) / P
sigma <- function(b, rho) (1-b) / (1+rho)
lambdau <-function(u,P,B,gamma,H) u*P*B/(u*P*B + gamma*H)
lambdan <-function(u,P,B,gamma,H,sigma) sigma * gamma * H / (u*P*B + gamma*H)
G <-function(n,u,lambdau,lambdan) n + u - (1-u)*lambdan -(1+n)*lambdau
X <- function(gamma, H) gamma * H
p <- function(beta,pbar,X) pbar - beta * X
N <- function(X, beta, pbar)  beta * X / (pbar - beta * X)

## define the function to calculate the result given parameters
## e.g., outcome() will give you the result of case 1 with the default paramters
## outcome(b=.3)
outcome <- function(B = 0.2,t = 0.8,b = 0.14, gamma = 1, P = 160, E = 20,
                    rho = 0.05, u0 = 0.05, pbar = 26, beta = 0.2){
    w = w(b,gamma,rho)
    h = h(u0,w,t,B)
    H = H(h,P,E)
    n = n(H,P)
    u = u(H,E,P)
    sigma = sigma(b,rho)
    lambdau = lambdau(u,P,B,gamma,H)
    lambdan = lambdan(u,P,B,gamma,H,sigma)
    G = G(n,u,lambdau,lambdan)
    X = X(gamma,H)
    p = p(beta,pbar,X)
    N = N(X,beta, pbar)
    outcome = list(w = w,h = h, H = H, sigma=sigma,G=G, X = X, p = p, N = N,n=n,u=u,lambdan = lambdan, lambdau = lambdau )
    return(outcome)
}
## define a function: Gini as a function of B
gini<-function(B){
    return(outcome(B=B)$G)
}
B<-seq(0,0.7,0.01)
plot(B,gini(B),type='l',ylab='Gini')

## plot the lorenz given the result
## e.g., lorenz(outcome(b=.5))
lorenz<-function(result){
  library(shape)
  library(pBrackets)
  #Set parameters for graphics
  axislabelsize <- 1.5
  labelsize <- 1.2
  graphlinewidth <- 3
  segmentlinewidth <- 2
  COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
  COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
  COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
  COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")
  #Edited the margins to cater for the larger LHS labels
  par(mar =  c(8, 6, 2, 6))
  #Add limits on axes
  ylims <- c(0, 1)
  xlims <- c(0, 1)
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
       yaxs="i"
  )
  ticksy <- c(ylims[1], result$sigma, ylims[2])
  ylabels <- c(0, round(100*result$sigma,1), 100)
  ticksx <- c(xlims[1], result$u, result$u+result$n, xlims[2])
  xlabels <- c(NA,  round(100*result$u,1), round(100*(result$u+result$n),1),  100)
  ticksy2 <- c(0,1)
  axis(1,at = ticksx,  pos = 0, labels = xlabels)
  axis(2,at = ticksy,  pos = 0, labels = ylabels, las = 1)
  axis(4,at = ticksy2,  pos = 1, labels = NA, las = 1)
  npts <- 500
  xx1 <- seq(xlims[1], xlims[2], length.out = npts)
  xx2 <- seq(0, xlims[2], length.out = npts)
  xx3 <- seq(xlims[1], 0, length.out = npts)
  xx4 <- seq(-11, 0, length.out = npts)
  text(-0.18, 0.5*ylims[2], expression(paste("Cumulative income, (%)")), xpd = TRUE, cex = axislabelsize, srt = 90)
  text(0.5*xlims[2], -0.22, expression(paste("Cumulative population, (%)")), xpd = TRUE, cex = axislabelsize)

  #Shaded Areas A and B
  #Area A
  xpoly1 <- c(0, result$u, result$n+result$u, 1, 0)
  ypoly1 <- c(0, 0, result$sigma, 1, 0)
  polygon(x = xpoly1,
          y = ypoly1,
          col = COLB[1], density = NULL, border = NA)
  #Area B
  xpoly2 <- c(0, result$u, 1, 1, result$n+result$u, result$u)
  ypoly2 <- c(0, 0, 0, 1, result$sigma, 0)
  polygon(x = xpoly2,
          y = ypoly2,
          col = COLA[1], density = NULL, border = NA)
  # Lorenz curve
  segments(0, 0, result$u, 0, lty = 1, col = COLA[5], lwd = graphlinewidth)
  segments(result$u, 0, result$u+result$n, result$sigma, lty = 1, col = COLA[5], lwd = graphlinewidth)
  segments(result$u+result$n, result$sigma, 1, 1, lty = 1, col = COLA[5], lwd = graphlinewidth)
  text(result$n+result$u - 0.1, result$sigma - 0.2, expression(paste("Lorenz")), cex = labelsize)
  text(result$n+result$u-0.1, result$sigma-0.23, expression(paste("curve")), cex = labelsize)
  #Line of equality
  segments(0, 0, 1, 1, lty = 1, col = COLB[5], lwd = graphlinewidth)
  text(0.8, 0.9, expression(paste("Line of")), cex = labelsize)
  text(0.8, 0.86, expression(paste("equality")), cex = labelsize)

  segments(result$u+result$n, 0, result$n+result$u, result$sigma, lty = 2, col = "gray", lwd = segmentlinewidth)
  segments(0, result$sigma, result$u+result$n, result$sigma, lty = 2, col = "gray", lwd = segmentlinewidth)


  #Label areas A and B
  text(0.3, 0.25, expression(paste(A)), cex = labelsize)

  # text(0.6, 0.2, expression(paste(B[1])), cex = labelsize)
  text((result$u+result$n)*1.05,result$sigma*0.95, expression(paste(B)), cex = labelsize)
  # text(0.9, 0.7, expression(paste(B[3])), cex = labelsize)

  #Label and provide Gini value
  #Bottom of frame
  #segments(0.08, 0.75, 0.32, 0.75, lty = 1, col = "black" , lwd = 1)
  #Top of frame
  #segments(0.08, 0.85, 0.32, 0.85, lty = 1, col = "black" , lwd = 1)
  #Left of frame
  #segments(0.08, 0.75, 0.08, 0.85, lty = 1, col = "black" , lwd = 1)
  #Right of frame
  #segments(0.32, 0.75, 0.32, 0.85, lty = 1, col = "black" , lwd = 1)
  #Gini equation
  G=round(result$G,2)
  #text(0.2, result$sigma+0.15, expression(paste(Gini == frac(A, A + B) , phantom() == 0.08)), cex = labelsize)
  text(0.2, result$sigma+0.15, bquote(Gini  ==.(G)), cex = labelsize)

  brackets(x1 = result$u*.99, y1 = -0.08, x2 = 0, y2 = -0.08,
           ticks = 0.5, curvature = 0.5, type = 1,
           col = "black", lwd = 2, lty = 1, xpd = TRUE)
  text(result$u*.99*.5, -0.14, expression(paste("unemployed")), xpd = TRUE)

  brackets(x1 = result$u + result$n, y1 = -0.08, x2 = result$u, y2 = -0.08,
           ticks = 0.5, curvature = 0.5, type = 1,
           col = "black", lwd = 2, lty = 1, xpd = TRUE)
  text(result$u+result$n*0.5, -0.14, expression(paste("employed")), xpd = TRUE)

  brackets(x1 = 1, y1 = -0.08, x2 = result$u+result$n, y2 = -0.08,
           ticks = 0.5, curvature = 0.5, type = 1,
           col = "black", lwd = 2, lty = 1, xpd = TRUE)
  text(.5*(1+result$u+result$n), -0.14, expression(paste("owners")), xpd = TRUE)
}


pdf(file = "capitalism/market_lorenz_1.pdf", width = 10, height = 8)
lorenz(outcome())
dev.off()
pdf(file = "capitalism/market_lorenz_2.pdf", width = 10, height = 8)
lorenz(outcome(b=.3))
dev.off()
pdf(file = "capitalism/market_lorenz_3.pdf", width = 10, height = 8)
lorenz(outcome(b=.5))
dev.off()

# the labor market plot
#require(plotrix)
# axislabelsize <- 1.5
# labelsize <- 1.2
# graphlinewidth <- 3
# segmentlinewidth <- 2
# par(mar =  c(5, 5.5, 0, 0))
# xlims <- c(0, 1.2)
# ylims <- c(0, 1.2)
#
#
# plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
#      xlab = expression(paste("Total hours of employment as a proportion, ", h)),
#      ylab = expression(paste("Real wage, ", w)),
#      xaxt = "n",
#      yaxt = "n",
#      cex.lab = axislabelsize,
#      line = 2.5,
#      bty = "n",
#      xaxs="i",
#      yaxs="i")
#
# npts <- 500
# npts2 <- 501
# #Specify the sequences of points for graphing.
# xx1 <- seq(xlims[1], 0.9, length.out = npts)
# xx2 <- seq(xlims[1], xlims[2], length.out = npts)
# xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
# xx4 <- seq(xlims[1], 25, length.out = npts2)
#
# #Draw the lines for the graphs
# WageFn <- function(w){h(u0=0.05,t =0.8,w)}
# lines( WageFn(xx1),xx1, col = COL[1], lwd = 4)
# #lines(xx2, solowCondition(xx2, delta = 5), col = COL[3], lwd = 4)
# #lines(xx2, solowInfeas(xx2, delta = 5), col = COL[1], lwd = 4, lty = 2)
#
# #Customize ticks and labels for the plot
# ticksy <- c(0, outcome()$w, outcome(b=0.5)$w, 1)
# ylabels <- c(0, round(outcome()$w,2), round(outcome(b=0.5)$w,2), NA)
# ticksx <- c(0, outcome(b=0.5)$h,outcome()$h, 1)
# xlabels <- c(0, round(outcome(b=0.5)$h,2), round(outcome()$h,2), 1.0)
# axis(1, at = ticksx, pos = 0, labels = xlabels)
# axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)
#
# #Annotation of the  graphs
# text(0.5, 0.25, expression(paste("Wage Curve ", w(h))))
#
# #segments(1, 0, 1, 42, lty = 2, lwd = 3, col = "darkgray")
# #segments(, 0, 0.75, 20, lty = 2, lwd = 2, col = "darkgray")
#
# #Arrows(0.8, 15, 0.8, 19, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
# #Arrows(0.8, 15, 0.8, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
# #text(0.92, 12.5, expression(paste("Employment Rent")))
#
# #Zero profit condition
# segments(0, outcome()$w, outcome()$h, outcome()$w, lty = 1, lwd = graphlinewidth, col = COLB[3])
# segments(0, outcome(b=0.5)$w, outcome(b=0.5)$h, outcome(b=0.5)$w, lty = 1, lwd = graphlinewidth, col = COLB[3])
# #segments(0.75, 20, 1.2, 20, lty = 2, lwd = segmentlinewidth, col = COLB[3])
#
# points(outcome()$h, outcome()$w, pch = 16, col = "black", cex = 1.5)
# points(outcome(b=0.5)$h, outcome(b=0.5)$w, pch = 16, col = "black", cex = 1.5)
# #text(0.74, 21, expression(paste("n")))
#
#
# segments(outcome()$h, 0, outcome()$h, outcome()$w, lty = 2, lwd = 2, col = "darkgray")
# segments(outcome(b=0.5)$h, 0, outcome(b=0.5)$h, outcome(b=0.5)$w, lty = 2, lwd = 2, col = "darkgray")
# #segments(0, 2.5, 1.2, 2.5, lty = 2, lwd = 2, col = "darkgray")
#
# #Zero profit condition
# text(0.5, outcome()$w+0.05, expression(paste("Zero profit condition, ", w == w[0])))
# text(0.5, outcome(b=0.5)$w+0.05, expression(paste("Zero profit condition, ", w == w[1])))
# #text(0.97, 6, expression(paste(B + a/t)))
# #text(0.97, 3.5, expression(paste(B, " (unemployment benefits)")))
