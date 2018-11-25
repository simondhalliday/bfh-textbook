require(shape, rootSolve)
pdf(file = "employment/monopsony_minwage1.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(4, 6, 1, 1))


Delta <- function(n,v=0.05){
  1-v*n / (1-n)
}

ACL <- function(n, a = 1, tau = 0.5, v = 0.05, b = 2, s = 0) {
  a / ( tau * Delta(n,v) ) + b - s
}


MCL <- function(n,a=1,tau = 0.5 , v = 0.05, b = 2, s= 0) {
  d_Delta <- function(n,v= 0.05){
    -v/(1-n)^2
  }
  d_ACL <- function(n,a=1,tau = 0.5 , v = 0.05, b = 2, s= 0){
    - a*d_Delta(n,v) / (tau * Delta(n,v)^2)
  }
  ACL(n,a,tau,v,b,s) + n * d_ACL(n, a,tau, v, b, s)
}

MRP<-function(n){
  4/n
}

employment <- function(a=1,tau = 0.5 , v = 0.05, b = 2, s = 0, min = FALSE){
  ## return the employment level
  ## The four cases: 
  ## No EITC, no min wage:  employment()
  ## No EITC, min = 4.6: employment(min =  4.6)
  ## EITC, no min wage: employment(s=1)
  ## EITC, min = 4.6: employment(min = 4.6, s = 1)
  nmax <- 1/(1+v)-0.001
  if (min == FALSE){
    n <- uniroot(MRP_eq_MCL(n,a,tau,v,b,s),c(0.0001,nmax))$root
  } else{
    n1 <- uniroot(MRP_eq_min(n, min),c(0.0001,nmax))$root
    n2 <- uniroot(ACL_eq_min(n, a, tau, v, b, s, min),c(0.0001,nmax))$root
    n <- min(n1,n2)
  }
  return(n)
}

MRP_eq_MCL <- function(n,a=1,tau = 0.5 , v = 0.05, b = 2, s = 0){
  equation <- function(n){
    MRP(n) - MCL(n,a,tau,v, b, s)
  }
}
ACL_eq_min <- function(n, a = 1, tau = 0.5, v = 0.05, b = 2, s = 0, min = 4.6){
  equation<-function(n){
    ACL(n,a,tau,v,b,s)-min
  }
}
MRP_eq_min <- function(n,min=4.6){
  equation <- function(n){
    MRP(n) - min
  }
}
Min <- function(n, min = 4.6){
  min*n/n
}

ylims <- c(0, 14)
xlims <- c(0, 1)

npts <- 501
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
#a <- c(uFn(16, 2), uFn(16, 4), uFn(16, 8)) #alpha = 0.8

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


# ticksy <- seq(from = 0, to = ylims[2], by = 1)
# ylabels <- seq(from = 0, to = ylims[2], by = 1)
ticksx <- seq(from = 0, to = xlims[2], by = 0.2)
xlabels <- seq(from = 0, to = xlims[2], by = 0.2)
ticksy <- seq(from = 0, to = ylims[2], by = 2)
ylabels <- seq(from = 0, to = ylims[2], by = 2)
# ticksx <- c(xlims[1], 5.25, 8.944272, xlims[2])
# xlabels <- c(NA, expression(paste(x,"*")), expression(paste(bar(x))), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(0.01, 0.95, length.out = npts)

#Draw the graphs
lines(xx1, ACL(xx1), col = COLA[2], lwd = graphlinewidth)
lines(xx1, MCL(xx1), col = COLB[1], lwd = graphlinewidth)
lines(xx1, MRP(xx1), col = COL[3], lwd = graphlinewidth)
lines(xx1, Min(xx1), col = COL[6], lwd = graphlinewidth)
# with s=1
lines(xx1, ACL(xx1, s = 1), col = COLA[2], lwd = graphlinewidth, lty= 3)
lines(xx1, MCL(xx1, s = 1), col = COLB[2], lwd = graphlinewidth,lty = 3)

#Axis labels
mtext(expression(paste("Employment, ", n)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.1, 0.5*ylims[2], expression(paste("Marginal revenue and marginal cost, ", list(mrp, mcl) )), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Label the indifference curves
#text(8.1, 38, expression(u[1]^A), cex = labelsize)
#text(9.5, 38, expression(u[2]^A), cex = labelsize)
#text(11.3, 38, expression(u[3]^A), cex = labelsize)

#Label the feasible frontiers
#text(14, 7.5, expression(paste("Demand for x")), cex = labelsize)
#text(14, 6, expression(paste(x == frac(alpha*m, p[x]), " or")), cex = labelsize)
#text(14, 4.5, expression(paste(p[x] == frac(alpha*m, x))), cex = labelsize)

#Annotate max u point on feasible frontier
#text(5.2, ppf(5) + 0.2, expression(paste(i)), cex = labelsize)

#segments(0, 10, 5, 10, lty = 2, col = "gray", lwd = segmentlinewidth)
#segments(5, 0, 5, 10, lty = 2, col = "gray", lwd = segmentlinewidth)

# segments(0, 1, 4, 1,  lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(4, 0, 4, 1,  lty = 2, col = "gray", lwd = segmentlinewidth)
# 
# segments(0, 0.5, 8, 0.5, lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(8, 0, 8, 0.5, lty = 2, col = "gray", lwd = segmentlinewidth)
#segments(0, ppf(x = 16, bary = 40, slope = 2), 16, ppf(x = 16, bary = 40, slope = 2), lty = 2, col = "gray", lwd = segmentlinewidth)

points(employment(), ACL(employment()), pch = 16, col = "black", cex = 1)
points(employment(), MRP(employment()), pch = 16, col = "black", cex = 1)
points(employment(min=4.6), MCL(employment(min=4.6)), pch = 16, col = "black", cex = 1)
points(employment(min=4.6), Min(employment(min=4.6)), pch = 16, col = "black", cex = 1)
#with s=1
points(employment(s=1), ACL(employment(s=1)), pch = 16, col = "black", cex = 1)
points(employment(s=1), MRP(employment(s=1)), pch = 16, col = "black", cex = 1)
#points(employment(min=4.6,s=1), MCL(employment(min=4.6,s=1)), pch = 16, col = "black", cex = 1)
points(employment(min=4.6,s=1), Min(employment(min=4.6,s=1)), pch = 16, col = "black", cex = 1)
# points(4, 1, pch = 16, col = "black", cex = 1.5)
# points(8, 0.5, pch = 16, col = "black", cex = 1.5)



# text(2.3+0.2, ppf(2.3) + 0.2, expression(paste(a)), cex = labelsize)
# segments(2.3, 0, 2.3, ppf(x = 2.3), lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, ppf(x = 2.3), 2.3, ppf(x = 2.3), lty = 2, col = "gray", lwd = segmentlinewidth)
# points(2.3, ppf(x = 2.3), pch = 16, col = "black", cex = 1.5)


# text(7.7+0.2, ppf(7.7) + 0.2, expression(paste(b)), cex = labelsize)
# segments(7.7, 0, 7.7, ppf(x = 7.7), lty = 2, col = "gray", lwd = segmentlinewidth)
# segments(0, ppf(x = 7.7), 7.7, ppf(x = 7.7), lty = 2, col = "gray", lwd = segmentlinewidth)
# points(7.7, ppf(x = 7.7), pch = 16, col = "black", cex = 1.5)



dev.off()


