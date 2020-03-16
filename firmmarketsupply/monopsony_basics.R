require(shape)
require(rootSolve)

#define functions
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
# solve the level of employment in different setting.
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

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

ylims <- c(0, 14.1)
xlims <- c(0.25, 1.05)
npts <- 501
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 
# first plot: competitive vs monopsony
pdf(file = "firmmarketsupply/monopsony_basics.pdf", width = 12, height = 10)
par(mar =  c(5, 10, 3, 5))

#Notice the plot starts at x = 0.2 not 0
plot(xlims[1], 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs = "i", 
     yaxs = "i"
)


ticksx <- c(xlims[1], employment(), 0.845, 1)
xlabels <- c(NA, expression(paste(l[M])), expression(paste(l[C])), NA)
ticksy <- c(ylims[1], ACL(employment()), ACL(0.845), MRP(employment()), ylims[2])
ylabels <- c(0, expression(paste(w[M])), expression(paste(w[C])), expression(paste(mrp[M])),NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = axislabelsize)
axis(2, at = ticksy, pos = xlims[1], labels = ylabels, las = 1, cex.axis = axislabelsize)

npts <- 500 
xx1 <- seq(0.01, 0.95, length.out = npts)
xx2 <- seq(0, 1, length.out = 600)

#Draw the graphs
lines(xx1, ACL(xx1), col = COLA[5], lwd  = graphlinewidth)
lines(xx1, MCL(xx1), col = COLA[4], lwd = graphlinewidth)
lines(xx2, MRP(xx2), col = COLB[3], lwd = graphlinewidth)

#Axis labels
mtext(expression(paste("Employment, ", l)), side = 1, line = 2.5, cex = axislabelsize)
text(0.12, 0.5*ylims[2], expression(paste("Costs, the wage, and marginal revenue product, ", list(mcl, acl, w, mrp) )), xpd = TRUE, cex = axislabelsize, srt = 90) 


#add segments
segments(0, ACL(employment()), employment(), ACL(employment()), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(employment(), 0, employment(), MRP(employment()), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, MRP(employment()), employment(), MRP(employment()), lty = 2, col = "gray", lwd = segmentlinewidth)

segments(0, ACL(0.845), xlims[2], ACL(0.845), lty = 1, col = COLA[3], lwd = segmentlinewidth)
segments(0.845, 0, 0.845, ACL(0.845), lty = 2, col = "gray", lwd = segmentlinewidth)

text(employment(), 9.5, expression(paste("Profit maximum at")), cex = annotatesize)
text(employment(), 9, expression(paste(mrp == mcl)), cex = annotatesize)
Arrows(employment(), 8.8, employment(), MRP(employment()) + 0.3, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


## add points

points(employment(), ACL(employment()), pch = 16, col = "black", cex = 1.5)
points(employment(), MRP(employment()), pch = 16, col = "black", cex = 1.5)
points(0.845, ACL(0.845), pch = 16, col = "black", cex = 1.5)

# label points
text(employment() + 0.01, ACL(employment()) - 0.3 , "b", cex = labelsize)
text(employment() + 0.01, MCL(employment()) - 0.3 , "a", cex = labelsize)
text(0.845 + 0.01, ACL(0.85) - 0.4, "c", cex = labelsize)


# add labels to graphs and polygon
text(1.04, 13.5, "Average cost \n of labor \n (acl)", cex = annotatesize, xpd = TRUE)

text(0.75, 13.4, "Marginal cost \n of labor \n (acl)", cex = annotatesize)

text(0.4, 13.4, "Marginal revenue \n product \n (mrp)", cex = annotatesize)

dev.off()
