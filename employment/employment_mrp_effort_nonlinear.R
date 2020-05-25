require(shape, rootSolve)

#define functions

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
grays <- gray.colors(25, start = 1, end = 0)


ylims <- c(0, 14.1)
xlims <- c(0.25, 1.05)
npts <- 501
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

# ----
# first plot: competitive vs monopsony
pdf(file = "employment/employment_mrp_effort_nonlinear.pdf", width = 12, height = 10)
par(mar =  c(5, 7, 3, 3))

#Notice the plot starts at x = 0.2 not 0
plot(xlims[1], 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n", 
     xaxs="i", 
     yaxs="i"
)

# ticksx <- seq(from = 0, to = xlims[2], by = 0.2)
# xlabels <- seq(from = 0, to = xlims[2], by = 0.2)
# ticksy <- seq(from = 0, to = ylims[2], by = 2)
# ylabels <- seq(from = 0, to = ylims[2], by = 2)

ticksx <- c(xlims[1], 0.845, 1)
xlabels <- c(NA, expression(paste(l^{N})), NA)
ticksy <- c(ylims[1], ACL(0.845), ylims[2])
ylabels <- c(NA,  expression(paste(frac(w^N,e^N) == c)), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = axislabelsize)
axis(2, at = ticksy, pos = xlims[1], labels = ylabels, las = 1, cex.axis = axislabelsize)

npts <- 500 
xx1 <- seq(0.01, 0.95, length.out = npts)
xx2 <- seq(0, 1, length.out = 600)

#Draw the graphs
lines(xx2, MRP(xx2), col = COLA[4], lwd = graphlinewidth)
#lines(xx2, Min(xx2, 4), col = COL[2],lwd = graphlinewidth)
#lines(xx1, Min(xx1), col = COL[6], lwd = graphlinewidth)
# with s=1
## lines(xx1, ACL(xx1, s = 1), col = COLA[3], lwd = graphlinewidth, lty= 3)
## lines(xx1, MCL(xx1, s = 1), col = COLB[1], lwd = graphlinewidth,lty = 3)

#Axis labels
mtext(expression(paste("Total labor used by the employer, ", l == eh)), side = 1, line = 2.5, cex = axislabelsize)
text(0.17, 0.5*ylims[2], expression(paste("Wage per unit of effort, ", c == frac(w,e) )), xpd = TRUE, cex = axislabelsize, srt = 90) 


#add segments
segments(0, ACL(0.845), 1, ACL(0.845), lty = 1, col = COLB[3], lwd = graphlinewidth)
segments(0.845, 0, 0.845, ACL(0.845), lty = 2, col = "gray", lwd = segmentlinewidth)

## add points

points(0.845, ACL(0.845), pch = 16, col = "black", cex = 1.5)

# label points
text(0.845 + 0.01, ACL(0.85) - 0.4, "n", cex = labelsize)

# add labels to graphs
text(0.38, 13.8, "Marginal revenue product", cex = labelsize)
text(0.38, 13.3, "of labor", cex = labelsize)
text(0.38, 12.8, "(mrp)", cex = labelsize)

#text(0.58, ACL(0.85) - 1.1, "Minimum wage", cex = axislabelsize)
#text(0.58, ACL(0.85) - 1.6, "not binding", cex = axislabelsize)

# text(0.85, 3.7, "Competitive", cex = axislabelsize)
# text(0.85, 3.2, "Marginal cost", cex = axislabelsize)
# text(0.85, 2.7, "of labor", cex = axislabelsize)
# text(0.85, 2.2, expression(paste( (mcl[C]) )), cex = axislabelsize)

dev.off()