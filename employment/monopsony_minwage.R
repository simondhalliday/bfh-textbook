require(shape, rootSolve)

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
axislabelsize <- 1.6
graphlinewidth <- 2.5
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

ylims <- c(0, 14.1)
xlims <- c(0.25, 1.05)
npts <- 501
x <- seq(xlims[1], xlims[2], length.out = npts)
y <- seq(ylims[1], ylims[2], length.out = npts) 

# ----
# first plot: competitive vs monopsony
pdf(file = "employment/monopsony_minwage1.pdf", width = 12, height = 10)
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

ticksx <- c(xlims[1], employment(), 0.845, 1)
xlabels <- c(NA, expression(paste(n[M])), expression(paste(n[C])), 1)
ticksy <- c(ylims[1], ACL(0.85) - 0.8, ACL(employment()), ACL(0.845), MCL(employment()), ylims[2])
ylabels <- c(0, expression(paste(w[underline("w")])), expression(paste(w[M])), expression(paste(w[C])), expression(paste("mrp"["M"])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = axislabelsize)
axis(2, at = ticksy, pos = xlims[1], labels = ylabels, las = 1, cex.axis = axislabelsize)

npts <- 500 
xx1 <- seq(0.01, 0.95, length.out = npts)
xx2 <- seq(0, 1, length.out = 600)

#Draw the graphs
lines(xx1, ACL(xx1), col = COLA[5], lwd  = graphlinewidth)
lines(xx1, MCL(xx1), col = COLA[4], lwd = graphlinewidth)
lines(xx2, MRP(xx2), col = COLB[3], lwd = graphlinewidth)
lines(xx2, Min(xx2, 4), col = COL[2],lwd = graphlinewidth)
#lines(xx1, Min(xx1), col = COL[6], lwd = graphlinewidth)
# with s=1
## lines(xx1, ACL(xx1, s = 1), col = COLA[3], lwd = graphlinewidth, lty= 3)
## lines(xx1, MCL(xx1, s = 1), col = COLB[1], lwd = graphlinewidth,lty = 3)

#Axis labels
mtext(expression(paste("Employment, ", n)), side = 1, line = 2.5, cex = axislabelsize)
text(0.18, 0.5*ylims[2], expression(paste("Costs, the wage, and marginal revenue product, ", list(mcl, acl, w, mrp) )), xpd = TRUE, cex = axislabelsize, srt = 90) 


#add segments
segments(0, ACL(employment()), employment(), ACL(employment()), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(employment(), 0, employment(), MRP(employment()), lty = 2, col = "gray", lwd = segmentlinewidth)

segments(0, ACL(0.845), 0.845, ACL(0.845), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0.845, 0, 0.845, ACL(0.845), lty = 2, col = "gray", lwd = segmentlinewidth)

segments(0, MCL(employment()), employment(), MCL(employment()), lty = 2, col = "gray", lwd = segmentlinewidth)
## add points

points(employment(), ACL(employment()), pch = 16, col = "black", cex = 1.5)
points(employment(), MRP(employment()), pch = 16, col = "black", cex = 1.5)
points(0.845, ACL(0.845), pch = 16, col = "black", cex = 1.5)

# label points
text(employment() + 0.01, ACL(employment()) - 0.5 , "b", cex = axislabelsize)
text(employment() - 0.01, MCL(employment()) + 0.3 , "a", cex = axislabelsize)
text(0.845 + 0.01, ACL(0.85) - 0.4, "c", cex = axislabelsize)


# add labels to graphs
#text(1.03, 14.3, "Monopsony", cex = axislabelsize, xpd = TRUE)
text(1.01, 13.8, "Average cost", cex = axislabelsize, xpd = TRUE)
text(1.01, 13.3, "of labor", cex = axislabelsize, xpd = TRUE)
text(1.01, 12.8, "(acl)", cex = axislabelsize, xpd = TRUE)

#text(0.77, 14.3, "Monopsony", cex = axislabelsize, xpd = TRUE)
text(0.78, 13.8, "Marginal cost", cex = axislabelsize)
text(0.78, 13.3, "of labor", cex = axislabelsize)
text(0.78, 12.8, "(mcl)", cex = axislabelsize)

text(0.38, 13.8, "Marginal revenue", cex = axislabelsize)
text(0.38, 13.3, "product", cex = axislabelsize)
text(0.38, 12.8, "(mrp)", cex = axislabelsize)

text(0.58, ACL(0.85) - 1.1, "Minimum wage", cex = axislabelsize)
text(0.58, ACL(0.85) - 1.6, "not binding", cex = axislabelsize)

# text(0.85, 3.7, "Competitive", cex = axislabelsize)
# text(0.85, 3.2, "Marginal cost", cex = axislabelsize)
# text(0.85, 2.7, "of labor", cex = axislabelsize)
# text(0.85, 2.2, expression(paste( (mcl[C]) )), cex = axislabelsize)



dev.off()
# ----
# second plot:  monopsony and minwage
pdf(file = "employment/monopsony_minwage2.pdf", width = 12, height = 10)
par(mar =  c(5, 7, 3, 3))

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

ticksx2 <- c(xlims[1], employment(), employment(min = 4.6), 1)
xlabels2 <- c(NA, expression(paste(n[M])), expression(paste(n[underline("w")])), 1)
ticksy2 <- c(ylims[1], ACL(employment()), ACL(employment(min = 4.6)), MRP(employment(min = 4.6)), MCL(employment()), ylims[2])
ylabels2 <- c(0, expression(paste("w"["M"])),expression(paste(underline("w")["M"])), expression(paste("mrp"[underline("w")])), expression(paste("mrp"["M"])), NA)

#Axis Lines
axis(1, at = ticksx2, pos = 0, labels = xlabels2)
#axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)
axis(2, at = ticksy2, pos = xlims[1], labels = ylabels2, las = 1, cex.axis = axislabelsize)


#Draw the graphs
lines(xx1, ACL(xx1), col = COLA[5], lwd  = graphlinewidth)
lines(xx1, MCL(xx1), col = COLA[4], lwd = graphlinewidth)
lines(xx2, MRP(xx2), col = COLB[3], lwd = graphlinewidth)
lines(xx2,Min(xx2), col = COL[2],lwd = graphlinewidth)

#Axis labels
mtext(expression(paste("Employment, ", n)), side = 1, line = 2.5, cex = axislabelsize)
# text(-0.1, 0.5*ylims[2], expression(paste("Marginal revenue and marginal cost, ", list(mrp, mcl) )), xpd = TRUE, cex = axislabelsize, srt = 90) 
text(0.18, 0.5*ylims[2], expression(paste("Costs, the wage, and marginal revenue product, ", list(mcl, acl, w, mrp) )), xpd = TRUE, cex = axislabelsize, srt = 90) 


#add segments
segments(employment(), 0, employment(), MRP(employment()), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(employment(min = 4.6), 0, employment(min =  4.6), MCL(employment(min = 4.6)), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, MCL(employment()), employment(), MCL(employment()), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, MRP(employment(min = 4.6)), employment(min = 4.6), MRP(employment(min = 4.6)), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(0, ACL(employment()), employment(), ACL(employment()), lty = 2, col = "gray", lwd = segmentlinewidth)

# add points
points(employment(), ACL(employment()), pch = 16, col = "black", cex = 1.5)
points(employment(), MRP(employment()), pch = 16, col = "black", cex = 1.5)
points(employment(min=4.6), MCL(employment(min=4.6)), pch = 16, col = "black", cex = 1.5)
points(employment(min=4.6), Min(employment(min=4.6)), pch = 16, col = "black", cex = 1.5)
points(employment(min = 4.6), MRP(employment(min = 4.6)), pch = 16, col = "black", cex = 1.5)

# add labels
text(1.03, 13.8, "Average cost", cex = axislabelsize, xpd = TRUE)
text(1.03, 13.3, "of labor", cex = axislabelsize, xpd = TRUE)
text(1.03, 12.8, "(acl)", cex = axislabelsize, xpd = TRUE)

text(0.77, 13.8, "Marginal cost", cex = axislabelsize)
text(0.77, 13.3, "of labor", cex = axislabelsize)
text(0.77, 12.8, "(mcl)", cex = axislabelsize)

text(0.4, 13.8, "Marginal revenue", cex = axislabelsize)
text(0.4, 13.3, "product", cex = axislabelsize)
text(0.4, 12.8, "(mrp)", cex = axislabelsize)

text(employment()-0.01,ACL(employment())-0.5 , "b'", cex = axislabelsize)
text(employment()-0.01,MCL(employment())+0.5 , "a'", cex = axislabelsize)
text(employment(min = 4.6)-0.01,ACL(employment(min = 4.6))-0.5 , "e", cex = axislabelsize)
text(employment(min = 4.6)-0.01,MCL(employment(min =  4.6))+0.5 , "d'", cex = axislabelsize)
text(employment(min = 4.6)-0.01, MRP(employment(min = 4.6))+0.5, "f", cex = axislabelsize)

text(0.58, ACL(0.85) - 1.1, "Minimum wage", cex = axislabelsize)
text(0.58, ACL(0.85) - 1.6, "binding", cex = axislabelsize)

dev.off()
# ----
# third plot:  monopsony, minwage and EITC
pdf(file = "employment/monopsony_minwage3.pdf", width = 15, height = 12)
par(mar =  c(5, 7, 3, 3))

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


axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

#Draw the graphs
lines(xx1, ACL(xx1), col = COLA[3], lwd  = graphlinewidth)
lines(xx1, MCL(xx1), col = COLB[3], lwd = graphlinewidth)
lines(xx2, MRP(xx2), col = COL[3], lwd = graphlinewidth)
lines(xx2,Min(xx2), col = COL[2],lwd = graphlinewidth)
# with s=1
lines(xx1, ACL(xx1, s = 1), col = COLA[3], lwd = graphlinewidth, lty= 2)
lines(xx1, MCL(xx1, s = 1), col = COLB[3], lwd = graphlinewidth,lty = 2)

#Axis labels
mtext(expression(paste("Employment, ", n)), side = 1, line = 2.5, cex = axislabelsize)
text(-0.1, 0.5*ylims[2], expression(paste("Marginal cost, average cost, and marginal revenue product, ", list(mcl, acl, mrp) )), xpd = TRUE, cex = axislabelsize, srt = 90) 


#add segments
segments(employment(), 0, employment(), MRP(employment()), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(employment(min = 4.6), 0, employment(min =  4.6), MCL(employment(min = 4.6)), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(employment(min = 4.6, s=1), 0, employment(min =  4.6, s=1), MRP(employment(min = 4.6, s= 1)), lty = 2, col = "gray", lwd = segmentlinewidth)
segments(employment(s=1), 0, employment(s=1), MRP(employment(s= 1)), lty = 2, col = "gray", lwd = segmentlinewidth)
## add points

points(employment(), ACL(employment()), pch = 16, col = "black", cex = 1)
points(employment(), MRP(employment()), pch = 16, col = "black", cex = 1)
points(employment(min=4.6), MCL(employment(min=4.6)), pch = 16, col = "black", cex = 1)
points(employment(min=4.6), Min(employment(min=4.6)), pch = 16, col = "black", cex = 1)
#with s=1
points(employment(s=1), ACL(s=1,employment(s=1)), pch = 16, col = "black", cex = 1)
points(employment(s=1), MRP(employment(s=1)), pch = 16, col = "black", cex = 1)
points(employment(min=4.6,s=1), MCL(s=1,employment(min=4.6,s=1)), pch = 16, col = "black", cex = 1)
points(employment(min=4.6,s=1), Min(employment(min=4.6,s=1)), pch = 16, col = "black", cex = 1)

# add labels
text(1.03, 13.8, "Average cost", cex = axislabelsize, xpd = TRUE)
text(1.03, 13.3, "of labor", cex = axislabelsize, xpd = TRUE)
text(1.03, 12.8, "(acl)", cex = axislabelsize, xpd = TRUE)

text(0.77, 13.8, "Marginal cost", cex = axislabelsize)
text(0.77, 13.3, "of labor", cex = axislabelsize)
text(0.77, 12.8, "(mcl)", cex = axislabelsize)

text(0.4, 13.8, "Marginal revenue", cex = axislabelsize)
text(0.4, 13.3, "product", cex = axislabelsize)
text(0.4, 12.8, "(mrp)", cex = axislabelsize)

text(employment()-0.01,ACL(employment())-0.3 , "b", cex = axislabelsize)
text(employment()-0.01,MCL(employment())+0.3 , "a", cex = axislabelsize)
text(employment(min = 4.6)-0.01,ACL(employment(min = 4.6))-0.3 , "c", cex = axislabelsize)
text(employment(min = 4.6)-0.01,MCL(employment(min =  4.6))+0.3 , "d", cex = axislabelsize)
text(employment(s=1)-0.01,ACL(s=1,employment(s=1))-0.3 , "f", cex = axislabelsize)
text(employment(s=1)-0.01,MCL(employment(s=1),s=1)+0.3 , "e", cex = axislabelsize)
text(employment(s=1, min = 4.6)-0.01,4.6-0.3 , "g", cex = axislabelsize)
dev.off()
