#Graph Designer: Simon Halliday
#Authors: Bowles, Foley and Halliday
#Title: Coordination, Conflict and Competition: A Text in Microeconomics

library(shape)
pdf(file = "credit/non_contractual_brf_part_a.pdf", width = 8, height = 6)

#Set parameters for graphics
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
grays <- gray.colors(25, start = 1, end = 0, alpha = 1)

#Edited the margins to cater for the larger LHS labels
par(mar =  c(4, 5, 1, 1))

brfFn <- function(delta, mu = 1) {
  .5 + (delta / (2 * mu)) 
}

isoreturnFn <- function(delta, pi=0.125) {
  1 - (pi)/delta
}


yFn <- function(d1, f1, mu = 1){
  mu*f1*(1 - f1) - d1*(1 - f1)
}

ylow <- function(delta, mu = 1, ybar = 0.03){
  (-sqrt(delta^2 - 2*delta*mu + mu^2 - 4*mu*ybar) + delta + mu)/(2*mu)
}

yhigh <- function(delta, mu = 1, ybar = 0.03){
  (sqrt(delta^2 - 2*delta*mu + mu^2 - 4*mu*ybar) + delta + mu)/(2*mu)
}

xlims <- c(0, 1)
ylims <- c(0, 1.05)

npts <- 501 
d1 <- seq(xlims[1], xlims[2], length.out = npts)
f1 <- seq(ylims[1], ylims[2], length.out = npts) 
a <- c(0.03, 0.0625, 0.109)

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


# ticksy <- seq(from = 0, to = ylims[2], by = 0.1)
# ylabels <- seq(from = 0, to = ylims[2], by = 0.1)
# ticksx <- seq(from = 0, to = xlims[2], by = 0.1)
# xlabels <- seq(from = 0, to = xlims[2], by = 0.1)
#ticksy <- c(ylims[1], 0.5, brfFn(delta = 0.34), brfFn(delta = 0.5), brfFn(delta = 0.65), yhigh(delta = 0.5), ylims[2])
#ticksy <- c(ylims[1], 0.5, brfFn(delta = 0.34), NA, brfFn(delta = 0.5), NA, NA, ylims[2])
ticksy <- c(ylims[1], NA, NA, ylow(delta = 0.34, ybar = 0.0625), brfFn(delta = 0.34), NA, yhigh(delta = 0.34, ybar = 0.0625), ylims[2])

ylabels <- c(NA, NA, NA, NA,NA,NA, NA, NA)
#ylabels <- c(NA, expression(paste(f == frac(1,2))), expression(paste(f^d)), expression(paste(f*(delta^L) )),  expression(paste(f*(delta^n) == frac(3,4))),expression(paste(f*(delta^H) )), expression(paste(f^e)), NA)
ticksx <- c(xlims[1], 0.34, 0.5, 0.65, xlims[2])
xlabels <- c(NA,  expression(paste(delta[b])), expression(paste(delta[n])), expression(paste(delta[e])), NA)

axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1,cex.axis = labelsize)

#ylabels <- c(NA, expression(paste(f == frac(1,2))), expression(paste(f^d)), expression(paste(f*(delta^L) )),  expression(paste(f*(delta^n) == frac(3,4))),expression(paste(f*(delta^H) )), expression(paste(f^e)), NA)
#text(-0.08, 0.45, expression(paste(f == frac(1,2))), cex = labelsize, xpd =TRUE)

text(-0.05, ylow(delta = 0.34, ybar = 0.0625), expression(paste(f[h])), cex = labelsize, xpd =TRUE)
#text(-0.07, 0.665, expression(paste(f*(delta^L) )), cex = labelsize, xpd =TRUE)
#text(-0.105, 0.753, expression(paste(f*(delta^N) ==frac(3,4) )), cex = labelsize, xpd =TRUE)
#text(-0.07, 0.832, expression(paste(f*(delta^n))), cex = labelsize, xpd =TRUE)
#text(-0.07, 0.93, expression(paste(f*(delta^H) )), cex = labelsize, xpd =TRUE)
text(-0.05, brfFn(delta = 0.34), expression(paste(f[b])), cex = labelsize, xpd =TRUE)
text(-0.05, yhigh(delta = 0.34, ybar = 0.0625), expression(paste(f[g])), cex = labelsize, xpd =TRUE)
# y-labels with numeric values
# text(-0.0905, 0.48, expression(paste(frac(1,2) == f)), cex = labelsize, xpd =TRUE)
# text(-0.05, 0.58, expression(paste(0.57 == f^d)), cex = labelsize, xpd =TRUE)
# text(-0.07, 0.66, expression(paste(0.67 == f*(delta^L) )), cex = labelsize, xpd =TRUE)
# text(-0.105, 0.76, expression(paste(frac(3,4) == f*(delta^n))), cex = labelsize, xpd =TRUE)
# text(-0.07, 0.84, expression(paste(0.83 == f*(delta^n) )), cex = labelsize, xpd =TRUE)
# text(-0.07, 0.93, expression(paste(0.93 == f*(delta^H) )), cex = labelsize, xpd =TRUE)
# text(-0.05, 1.05, expression(paste(1.05f^e)), cex = labelsize, xpd =TRUE)


npts <- 503 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
xx2 <- seq(0, 1, length.out = npts)

#Draw the graphs
#lines(xx1, brfFn(xx1), col = COLA[4], lwd = graphlinewidth)

#Axis labels
mtext(expression(paste("Interest factor, ", delta)), side = 1, line = 3, cex = axislabelsize)
text(-0.1, 0.5*(ylims[2]), expression(paste("Probability of failure (risk), ", f)), xpd = TRUE, cex = axislabelsize, srt = 90) 

#Contour lines
contour(d1, f1,
        outer(d1, f1, yFn),
        drawlabels = FALSE,
        col = COLA[3],
        lwd = graphlinewidth,
        levels = a,
        xaxs="i",
        yaxs="i",
        add = TRUE)

segments(0.5, 0.63, 0.5, yhigh(delta = 0.5) - 0.05, lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, yhigh(delta = 0.34, ybar = 0.0625), 0.34, yhigh(delta = 0.34, ybar = 0.0625), lty = 2, col = grays[20] , lwd = segmentlinewidth)
segments(0, ylow(delta = 0.34, ybar = 0.0625), 0.34, ylow(delta = 0.34, ybar = 0.0625), lty = 2, col = grays[20] , lwd = segmentlinewidth)

segments(0.34, 0, 0.34, ylims[2], lty = 1, col = COLB[3], lwd = segmentlinewidth)
segments(0.655, brfFn(delta = 0.655) - 0.12, 0.655, brfFn(delta = 0.655) + 0.12, lty = 2, col = grays[20] , lwd = segmentlinewidth)

segments(0, 0, 1, 1, lty = 1, col = COLA[3] , lwd = segmentlinewidth)

# 
# segments(8, 0, 8,indiffA(x = 8)  , lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(0, indiffA(x = 8) , 8,indiffA(x = 8)  , lty = 2, col = "gray" , lwd = segmentlinewidth)

text(0.5+ 0.025, brfFn(delta = 0.5) - 0.02, expression(paste(n)), cex = labelsize)
points(0.5, brfFn(delta = 0.5), pch = 16, col = "black", cex = 1.5)
text(0.34+ 0.025, brfFn(delta = 0.34) - 0.02, expression(paste(b)), cex = labelsize)
points(0.34, brfFn(delta = 0.34), pch = 16, col = "black", cex = 1.5)
text(0.65 + 0.02, brfFn(delta = 0.65) - 0.02, expression(paste(e)), cex = labelsize)
points(0.655, brfFn(delta = 0.655), pch = 16, col = "black", cex = 1.5)

#Points g and h on the Nash iso-expected income
#but on the lower delta for delta[b]
text(0.34 - 0.02, ylow(delta = 0.34, ybar = 0.0625) + 0.03, expression(paste(h)), cex = labelsize)
points(0.34, ylow(delta = 0.34, ybar = 0.0625), pch = 16, col = "black", cex = 1.5)
text(0.34 - 0.02, yhigh(delta = 0.34, ybar = 0.0625) - 0.028, expression(paste(g)), cex = labelsize)
points(0.34, yhigh(delta = 0.34, ybar = 0.0625), pch = 16, col = "black", cex = 1.5)



text(0.53, 1.02, expression(paste("Iso-expected")), cex = labelsize)
text(0.53, 0.97, expression(paste("income curves")), cex = labelsize)
#text(0.3, 1.01, expression(paste(y[1] == y^L)), cex = labelsize)
#text(0.3, 0.88, expression(paste(y[2] == y^{NE})), cex = labelsize)
#text(0.2, 0.75, expression(paste(y[3] == y^H)), cex = labelsize)

text(0.6, 0.55, expression(paste(hat(y)[0])), cex = labelsize - 0.15)
text(0.445, 0.55, expression(paste(hat(y)[1])), cex = labelsize - 0.15)
text(0.37, 0.55, expression(paste(hat(y)[2])), cex = labelsize - 0.15)
text(0.275, 0.55, expression(paste(hat(y)[3])), cex = labelsize - 0.15)
text(0.55, 0.24, expression(paste("Slope", phantom() == -mrs(delta, f))), cex = labelsize)
text(0.54, 0.14, expression(paste(phantom() == -frac(hat(y)[delta], hat(y)[f]))), cex = labelsize)
#text(0.61, 0.14, expression(paste(phantom() == frac(1 - f, q*(1 - 2*f) + delta))), cex = labelsize)
Arrows(0.45, 0.26, 0.45, 0.48, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)


#text(0.82, 0.6, expression(paste("More expected income")), cex = labelsize, xpd = TRUE)
text(0.175, 0.65, expression(paste("Better for borrower")), cex = labelsize,xpd = TRUE)
Arrows(0.2, 0.6, 0.05, 0.6, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

text(0.85, 1.02, expression(paste("Participation")), cex = labelsize)
text(0.85, 0.97, expression(paste("constraint")), cex = labelsize)



#text(0.85, 0.68, expression(paste("A's Best Response")), cex = labelsize, xpd  = TRUE)
#text(0.85, 0.63, expression(paste("function")), cex = labelsize, xpd  = TRUE)
#text(0.86, 0.55, expression(paste("Slope ", phantom() == frac(1, 2*q))), cex = labelsize, xpd  = TRUE)
#Arrows(0.85, 0.73, 0.85, 0.89, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

dev.off()
