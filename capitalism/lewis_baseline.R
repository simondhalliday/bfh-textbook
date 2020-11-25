require(ggplot2)
require(shape)
require(plotrix)
require(pBrackets)
pdf(file = "capitalism/lewis_baseline.pdf", width = 9, height = 7)

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
CBCols <- c("#009E73","#0072B2","#E69F00","#CC79A7", "#F0E442","#D55E00")

WageFn <- function(h, ubar = 0.1, B = 0.1, t = 0.8) {
  B + ubar + (ubar - ubar*t +  ubar*t*h )/(t*(1 - h))
}

wCompCond <- function(b, rho = 0.05, gamma = prodlvls[1]){
  ((1 - b)/(1 + rho))*gamma
}

LewisFallback <- function(h, constant = 0.1, alpha = 0.5){
  constant*(1 - h)^(alpha - 1)
}


#Ylims in this figure are set at 40 for legacy reasons, 
#so multiply by 40 to get the y coordinates; was too hard 
#to re-do everything
#wCompCond(b = 0.5)*40
#Work out the x-coordinates where the Wage function 
#equals the competition condition at the parameters
x <- seq(xlims[1], xlims[2], length.out = npts)
barriers <- 0.55
oppcost <- 0.05
prodlvls <- c(0.9, 1.4)
benefits <- c(0.1, 0.2)

eq1 <- uniroot(function(x)  WageFn(x) - wCompCond(b = barriers[1], rho = oppcost[1])  , c(.01,1), tol=1e-8)   
eq2 <- uniroot(function(x)  WageFn(x, B = benefits[2]) - wCompCond(b = barriers[1], gamma = prodlvls[2], rho = oppcost[1])  , c(.01,1), tol=1e-8)   

emplvls <- c(as.numeric(eq1[1]), as.numeric(eq2[1]))
wagelvls <- c(wCompCond(b = barriers[1]), wCompCond(b = barriers[1], gamma = prodlvls[2]))
fallbacklvls <- c(LewisFallback(emplvls[1]), LewisFallback(emplvls[2]))



#COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99")
par(mar =  c(6.5, 5, 2, 7))
xlims <- c(0, 1.05)
ylims <- c(0, 1.45)


plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")),
     xaxt = "n", 
     yaxt = "n",
     cex.lab = axislabelsize, 
     line = 2.5,
     bty = "n", 
     xaxs="i", 
     yaxs="i")

npts <- 500 
npts2 <- 501
#Specify the sequences of points for graphing. 
xx1 <- seq(xlims[1], 1, length.out = npts)
xx2 <- seq(xlims[1], xlims[2], length.out = npts)
xx3 <- seq(xlims[1], xlims[2], length.out = npts2)
xx4 <- seq(xlims[1], 25, length.out = npts2)
xx5 <- seq(xlims[1], 0.75, length.out = npts2)

# Shade below green line
# polygon(c(0, xx2, xlims[2]), 
#         c(0, WageFn(xx2), 0), border = FALSE, col = COLA[1])
# rect(0.99, 0, 1, 1, border = FALSE, col = COLA[1])

#Axis labels
text(0.5*xlims[2], -0.3, expression(paste("Employment in the capitalist sector as \n a fraction of all hours worked, H")), cex = axislabelsize, xpd = TRUE)
text(-0.125, 0.5*ylims[2], expression(paste("Wage, ", w)), cex = axislabelsize, xpd = TRUE, srt = 90)


#Draw the lines for the graphs
lines(xx1, WageFn(xx1), col = CBCols[1], lwd = graphlinewidth)
#lines(xx1, WageFn(xx1, B = benefits[2]), col = CBCols[1], lwd = graphlinewidth)
#lines(xx1, LewisFallback(xx1), col = CBCols[3], lwd = graphlinewidth)


#Annotation of the  graphs
#Wage curves
#text(0.96, 1.6, expression(paste("Wage curves")), cex = labelsize, xpd = TRUE)
#text(0.81, 1.15, expression(paste(w[2]^N*(H) )), cex = labelsize, xpd = TRUE)
text(0.86, 0.7, expression(paste(w^N*(H) )), cex = labelsize, xpd = TRUE)

text(0.86, 0.22, expression(paste("Informal sector")), cex = labelsize, xpd = TRUE)
text(0.86, 0.15, expression(paste("productivity, ", gamma^I)), cex = labelsize, xpd = TRUE)

#text(0.75, 38, expression(paste("Employment level, ", H^N)), cex = labelsize,  xpd = TRUE)
# text(0.71, 43, expression(paste("Employment")), cex = labelsize,  xpd = TRUE)
# text(0.71, 41, expression(paste("level, ", H^N)), cex = labelsize,  xpd = TRUE)


#Arrows(0.8, 15, 0.8, 19, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#Arrows(0.8, 15, 0.8, 6, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
#text(0.92, 12.5, expression(paste("Employment Rent")))


#Customize ticks and labels for the plot
ticksy <- c(0, wagelvls[1], prodlvls[1], ylims[2])
ylabels <- c(0, expression(paste(w^C)), expression(paste(gamma)),  NA)
ticksx <- c(0, emplvls[1], 1)
xlabels <- c(0, expression(paste(H^N)), 1)
axis(1, at = ticksx, pos = 0, labels = xlabels, cex.axis = labelsize)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1, cex.axis = labelsize)



#Competition conditions
#CC1
segments(0, wagelvls[1], emplvls[1], wagelvls[1], lty = 1, lwd = graphlinewidth, col = CBCols[2])
segments(emplvls[1], wagelvls[1], 1, wagelvls[1], lty = 1, lwd = graphlinewidth, col = CBCols[2])
segments(emplvls[1], 0, emplvls[1], wagelvls[1], lty = 2, lwd = segmentlinewidth, col = grays[22])

#CC2
# segments(0, wagelvls[2], emplvls[2], wagelvls[2], lty = 1, lwd = graphlinewidth, col = CBCols[2])
# segments(emplvls[2], wagelvls[2], 1, wagelvls[2], lty = 1, lwd = graphlinewidth, col = CBCols[2])
# segments(emplvls[2], 0, emplvls[2], wagelvls[2], lty = 2, lwd = segmentlinewidth, col = grays[22])

# Informal fallback
segments(0, LewisFallback(h = 0), 1, LewisFallback(h = 0), lty = 2, lwd = graphlinewidth, col = CBCols[3])
#segments(emplvls[2], wagelvls[2], 1, wagelvls[2], lty = 1, lwd = graphlinewidth, col = CBCols[2])



# Gammas
#gamma 1
segments(0, prodlvls[1], 1, prodlvls[1], lty = 2, lwd = 2, col = COLA[3])
#gamma 2
#segments(0, prodlvls[2], 1, prodlvls[2], lty = 2, lwd = 2, col = COLA[3])

#text(1.11, prodlvls[1] + 0.07, expression(paste("Initial")), cex = labelsize,  xpd = TRUE)
text(1.11, prodlvls[1] + 0.07 , expression(paste("Output per")), cex = labelsize,  xpd = TRUE)
text(1.11, prodlvls[1], expression(paste("worker, ", gamma )), cex = labelsize,  xpd = TRUE)


# text(1.11, 1.47, expression(paste("New")), cex = labelsize,  xpd = TRUE)
# text(1.11, 1.4, expression(paste("output per")), cex = labelsize,  xpd = TRUE)
# text(1.11, 1.33, expression(paste("worker, ", gamma[2])), cex = labelsize,  xpd = TRUE)

# Supply of labor
segments(1, 0, 1, ylims[2], lty = 2, lwd = segmentlinewidth, col = grays[20], xpd = TRUE)
# text(1.11, 0.15, expression(paste("Supply of")), cex = labelsize,  xpd = TRUE)
# text(1.11, 0.08, expression(paste("labor, ", H == 1)), cex = labelsize,  xpd = TRUE)


# points(emplvls[2], wagelvls[2], pch = 16, col = "black", cex = 1.5)
# text(emplvls[2] - 0.02, wagelvls[2] + 0.04,  expression(paste(b)), cex = labelsize)

points(emplvls[1], wagelvls[1], pch = 16, col = "black", cex = 1.5)
text(emplvls[1] + 0.02, wagelvls[1] - 0.04,  expression(paste(a)), cex = labelsize)

# points(emplvls[1], fallbacklvls[1], pch = 16, col = "black", cex = 1.5)
# text(emplvls[1] + 0.02, fallbacklvls[1] - 0.04,  expression(paste(d)), cex = labelsize)

# points(emplvls[2], fallbacklvls[2], pch = 16, col = "black", cex = 1.5)
# text(emplvls[2] + 0.02, fallbacklvls[2] - 0.04,  expression(paste(e)), cex = labelsize)


#Unemployment benefits & a
#segments(0, 5, 1.2, 5, lty = 2, lwd = 2, col = "darkgray")
#segments(0, 2.5, 1.2, 2.5, lty = 2, lwd = 2, col = "darkgray")

#Zero profit condition
text(1.12, 19, expression(paste("Competition")), cex = labelsize, xpd = TRUE)
text(1.12, 17, expression(paste("condition, ", w^C)), cex = labelsize, xpd = TRUE)
#text(1.02, 21.25, expression(paste(w == bar(Delta))), cex = labelsize, xpd = TRUE)

# text(0.25, 23, expression(paste("Firms leaving")), cex = labelsize)
# text(0.25, 17, expression(paste("Firms entering")), cex = labelsize)

# Arrows(0.6, 19, 0.6, 10, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
# text(0.42, 28, expression(paste("If the ", w>w^c,  " profits will")), cex = labelsize)
# text(0.42, 26, expression(paste("be insufficient and firms")), cex = labelsize)
# text(0.42, 24, expression(paste("will leave")), cex = labelsize)

# text(0.6, 7, expression(paste("No")), cex = labelsize)
# text(0.6, 5, expression(paste("production")), cex = labelsize)

#text(0.97, 6, expression(paste(B + a/t)))
#text(0.97, 3.5, expression(paste(B, " (unemployment benefits)")))
#text(1.08, 36, expression(paste("level of")))
#text(1.08, 34, expression(paste("employment, ", bar(H))))

#Profits
brackets(x1 = 1.02, y1 = prodlvls[1] - 0.02, x2 = 1.02, y2 = wagelvls[1] + 0.02,  
         ticks = 0.5, curvature = 0.5, type = 1, 
         col = "black", lwd = 2, h = 0.03,
         lty = 1, xpd = TRUE)
text(1.15, (prodlvls[1] + wagelvls[1])/2 + 0.035, expression(paste("Accounting")), cex = labelsize, xpd = TRUE)
text(1.15, (prodlvls[1] + wagelvls[1])/2 - 0.035, expression(paste("profits, ", pi^A)), cex = labelsize, xpd = TRUE)

#Employment rents
brackets(x1 = 1.02, y1 = wagelvls[1] - 0.02, x2 = 1.02, y2 = LewisFallback(0) + 0.02,  
         ticks = 0.5, curvature = 0.5, type = 1, 
         col = "black", lwd = 2, h = 0.03,
         lty = 1, xpd = TRUE)
text(1.15, (LewisFallback(0)  + wagelvls[1])/2 + 0.035, expression(paste("Employment")), cex = labelsize, xpd = TRUE)
text(1.15, (LewisFallback(0)  + wagelvls[1])/2 - 0.035, expression(paste("rent")), cex = labelsize, xpd = TRUE)


brackets(x1 = emplvls[1] - 0.02, y1 = - 0.01, x2 = 0.02, y2 = - 0.01,  
         ticks = 0.5, curvature = 0.5, type = 1, 
         col = "black", lwd = 2, h = 0.03,
         lty = 1, xpd = TRUE)

text(emplvls[1]/2, - 0.07, expression(paste("Capitalist")), cex = labelsize, xpd = TRUE)
text(emplvls[1]/2, - 0.13, expression(paste("employment")), cex = labelsize, xpd = TRUE)


brackets(x1 = 1 - 0.02, y1 = - 0.01, emplvls[1] + 0.02, y2 = - 0.01,  
         ticks = 0.5, curvature = 0.5, type = 1, 
         col = "black", lwd = 2, h = 0.03,
         lty = 1, xpd = TRUE)

text((1 + emplvls[1])/2, - 0.07, expression(paste("Informal")), cex = labelsize, xpd = TRUE)
text((1 + emplvls[1])/2, - 0.13, expression(paste("work")), cex = labelsize, xpd = TRUE)


# text(0.55, 15, expression(paste("Wages")))
# Arrows(0.5, 18, 0.5, 12, col = "black", lty = 1, lwd = 2, arr.type = "triangle")
# Arrows(0.5, 12, 0.5, 18, col = "black", lty = 1, lwd = 2, arr.type = "triangle")


dev.off()

