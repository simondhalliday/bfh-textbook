# TO DO
# Troubleshoot points not showing up ---> Have infinite value nested
# Troubleshoot points label ---> equal infinity
# For labeling, reference LRn_Dec_BTE.R

require(shape)
pdf(file = "competitionmarkets/LRn_Dec_Costs_L.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(6, 6, 4, 4))

# Levels for the barriers to entry specified here
barriers <- c(0.2, 0.5, 0.71)
# Cost Levels
costs <- c(5, 10)

# ----
# Functions
Profit <- function(n, pbar = 60, c = costs[2], beta = 0.5) {
  (pbar - c)^2 / (((n + 1)^2)*beta)
}

totaln <- function(pi, pbar = 60, c = costs[2], beta = 0.5){
  (pbar - c)/(sqrt(pi*beta)) - 1
}

cournotPrice <- function(n, pbar = 60, c = costs[2]) {
  c + (1/(n+1))*(pbar - c)
}

# Barriers to entry
bte <- function(n, b = 0) {
  cournotPrice(n)*(1-b)
}

# Hack function that gets around nested function scope error 
bte_unnested <- function(n, pbar = 60, c, b = 0){
  (c + (1/(n+1))*(pbar - c))*(1 - b)
}

nstar <- function(b, pbar = 60, c = costs[2]){
  (pbar*(1 - b) - c) / (b*c)
}

# ---- 
xlims <- c(0, 40) 
ylims <- c(0, 25)

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(paste("")),
     ylab = expression(paste("")), 
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = axislabelsize, 
     bty = "n",
     xaxs="i", 
     yaxs="i")

ticksy <- c(0, 
            costs[1], 
            bte(n = nstar(b = barriers[2]), b = barriers[2]), 
            NA, # costs[2],
            ylims[2]
            )
# Asteriks need to be fixed
ylabels <- c(NA, 
             expression(paste(c[L])), 
             expression(paste(P(n**b, "*"))), 
             NA, 
             NA
             ) 

ticksx <- c(0, 
            NA,#nstar(b = barriers[2]), 
            nstar(b = barriers[2], c = costs[1]), 
            NA, # = Inf 
            nstar(b = barriers[1], c = costs[1]), # = Inf
            xlims[2]
            )

xlabels <- c(NA, 
             NA, 
             expression(paste(n**b)),
             NA,
             NA,
             NA
             )

axis(1, at = ticksx, pos = 0, labels = xlabels)
axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)

npts <- 500 
xx1 <- seq(xlims[1], xlims[2], length.out = npts)
# ----

# BTE --- Cost 1, Barrier = 0
lines(xx1, bte_unnested(n = xx1, c = costs[1], b = 0), col = COLA[4], lty = 1, lwd = graphlinewidth) 

# BTE --- Cost 1, Barrier[2]
lines(xx1, bte_unnested(n = xx1, c = costs[1], b = barriers[2]), col = COLA[3], lty = 2, lwd = graphlinewidth) 

# ----

#Label axes
mtext(expression(paste("Number of firms, ", n)), side=1, line = 2.5, cex = axislabelsize)
text(-5.5, 0.5*ylims[2], expression(paste("Costs, Price, and Expected Price, ", list(c, p, hat(p)) )), xpd = TRUE, cex = axislabelsize, srt = 90) 


# Label Expressions
# ---- 
# C[L] b = 0, p(n)
text(35, 7.3, expression(paste(p(n, c[L]))), cex = labelsize)
# C[L] b \neq 0, \hat{p(n)}
text(34, 2.5, expression(paste(hat(p)(n, c[L]) == (1-b)*p(n) )), cex = labelsize)
# ---- 


# C_L
segments(0, costs[1], xlims[2], costs[1], # Cut x_1 segment so it didn't overlap eq. 
         lty = 1, col = COLB[5] , lwd = graphlinewidth
)

segments(nstar(b = barriers[2], c = costs[1]), 0, nstar(b = barriers[2], c = costs[1]), bte(n = nstar(b = barriers[2]), b = barriers[2]),
         lty = 2, col = "gray", lwd = segmentlinewidth
)

segments(0, bte(n = nstar(b = barriers[2]), b = barriers[2]), nstar(b = barriers[2], c = costs[1]), bte(n = nstar(b = barriers[2]), b = barriers[2]),
         lty = 2, col = "grey", lwd = segmentlinewidth
)

# ---- 

# Label Points
# This should probably have a loop, but I haven't worked it out. 
points(nstar(b = barriers[2], c = costs[1]), bte(n = nstar(b = barriers[2]), b = barriers[2]),
       pch = 16, col = "black", cex = 1.5
)
points(nstar(b = barriers[2], c = costs[1]), bte_unnested(n = nstar(b = barriers[2], c = costs[1]), c = costs[1], b = barriers[2]), 
       pch = 16, col = "black", cex = 1.5
)


dev.off()