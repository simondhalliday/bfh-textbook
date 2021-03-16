require(shape)
pdf(file = "competitionmarkets/consumersurplus_profits_lines.pdf", width = 9, height = 7)

#Set parameters for graphics
pointsize <- 1.8
axislabelsize <- 1.8
labelsize <- 1.5
namesize <- 1.8
annotatesize <- 1.5
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 6, 4, 4))

marketProfit <- function(n, s = 0.5, pmax = 20, c1 = 2) {
  n*(1/(n+1)^2)*(pmax - c1)^2/(s)
}

consumerSurplus <- function(n, s = 0.5, pmax = 20, c1 = 2) {
  (1/2)*(pmax - (c1 + ((1/(n+1))*(pmax -c1)  )) )*(n/(n+1))*( (pmax - c1)/(s))
}



# brfPE <- function(ea, alpha = 16, beta = 1/12) {
#   alpha*(1 - beta*ea)
# }
# 
# indiffeA <- function(ea, alpha = 16, beta = 1/24, uA = 46.08) {
#   1/beta - (0.5*ea)/(alpha*beta) - uA/(alpha*beta*ea)
# }
# 
# indiffB <- function(eb, alpha = 16, beta = 1/24, uB = 46.08) {
#   (alpha*eb - uB - 0.5*eb^2)/(alpha*beta*eb)
# }
# 
# indiffB2 <- function(ea, alpha = 16, beta = 1/24, uB = 46.08) {
#   sqrt(alpha^2*(beta*ea - 1)^2 - 2*uB) - alpha*beta*ea + alpha
# }
# 
# indiffB3 <- function(ea, alpha = 16, beta = 1/24, uB = 46.08) {
#   -sqrt(alpha^2*(beta*ea - 1)^2 - 2*uB) - alpha*beta*ea + alpha
# }


#Input into Wolfram Alpha: solve for x y = 1/b - (0.5*x)/(a*b) - u/(a*b*x)

# indiffAlowB <- function(eb, alpha = 16, beta = 1/24, uA = 46.08) {
#   0.5*(-sqrt((2*alpha*beta*eb - 2*alpha)^2 - 8*uA)) - 2*alpha*beta*eb + 2*alpha
# }
# 
# indiffAlow <- function(ea, alpha = 16, beta = 1/24, uA = 46.08) {
#   0.5*(-sqrt((2*alpha*beta*ea - 2*alpha)^2 - 8*uA)) - 2*alpha*beta*ea + 2*alpha
# }
# 
# indiffAgain <- function(ea, alpha = 16, beta = 1/24, uA = 46.08) {
#   (-2*alpha*ea +2*uA + ea^2 )/ ( 2 * alpha * beta * ea)
# }

library(tidyverse)
xlims <- c(1, 17)

n <- seq(xlims[1], xlims[2], by = 1)
profitN <- marketProfit(n)
csN <- consumerSurplus(n)
df <- data.frame(n, profitN, csN)

df1 <- 
  df %>% 
  gather(type, value, -n)

df1 %>%
  ggplot(aes(x = n, y = value, shape = type, color = type)) + 
  geom_point(size = 2.5) + 
  geom_line(lwd = 0.6) + 
  #geom_bar(stat = "identity", position = "dodge") + 
  xlab("Number of firms, n") +
  ylab("Total economic profit and consumer surplus") + 
  scale_shape_discrete(name = "",
                    breaks = c("csN", "profitN"),
                    labels = c("Total consumer surplus", "Total economic profit")
  ) +
  scale_color_brewer(type = "qual", palette = "Set1", name = "",
                     breaks = c("csN", "profitN"),
                     labels = c("Total consumer surplus", "Total economic profit")
  ) +
  theme_bw() + 
  scale_x_continuous(breaks = c(1,5,10,15)) +
  theme(legend.position="top",
                     axis.title = element_text(size = 20),
                     axis.title.y = element_text(size = 20),
                     axis.text.y = element_text(size = 18),
                     axis.text.x = element_text(size = 18),
                     #legend.title = element_text(size = 16),
                     legend.text = element_text(size = 18),
                     legend.title = element_text(size = 18))
#ggsave(file = "competitionmarkets/consumersurplus_profits.pdf", device = "pdf", width = 6, height = 4)


#B's value when at A's bliss point
#0.35*log(5.88) + 0.35*log(8.88) + 0.5*log(10 - 5.88) + 0.5*log(15 - 8.88) 

#B's bliss point x = 4.11765; y = 6.17647
#A's value when at A's bliss point
#0.5*log(4.11765) + 0.5*log(6.17647) + 0.35*log(10 - 4.11765) + 0.35*log(15 - 6.17647) 

# 
# plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
#      xlab = expression(paste("")),
#      ylab = expression(paste("")), 
#      xaxt = "n", 
#      yaxt = "n", 
#      cex.lab = axislabelsize, 
#      bty = "n",
#      xaxs="i", 
#      yaxs="i")
# 
# # ticksy <- seq(from = 0, to = ylims[2], by = 20)
# # ylabels <- seq(from = 0, to = ylims[2], by = 20)
# # ticksx <- seq(from = 0, to = xlims[2], by = 2)
# # xlabels <- seq(from = 0, to = xlims[2], by = 2)
# ticksy <- c(0, ylims[2])
# ylabels <- c(NA, NA)
# ticksx <- c(0, xlims[2])
# xlabels <- c(NA, NA)
# 
# axis(1, at = ticksx, pos = 0, labels = xlabels)
# 
# #text(x = c(0, 12, 18, 36, xlims[2]), par("usr")[3] - 0.4, labels = xlabels, srt = 0, pos = 1, xpd = TRUE)
# 
# axis(2, at = ticksy, pos = 0, labels = ylabels, las = 1)
# 
# npts <- 500 
# xx1 <- seq(xlims[1], xlims[2], length.out = npts)
# xx2 <- seq(1, xlims[2], length.out = npts)
# 
# lines(xx2, consumerSurplus(xx2, s = 1/2, pmax = 20, c1 = 2), col = COLA[4], lwd = graphlinewidth)
# lines(xx2, marketProfit(xx2, s = 1/2, pmax = 20, c1 = 2), col = COLB[4], lwd = graphlinewidth)

# segments(0, 12, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)
# segments(12, 0, 12, 12, lty = 2, col = "gray" , lwd = segmentlinewidth)

#mtext(expression(paste("A's output, ", x^A)), side=1, line = 2.5, cex = axislabelsize)
text(0.5*(xlims[2]), -35, expression(paste("Number of firms, ", n)), xpd = TRUE, cex = axislabelsize) 
text(-1.8, 0.5*ylims[2], expression(paste("Market profits and consumer surplus, ($)")), xpd = TRUE, cex = axislabelsize, srt = 90) 

#segments(4.11765, 6.17647, 5.88, 8.88, lty = 1, col = COL[2] , lwd = graphlinewidth)
#text(7.3, 3, expression("Pareto Efficient"))
#text(7.3, 2.5, expression("Curve"))
#Arrows(7.3, 3.5, 7.3, 6.1, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#Label the iso-welfare functions for the HG, Aisha
# text(3.8, 1.5, expression(u[1]^A))
# text(4.6, 1.5, expression(u[2]^A))
# text(5.5, 1.5, expression(u[3]^A))
#text(6.6, 8.3, expression(u[4]^A))

#Label the indifference curves for the HG, Betty
# text(7.6, 17, expression(u[1]^B))
# text(6.75, 17, expression(u[2]^B))
# text(6, 17, expression(u[3]^B))
#text(3.4, 6.9, expression(v[4]^B))

#Label point i. 
# points(12, 12, pch = 16, col = "black", cex = 1.5)
# text(16.5, 12.5, expression(paste("Nash Equilibrium")))
# text(11.3, 11.3, expression(paste("n")))

#Annotate Pareto Efficient Curve and relevant points
# segments(8, 6, 6, 8, lty = 1, col = COL[2] , lwd = graphlinewidth)
# points(6, 8, pch = 16, col = "black", cex = 1.5)
# text(6, 8.5, expression(paste("g")))
# 
# points(8, 6, pch = 16, col = "black", cex = 1.5)
# text(7, 7.5, expression(paste("i")))
# 
# points(7, 7, pch = 16, col = "black", cex = 1.5)
# text(8, 6.5, expression(paste("f")))

#points(5.84, 8.77, pch = 16, col = "black", cex = 1.5)

#B's brf
text(14, 270, expression(paste("Consumer surplus")), cex = labelsize)
#text(14, 6, expression(paste("function")))
#Arrows(29.5, 6.75, 24, 6.75, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)

#A's brf
text(14, 50, expression(paste("Market profits")), cex = labelsize)
#text(6, 32.75, expression(paste("function")))
#Arrows(6, 32, 6, 26, col = "black", lty = 1, lwd = 2, arr.type = "triangle", arr.lwd = 0.5)



# par(new = TRUE)
# 
# #Use the same x and ylims as previously, but with locations switched
# xlims2 <- c(18, 0)
# ylims2 <- c(18, 0)

#Leave the ylab and xlab blank to ensure no axes titles
# plot(0, 0, xlim = xlims2, ylim = ylims2, type = "n",
#      xlab = expression(paste("")),
#      ylab = expression(paste("")),
#      xaxt = "n", 
#      yaxt = "n", 
#      cex.lab = 1.3, 
#      bty = "n",
#      xaxs="i", 
#      yaxs="i")
# 
# 
# lines(xx1, indiffAlow(xx1, uA = 46.08, alpha = 16, beta = 1/24), col = COL[2], lwd = graphlinewidth)


#Set up axes at sides 3 and 4 (top and right)
# axis(side = 3, at = ticksx, pos = 0, labels = xlabels)
# axis(side = 4, at = ticksy, pos = 0, labels = ylabels, las = 0)
# mtext("B's Apples, x", side=3, line = 2.5, cex = axislabelsize)
# text(-0.8, 7, expression(paste("B's Oranges, y")), xpd = TRUE, cex = axislabelsize, srt = 270) 
# 
# #Add arrows:
# arrows(-0.8, 10, -0.8, 14, xpd = TRUE, length=0.1,angle=40,lwd=3)
# arrows(6.2, -1.8, 9, -1.8, xpd = TRUE, length=0.1,angle=40,lwd=3)
# 

#Customize ticks and labels for the plot
#ticksy <- seq(from = 0, to = 15, by = 1)
#ylabels <- seq(from = 0, to = 15, by = 1)
#ticksx <- seq(from = 0, to = 10, by = 1)
#xlabels <- seq(from = 0, to = 10, by = 1)
#axis(1, at = ticksx, pos = 0, labels = xlabels)
#axis(2, at = ticksy, pos = 0, labels = ylabels, las = 0)

dev.off()
