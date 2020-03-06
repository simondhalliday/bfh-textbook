require(shape)
pdf(file = "competitionmarkets/profit_consumersurplus_n.pdf", width = 9, height = 7)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")

par(mar =  c(6, 6, 4, 4))

deadweightLoss <- function(n, s = 0.5, pmax = 20, c1 = 2) {
  (1/2)*((c1 + ((1/(n+1))*(pmax -c1)  - c1)) )*((pmax - c1)/s -  (n/(n+1))*( (pmax - c1)/(s)))
}

dlProp <- function(n, s = 0.5, pmax = 20, c1 = 2) {
  ((1/2)*((c1 + ((1/(n+1))*(pmax -c1)  - c1)) )*((pmax - c1)/s -  (n/(n+1))*( (pmax - c1)/(s)))) / (0.5*(pmax - c1)*(n/(n+1))*(pmax - c1)/(s))
}



library(tidyverse)
xlims <- c(1, 17)

n <- seq(xlims[1], xlims[2], by = 1)
dlN <- dlProp(n)
df <- data.frame(n, dlN)

df %>%
  ggplot(aes(x = n, y = dlN)) + 
  geom_bar(stat = "identity", fill = "#feb24c") + 
  xlab("Number of firms, n") +
  ylab("Deadweight Loss") + 
  theme_bw() + 
  theme(text = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 19),
        panel.grid.minor = element_blank())

ggsave(file = "competitionmarkets/deadweight_loss_n.pdf", device = "pdf", width = 6, height = 4)

