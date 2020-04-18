require(shape)
library(tidyverse)
# pdf(file = "competitionmarkets/long_run_n_bargraph.pdf", width = 9, height = 7)


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
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

par(mar =  c(5, 8, 1, 1))

Profit <- function(n, pbar = 60, c = 10, beta = 0.5) {
  (pbar - c)^2 / (((n + 1)^2)*beta)
}

totaln <- function(pi, pbar = 60, c = 10, beta = 0.5){
  (pbar - c)/(sqrt(pi*beta)) - 1
}

cournotPrice <- function(n, pbar = 60, c1 = 10) {
  c1 + (1/(n + 1))*(pbar - c1)
}


# Barriers to entry
# Inputs: Probability of BTE (b) and number of firms
# Outputs: Price Level at some n
bte <- function(n, b = 0) {
  cournotPrice(n)*(1-b)
}

nstar <- function(b, pbar = 60, c = 10){
  (pbar*(1 - b) - c) / (b*c)
}

#Levels for the barriers to entry specified here
barriers <- c(0.2, 0.5, 0.71)
costs <- c(5, 10)

firms <- seq(1, 40, 1)
cost <- rep(6, length(firms))
nfirms <- tibble(firms, cost)
nfirms <- 
  nfirms %>% 
  mutate(price = cournotPrice(firms), 
         pbar = bte(firms, b = 0.85))

plot <- 
  nfirms %>% 
  ggplot(aes(x = firms, y = pbar, fill = pbar, col = pbar)) + 
  geom_bar(stat = "identity", position = "dodge", col = "#377eb8", fill = "#377eb8") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5.5)) +
  xlab("Number of firms, n") +
  ylab(expression(paste("Expected Price, ", hat(p)*(n) ))) + 
  xlim(0, 25) + 
  geom_line(y = 2, lty = 2, col = "black", lwd = 0.8) +
  annotate("text", x = 20, y = 2.2, label = "marginal cost, c = 2", cex = 8) + 
  
  # scale_fill_brewer(type = "qual", palette = "Set1", 
  #                   name = "",
  #                   breaks = c("pbar"),
  #                   labels = c("Expected Price")
  # ) + 
  # scale_color_brewer(type = "qual", palette = "Set1", 
  #                    name = "",
  #                    breaks = c("pbar"),
  #                    labels = c("Expected Price")
  # ) +
  theme_bw() + 
  theme(#legend.position="top",
        axis.title = element_text(size = 24),
        axis.title.y = element_text(size = 24),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18)
        )
ggsave(plot, file = "competitionmarkets/long_run_n_bargraph.pdf", width = 9, height = 7)

