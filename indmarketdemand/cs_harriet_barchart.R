require(shape)
library(tidyverse)

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

par(mar =  c(5, 7, 1, 1))

mrsA <- function(x, rmax = 20, xmax = 10) {
  rmax - (rmax/xmax)*x
}

cs <- function(x, p = 10){
  mrsA(x) - p 
}

uA <- function(x, y, rmax = 20, xmax = 10) {
  y + rmax*x - (1/2)(rmax/xmax)*x^2
}

x <- seq(0, 10, 1)
df <- as.data.frame(x)
dfp1 <-
  df %>%
  mutate(mrsx = mrsA(x), 
         csx = cs(x),
         xcons = x + 1) %>%
  filter(xcons <= 6)
plot1 <-
  dfp1 %>%
  ggplot(aes(x = x, y = csx)) +
  geom_bar(stat = "identity", fill = COLA[3]) +
  scale_x_continuous(breaks = seq(0,5,1), 
                     labels = seq(0,5,1)
  ) + 
  scale_y_continuous(breaks = seq(0,10,2), 
                     labels = seq(0,10,2)
  ) +
  ylab("Consumer surplus") + 
  xlab("Quantity of x") +
  theme_bw() + 
  theme(legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 22)
        ) 
plot1
ggsave(plot1, file = "indmarketdemand/harriet_cs_barchart.pdf",  width = 9, height = 7)

dfp2 <-
  df %>%
  mutate(mrsx = mrsA(x), 
         csx = cs(x),
         xcons = x + 1) 
plot2 <-
  dfp2 %>%
  ggplot(aes(x = x, y = mrsx)) +
  geom_bar(stat = "identity", fill = COLA[4]) +
  scale_x_continuous(breaks = seq(0,10,1), 
                     labels = seq(0,10,1)
  ) + 
  scale_y_continuous(breaks = seq(0,20,2), 
                     labels = seq(0,20,2)
  ) +
  ylab("Willingness to pay ($), mrs(x)") + 
  xlab("Quantity of x") +
  geom_line(y = 10, lty = 1, col = COLB[4], lwd = 0.8) +
  annotate("text", x = 9, y = 10.8, label = "price, p = 10", cex = 8) + 
    theme_bw() + 
  theme(legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 22)
  ) 
plot2
ggsave(plot2, file = "indmarketdemand/harriet_wtp_barchart.pdf",  width = 9, height = 7)
