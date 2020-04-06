require(shape)
library(tidyverse)
#pdf(file = "competitionmarkets/long_run_n_bargraph.pdf", width = 9, height = 7)


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

n <- as.factor(c(1:11))
u <- c(225,144,100,73.5,56.3,44.4,36,29.8,25,21.3,18.4)

df <- tibble(n,u)


plot <- 
  df %>% 
  ggplot(aes(x = n, y = u)) + 
  geom_bar(stat = "identity", position = "dodge", col = "#377eb8", fill = "#377eb8") + 
  xlab("Number of people, n") +
  ylab(expression(paste("Utility, ", u ))) + 
  scale_y_continuous(breaks = seq(0,240,20),
                     labels = seq(0,240,20),
                     limits = c(0,240)) +
  geom_hline(yintercept=20, linetype='dashed', col = 'black') +
  annotate("text", x = 9.5, y = 60, label = "fallback option = 20", cex = 7) + 
  
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
  ) + 
  geom_text(
    aes(x = n, y = u, label = u), 
    vjust = -1, size = 6,
    position = position_dodge(width = 1),
    inherit.aes = TRUE
  ) 

plot

ggsave(plot, file = "coordination_failures/privateproperty.pdf", width = 9, height = 7)

