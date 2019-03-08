# TO-DO:


# ----
library(readxl)
#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#e0f3db", "#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#c6dbef", "#4eb3d3", "#2b8cbe", "#0868ac","#084081")
COLC <- c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d")

# ----
# pull in data
# ----

cost_cap <- read_excel("firmmarketsupply/cost_cap.xlsx")

# ----
# graph
# ----
p <-  ggplot(data = cost_cap) +
  #geom_jitter(aes(x=CapGal, y=PC), color = COLB[4]) + # Purchased Cost
  #geom_jitter(aes(x=CapGal, y=MC), color = COLA[4]) + # MC
  geom_jitter(aes(x=CapGal, y=M), color = COLB[2]) + # Rise/Run
  theme_classic() +
  xlab("Capacity, gal") + 
  ylab("Purchased Cost, dollars") + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

#Save plot to PDF
ggsave(p, filename = "cost_cap.pdf", 
       path = "firmmarketsupply",
       width = 7, height = 7, units = "in")

# ----


