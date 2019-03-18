# TO-DO:


# ----
library(readxl)
library(tidyverse)
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
ccDF <- 
  cost_cap %>% 
  filter(!CapGal %in% c(180, 260, 550, 500, 1200, 2000, 3000, 5000, 8000, 10000, 17500))

# ----
# graph
# ----
p <-  ggplot(data = ccDF, aes(x = CapGal, y = M)) +
  #geom_jitter(aes(x=CapGal, y=PC), color = COLB[4]) + # Purchased Cost
  #geom_jitter(aes(x=CapGal, y=MC), color = COLA[4]) + # MC
  geom_line(color = COLB[4]) + # Rise/Run
  #geom_point(color = COLB[2]) + # Rise/Run
  theme_classic() +
  xlim(0, 20000) +
  xlab("Capacity of steel piping, gallons") + 
  ylab("Cost per gallon  (1990 US Dollars)") + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14,face = "bold"))

#Save plot to PDF
ggsave(p, filename = "cost_cap.pdf", 
       path = "firmmarketsupply",
       width = 7, height = 7, units = "in")

# ----


