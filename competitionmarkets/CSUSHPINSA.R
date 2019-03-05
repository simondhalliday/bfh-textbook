# TO-DO:
# Scale x-axis

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

CSUSH <- read_excel("competitionmarkets/CSUSHPINSA.xls", 
                         range = "A11:B395")

# ----
# graph
# ----
p <-  ggplot(data = CSUSH ) +
  geom_line(aes(x=observation_date, y=CSUSHPINSA), color = COLB[4]) +
  #scale_x_date(breaks='6 months') +
  geom_vline(xintercept = as.numeric(CSUSH$observation_date[236]), linetype="dashed", 
               color = COLA[4], size=0.5) +
  annotate("text", label = "Peak of Housing Prices", x = CSUSH$observation_date[227], y = 100, 
           color = COL[1], angle = 90, size = 6) +
  geom_vline(xintercept = as.numeric(CSUSH$observation_date[261]), linetype="dashed", 
             color = COLA[4], size=0.5) +
  annotate("text", label = "Start of Banking Crisis", x = CSUSH$observation_date[269], y = 100, 
           color = COL[1], angle = 90, size = 6) +
  theme_classic() +
  xlab("Date") + 
  ylab("S&P/Case-Shiller U.S. National Home Price Index") + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

#Save plot to PDF
ggsave(p, filename = "CSUSHPINSA.pdf", 
       path = "competitionmarkets",
       width = 7, height = 7, units = "in")

# ----




