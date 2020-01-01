# ----
library(ggplot2)
library(readxl)
library(reshape2)

#Set parameters for graphics
axislabelsize <- 1.5
labelsize <- 1.2
graphlinewidth <- 2
segmentlinewidth <- 1.5

# ----
# pull in data
# ----
PV_mod <- read_excel("firmmarketsupply/PV_module _data.xlsx", 
                     range = "A1:L41")

PV <- reshape2::melt(as.data.frame(PV_mod), id="Year")
# ----
# graph
# ----
PV$variable = factor(PV$variable)

pv_plot <-  ggplot(data = PV, aes(x=Year, y=value, group=variable, color = variable)) +
  geom_line() +
  geom_point() +
  xlim(1978, NA) +
  ylim(NA, 50) +
  labs(color = "Price and Cost", x = "Year", y = "PV Cost and Price (2015 USD)") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
  
# ----
#Save plot to PDF
# ----
ggsave(pv_plot, filename = "PV_Module.pdf", 
       path = "firmmarketsupply",
       width = 7, height = 7, units = "in")
# ----


