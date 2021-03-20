# ----
library(ggplot2)
library(readxl)
library(reshape2)
library(ggsci)

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

#PV <- PV %>% separate(col = variable, into = c("variable", "country"), sep = ",")
# ----
# graph
# ----
PV$variable = factor(PV$variable)

pv_plot <-  ggplot(data = PV, aes(x=Year, y=value, group=variable, color = variable)) +
  geom_line() +
  geom_point() +
  xlim(1978, NA) +
  ylim(NA, 50) +
  labs(color = "Price and cost", x = "Year", y = "PV cost and price (2015 USD)") +
  theme_bw() +
  theme(axis.text=element_text(size=17),
        axis.title=element_text(size=22),
        legend.position = c(0.82, 0.73),
        text = element_text(size=17)) +
  scale_color_d3(palette = "category20") 

# ----
#Save plot to PDF
# ----
ggsave(pv_plot, filename = "PV_Module.pdf", 
       path = "firmmarketsupply",
       width = 9, height = 7, units = "in")
# ----


