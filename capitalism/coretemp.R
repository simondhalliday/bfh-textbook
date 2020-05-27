library(readr)
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(grid)
library(ineq)
library(forcats)
library(ggrepel)
library(xts)

Coretemp <- read_excel("capitalism/coretemp.xlsx")

CTplot <- 
  Coretemp %>%
  ggplot(aes(x = year, y = value, color = value)) +
  geom_hline(yintercept = 0, color = "grey") + 
  geom_line() + 
  ylab(expression(paste("Deviation from average \n temperature (1960-1990)"))) + 
  xlab("Year") + 
  scale_color_gradient(low="blue", high="red", name = "Deviation size") +
  theme_bw() +
  theme(
    axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
    axis.title.y = element_text(margin = unit(c(0, 6, 0, 0), "mm"), angle = 90, vjust = - 3)
  ) 
CTplot 

pdf(file = "capitalism/coretemp.pdf", width = 4.5, height = 3)
CTplot
dev.off()