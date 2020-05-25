library(tidyverse)
library(readxl)
library(ggsignif)

# Import
Unit_17_data_file_for_charts <- read_excel("capitalism/data/Unit-17-data-file-for-charts.xlsx", 
                                           sheet = "D - Fig 17 - Wages and product", 
                                           skip = 13)

# Clean, wide-to-long
wage_prod <- 
  Unit_17_data_file_for_charts[-c(1:12), -c(2:8, 10, 12:14)] %>%
  rename(productivity = `Combined manufacturing productivity series`, 
         real_wages = `For chart: Manufacturing real wages`,
         year = Variable) %>% 
  gather(measure, index, 2:3)

# colors 

COL <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
COLA <- c("#99d8c9","#66c2a4","#41ae76", "#238b45", "#005824")
COLB <- c("#4eb3d3", "#2b8cbe", "#0868ac","#084081")

# for every nth year
every_nth <- function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

# plot

wage_prod <- 
  wage_prod %>% 
  mutate(year = parse_number(year), 
         index = as.double(index))

wage_prod_plot <- 
  wage_prod %>%
  ggplot() +
  geom_line(aes(
    x = year,
    y = index,
    group = measure,
    color = measure
  ),
  size = 0.8) +
  ylim(0, 810) +
  scale_color_manual(values = c(COLA[4], COLB[4]),
                     labels = c("Productivity", "Real Wages")) +
  labs(x = "Year",
       y = "Index (1949 = 100)") +
  theme_bw() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 20),
    legend.title = element_blank(),
    legend.position = c(0.85, 0.1),
    axis.title.x = element_text(size = 18, vjust = -1),
    axis.title.y = element_text(size = 18, vjust = 1),
    axis.text.x = element_text(size = 17, angle = 90, color = "black"),
    axis.text.y = element_text(size = 17, color = "black"),  
    legend.background = element_rect(
      linetype = 1,
      size = 0.25,
      color = 1
    )
  ) 


#wage_prod_plot + 
  #annotate("rect", xmin=c(1900, 12.9), xmax=c(1950, 14), ymin=c(-100, -100) , ymax=c(850,850), alpha=0.2,fill="gray")
  #annotate("text", x = 1960, y = 600, label = "Golden Age")


bracketsGrob <- function(...){
  l <- list(...)
  e <- new.env()
  e$l <- l
  grid:::recordGrob(  {
    do.call(grid.brackets, l)
  }, e)
}


b1 <- bracketsGrob(0.01, 0.82, 0.445, 0.82, h=0.05, lwd=2, col="black")
b2 <- bracketsGrob(0.449, .82, 0.99, .82, h=0.05,  lwd=2, col="black")


wage_prod_plot <- 
  wage_prod_plot + 
  annotation_custom(b1)+ 
  annotation_custom(b2) +
  annotate("text", x = 1962, y = 807, label = expression(paste(bold("1949-73; 1973-79"))), size = 5) +
  annotate("text", x = 1999, y = 807, label = expression(paste(bold("1979-2008; 2008-14"))), size = 5) + 
  annotate("text", x = 1962, y = 782, label = "Golden age epoch", size = 5) +
  annotate("text", x = 1999, y = 782, label = "From stagflation to", size = 5) +
  annotate("text", x = 1999, y = 755, label = "financial crisis epoch", size = 5) 
wage_prod_plot 

ggsave(
  plot = wage_prod_plot,
  filename = "capitalism/wage_prod.pdf",
  device = "pdf",
  width = 9,
  height = 7
)


  